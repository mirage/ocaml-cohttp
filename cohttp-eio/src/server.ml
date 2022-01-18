open Eio.Std

type handler = Request.t -> Response.t
type middleware = handler -> handler

type t = {
  socket_backlog : int;
  domains : int;
  port : int;
  request_handler : handler;
  stopped : bool Atomic.t;
}

let stop t = ignore @@ Atomic.compare_and_set t.stopped false true

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)

let rec handle_request (t : t) (conn : Client_connection.t) : unit =
  match Reader.parse conn.reader Parser.request with
  | req -> (
      let req = Request.{ req; reader = conn.reader; read_complete = false } in
      let response = t.request_handler req in
      let keep_alive = Request.is_keep_alive req in
      response.headers <-
        Http.Header.add_unless_exists response.headers "connection"
          (if keep_alive then "keep-alive" else "close");
      Response.write conn response;
      match (keep_alive, Atomic.get t.stopped) with
      | _, true | false, _ -> Client_connection.close conn
      | true, false ->
          (* Drain unread bytes from client connection before
             reading another request. *)
          if not req.read_complete then
            match Http.Header.get_transfer_encoding (Request.headers req) with
            | Http.Transfer.Fixed _ -> ignore @@ Request.read_fixed req
            | Http.Transfer.Chunked -> ignore @@ Request.read_chunk req ignore
            | _ -> ()
          else ();
          (handle_request [@tailcall]) t conn)
  | exception End_of_file ->
      Printf.eprintf "\nClosing connection%!";
      Client_connection.close conn
  | exception Reader.Parse_error msg ->
      Printf.eprintf "\nRequest parsing error: %s%!" msg;
      Response.write conn Response.bad_request
  | exception exn ->
      Printf.eprintf "\nUnhandled exception: %s%!" (Printexc.to_string exn);
      Response.write conn Response.internal_server_error

let run_domain (t : t) env =
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run @@ fun sw ->
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:t.socket_backlog
    @@ `Tcp (Eio.Net.Ipaddr.V4.loopback, t.port)
  in
  while not (Atomic.get t.stopped) do
    Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error (fun ~sw flow addr ->
        let client_conn =
          {
            Client_connection.flow;
            addr;
            switch = sw;
            reader = Reader.create 1024 (flow :> Eio.Flow.source);
            response_buffer = Buffer.create 1024;
          }
        in
        handle_request t client_conn)
  done

let create ?(socket_backlog = 10_000) ?(domains = domain_count) ~port
    request_handler : t =
  {
    socket_backlog;
    domains;
    port;
    request_handler;
    stopped = Atomic.make false;
  }

(* wrk2 -t 24 -c 1000 -d 60s -R400000 http://localhost:8080 *)
let run (t : t) env =
  Eio.Std.traceln "\nServer listening on 127.0.0.1:%d" t.port;
  Eio.Std.traceln "\nStarting %d domains ...%!" t.domains;
  Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  for _ = 2 to t.domains do
    Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain t env))
  done;
  run_domain t env

(* Basic handlers *)

let not_found : handler = fun (_ : Request.t) -> Response.not_found

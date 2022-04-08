open Eio.Std

type handler = Request.t -> Response.t
type middleware = handler -> handler

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

let is_custom resp = match Response.body resp with
  | Custom _ -> true
  | _ -> false

let rec handle_request reader writer flow handler =
  match Request.parse reader with
  | request ->
      let response = handler request in
      Response.write response writer;
      (* A custom response needs to write the main response before calling
         the custom function for the body. Response.write wakes the writer for
         us if that is the case. *)
      if not (is_custom response) then Writer.wakeup writer;
      if Request.is_keep_alive request then
        handle_request reader writer flow handler
      else Eio.Flow.close flow
  | (exception End_of_file) | (exception Eio.Net.Connection_reset _) ->
      Eio.Flow.close flow
  | exception Parser.Parse_failure _e ->
      Response.(write bad_request writer);
      Writer.wakeup writer;
      Eio.Flow.close flow
  | exception _ ->
      Response.(write internal_server_error writer);
      Writer.wakeup writer;
      Eio.Flow.close flow

let run_domain ssock handler =
  let on_accept_error exn =
    Printf.fprintf stderr "Error while accepting connection: %s"
      (Printexc.to_string exn)
  in
  Switch.run (fun sw ->
      while true do
        Eio.Net.accept_sub ~sw ssock ~on_error:on_accept_error
          (fun ~sw flow _addr ->
            let reader = Reader.create 0x1000 (flow :> Eio.Flow.source) in
            let writer = Writer.create flow in
            Eio.Fiber.fork ~sw (fun () -> Writer.run writer);
            handle_request reader writer flow handler)
      done)

let run ?(socket_backlog = 128) ?(domains = domain_count) ~port env sw handler =
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:socket_backlog
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  for _ = 2 to domains do
    Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () -> run_domain ssock handler))
  done;
  run_domain ssock handler

(* Basic handlers *)

let not_found _ = Response.not_found

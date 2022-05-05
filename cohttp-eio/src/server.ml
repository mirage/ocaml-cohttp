open Eio.Std

type middleware = handler -> handler
and handler = request -> response
and request = Http.Request.t * Reader.t
and response = Http.Response.t * Body.t

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

(* Request *)

let read_fixed ((request, reader) : Http.Request.t * Reader.t) =
  match request.meth with
  | `POST | `PUT | `PATCH -> Body.read_fixed reader request.headers
  | _ ->
      let err =
        Printf.sprintf
          "Request with HTTP method '%s' doesn't support request body"
          (Http.Method.to_string request.meth)
      in
      raise @@ Invalid_argument err

let read_chunked : request -> (Body.chunk -> unit) -> Http.Header.t =
 fun (request, reader) f -> Body.read_chunked reader request.headers f

(* Responses *)

let is_custom body = match body with Body.Custom _ -> true | _ -> false

let text_response body =
  let headers =
    Http.Header.of_list
      [
        ("content-type", "text/plain; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  let response =
    Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ()
  in
  (response, Body.Fixed body)

let html_response body =
  let headers =
    Http.Header.of_list
      [
        ("content-type", "text/html; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  let response =
    Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ()
  in
  (response, Body.Fixed body)

let not_found_response = (Http.Response.make ~status:`Not_found (), Body.Empty)

let internal_server_error_response =
  (Http.Response.make ~status:`Internal_server_error (), Body.Empty)

let bad_request_response =
  (Http.Response.make ~status:`Bad_request (), Body.Empty)

let write_response (writer : Writer.t)
    ((response, body) : Http.Response.t * Body.t) =
  let version = Http.Version.to_string response.version in
  let status = Http.Status.to_string response.status in
  Writer.write_string writer version;
  Writer.write_string writer " ";
  Writer.write_string writer status;
  Writer.write_string writer "\r\n";
  Body.write_headers writer response.headers;
  Writer.write_string writer "\r\n";
  match body with
  | Fixed s -> Writer.write_string writer s
  | Chunked chunk_writer -> Body.write_chunked writer chunk_writer
  | Custom f ->
      Writer.wakeup writer;
      f (writer.sink :> Eio.Flow.sink)
  | Empty -> ()

(* main *)

let rec handle_request reader writer flow handler =
  match Reader.http_request reader with
  | request ->
      let response, body = handler (request, reader) in
      write_response writer (response, body);
      (* A custom response needs to write the main response before calling
         the custom function for the body. Response.write wakes the writer for
         us if that is the case. *)
      if not (is_custom body) then Writer.wakeup writer;
      if Http.Request.is_keep_alive request then
        handle_request reader writer flow handler
      else Eio.Flow.close flow
  | (exception End_of_file) | (exception Eio.Net.Connection_reset _) ->
      Eio.Flow.close flow
  | exception Reader.Parse_failure _e ->
      write_response writer bad_request_response;
      Writer.wakeup writer;
      Eio.Flow.close flow
  | exception _ ->
      write_response writer internal_server_error_response;
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
            let writer = Writer.create (flow :> Eio.Flow.sink) in
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

let not_found_handler _ = not_found_response

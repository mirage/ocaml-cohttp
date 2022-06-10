open Eio.Std
module Write = Eio.Buf_write

type middleware = handler -> handler
and handler = request -> response
and request = Http.Request.t * Eio.Buf_read.t * Eio.Net.Sockaddr.stream
and response = Http.Response.t * Body.t

let domain_count =
  match Sys.getenv_opt "COHTTP_DOMAINS" with
  | Some d -> int_of_string d
  | None -> 1

(* Request *)

let read_fixed request reader =
  match Http.Request.meth request with
  | `POST | `PUT | `PATCH -> Body.read_fixed reader request.headers
  | _ -> None

let read_chunked : request -> (Body.chunk -> unit) -> Http.Header.t option =
 fun (request, reader, _) f -> Body.read_chunked reader request.headers f

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

let write_response (writer : Write.t)
    ((response, body) : Http.Response.t * Body.t) =
  let version = Http.Version.to_string response.version in
  let status = Http.Status.to_string response.status in
  Write.string writer version;
  Write.string writer " ";
  Write.string writer status;
  Write.string writer "\r\n";
  Body.write_headers writer response.headers;
  Write.string writer "\r\n";
  match body with
  | Fixed s -> Write.string writer s
  | Chunked chunk_writer -> Body.write_chunked writer chunk_writer
  | Custom f -> f writer
  | Empty -> ()

(* main *)

let rec handle_request client_addr reader writer flow handler =
  match Reader.http_request reader with
  Writer.write_string writer version;
  Writer.write_char writer ' ';
  Writer.write_string writer status;
  Writer.write_string writer "\r\n";
  Writer.write_headers writer response.headers;
  Writer.write_string writer "\r\n";
  Writer.write_body writer body

(* request parsers *)

open Eio.Buf_read.Syntax
module Buf_read = Eio.Buf_read

let meth =
  let+ meth = Parser.(token <* space) in
  Http.Method.of_string meth

let resource = Parser.(take_while1 (fun c -> c != ' ') <* space)

let[@warning "-3"] http_request t =
  match Buf_read.at_end_of_input t with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let meth = meth t in
      let resource = resource t in
      let version = Parser.(version <* crlf) t in
      let headers = Parser.http_headers t in
      let encoding = Http.Header.get_transfer_encoding headers in
      { Http.Request.meth; resource; version; headers; scheme = None; encoding }

(* main *)

let rec handle_request reader writer flow handler =
  match Reader.http_request reader with
  | request ->
      let response, body = handler (request, reader, client_addr) in
      write_response writer (response, body);
      if Http.Request.is_keep_alive request then
        handle_request client_addr reader writer flow handler
  | (exception End_of_file) | (exception Eio.Net.Connection_reset _) -> ()
  | exception (Failure _ as ex) ->
      write_response writer bad_request_response;
      raise ex
  | exception ex ->
      write_response writer internal_server_error_response;
      raise ex

let connection_handler (handler : handler) flow client_addr =
  let reader =
    Eio.Buf_read.of_flow ~initial_size:0x1000 ~max_size:max_int flow
  in
  Write.with_flow flow (fun writer ->
      handle_request client_addr reader writer flow handler)
      Writer.wakeup writer;
      raise ex

let run_domain ssock handler =
  let on_error exn =
    Printf.fprintf stderr "Error handling connection: %s\n%!"
      (Printexc.to_string exn)
  in
  let handler = connection_handler handler in
  Switch.run (fun sw ->
      let rec loop () =
        Eio.Net.accept_fork ~sw ssock ~on_error handler;
        loop ()
      in
      loop ())

let run ?(socket_backlog = 128) ?(domains = domain_count) ~port env handler =
  Switch.run @@ fun sw ->
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

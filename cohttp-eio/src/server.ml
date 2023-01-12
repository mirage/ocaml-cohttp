module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write
module Switch = Eio.Switch

type middleware = handler -> handler
and handler = request -> response
and request = Http.Request.t * Eio.Buf_read.t * Eio.Net.Sockaddr.stream
and response = Http.Response.t * Body.t

type 'a env =
  < domain_mgr : Eio.Domain_manager.t
  ; net : Eio.Net.t
  ; clock : Eio.Time.clock
  ; .. >
  as
  'a

(* Request *)

let read_fixed request reader =
  match Http.Request.meth request with
  | `POST | `PUT | `PATCH ->
      let ( let* ) o f = Option.bind o f in
      let ( let+ ) o f = Option.map f o in
      let* v = Http.Header.get request.headers "Content-Length" in
      let+ content_length = int_of_string_opt v in
      Buf_read.take content_length reader
  | _ -> None

let read_chunked request reader f =
  Body.read_chunked reader (Http.Request.headers request) f

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

let http_date clock =
  let now = Eio.Time.now clock |> Ptime.of_float_s |> Option.get in
  let (year, mm, dd), ((hh, min, ss), _) = Ptime.to_date_time now in
  let weekday = Ptime.weekday now in
  let weekday =
    match weekday with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  let month =
    match mm with
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _ -> failwith "Invalid HTTP datetime value"
  in
  Format.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday dd month year hh
    min ss

let write_response ?request clock writer (response, body) =
  let headers =
    let request_meth = Option.map Http.Request.meth request in
    Body.add_content_length
      (Http.Response.requires_content_length ?request_meth response)
      (Http.Response.headers response)
      body
  in
  let headers =
    (* https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1 *)
    match Http.Response.status response with
    | #Http.Status.informational | #Http.Status.server_error -> headers
    | _ -> Http.Header.add headers "Date" (http_date clock)
  in
  let version = Http.Version.to_string response.version in
  let status = Http.Status.to_string response.status in
  Buf_write.string writer version;
  Buf_write.char writer ' ';
  Buf_write.string writer status;
  Buf_write.string writer "\r\n";
  Rwer.write_headers writer headers;
  Buf_write.string writer "\r\n";
  let write_chunked_trailers =
    Option.map Http.Request.supports_chunked_trailers request
  in
  Body.write_body ?write_chunked_trailers writer body

(* request parsers *)

let meth =
  let open Eio.Buf_read.Syntax in
  let+ meth = Rwer.(token <* space) in
  Http.Method.of_string meth

let resource =
  let open Eio.Buf_read.Syntax in
  Rwer.(take_while1 (fun c -> c != ' ') <* space)

let[@warning "-3"] http_request t =
  let open Eio.Buf_read.Syntax in
  let meth = meth t in
  let resource = resource t in
  let version = Rwer.(version <* crlf) t in
  let headers = Rwer.http_headers t in
  let encoding = Http.Header.get_transfer_encoding headers in
  { Http.Request.meth; resource; version; headers; scheme = None; encoding }

(* main *)

let rec handle_request clock client_addr reader writer flow handler =
  match http_request reader with
  | request ->
      let response, body = handler (request, reader, client_addr) in
      write_response ~request clock writer (response, body);
      if Http.Request.is_keep_alive request then
        handle_request clock client_addr reader writer flow handler
  | (exception End_of_file)
  | (exception Eio.Io (Eio.Net.E (Connection_reset _), _)) ->
      ()
  | exception (Failure _ as ex) ->
      write_response clock writer bad_request_response;
      raise ex
  | exception ex ->
      write_response clock writer internal_server_error_response;
      raise ex

let connection_handler (handler : handler) clock flow client_addr =
  let reader = Buf_read.of_flow ~initial_size:0x1000 ~max_size:max_int flow in
  Buf_write.with_flow flow (fun writer ->
      handle_request clock client_addr reader writer flow handler)

let run_domain env ssock handler =
  let on_error exn =
    Printf.fprintf stderr "Error handling connection: %s\n%!"
      (Printexc.to_string exn)
  in
  let handler = connection_handler handler env#clock in
  Switch.run (fun sw ->
      let rec loop () =
        Eio.Net.accept_fork ~sw ssock ~on_error handler;
        loop ()
      in
      loop ())

let run ?(socket_backlog = 128) ?(domains = 1) ~port env handler =
  Switch.run @@ fun sw ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let ssock =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~reuse_addr:true ~reuse_port:true
      ~backlog:socket_backlog
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  for _ = 2 to domains do
    Eio.Std.Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () ->
            run_domain env ssock handler))
  done;
  run_domain env ssock handler

(* Basic handlers *)

let not_found_handler _ = not_found_response

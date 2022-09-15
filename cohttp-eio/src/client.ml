module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write

type response = Http.Response.t * Buf_read.t
type host = string * int option
type resource_path = string

type 'a body_disallowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  conn:(#Eio.Flow.two_way as 'a) ->
  host ->
  resource_path ->
  response
(** [body_disallowed_call] denotes HTTP client calls where a request is not
    allowed to have a request body. *)

type 'a body_allowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  conn:(#Eio.Flow.two_way as 'a) ->
  host ->
  resource_path ->
  response

(* Request line https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1 *)
let write_request request writer body =
  let headers =
    Body.add_content_length
      (Http.Request.requires_content_length request)
      (Http.Request.headers request)
      body
  in
  let headers = Http.Header.clean_dup headers in
  let headers = Http.Header.Private.move_to_front headers "Host" in
  let meth = Http.Method.to_string @@ Http.Request.meth request in
  let version = Http.Version.to_string @@ Http.Request.version request in
  Buf_write.string writer meth;
  Buf_write.char writer ' ';
  Buf_write.string writer @@ Http.Request.resource request;
  Buf_write.char writer ' ';
  Buf_write.string writer version;
  Buf_write.string writer "\r\n";
  Rwer.write_headers writer headers;
  Buf_write.string writer "\r\n";
  Body.write_body ~write_chunked_trailers:true writer body

(* response parser *)

let is_digit = function '0' .. '9' -> true | _ -> false

let status_code =
  let open Rwer in
  let open Buf_read.Syntax in
  let+ status = take_while1 is_digit in
  Http.Status.of_int (int_of_string status)

let reason_phrase =
  Buf_read.take_while (function
    | '\x21' .. '\x7E' | '\t' | ' ' -> true
    | _ -> false)

(* https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.2 *)
let response buf_read =
  let open Buf_read.Syntax in
  let version = Rwer.(version <* space) buf_read in
  let status = Rwer.(status_code <* space) buf_read in
  let () = Rwer.(reason_phrase *> crlf *> Buf_read.return ()) buf_read in
  let headers = Rwer.http_headers buf_read in
  Http.Response.make ~version ~status ~headers ()

(* Generic HTTP call *)

let call ?meth ?version ?(headers = Http.Header.init ()) ?(body = Body.Empty)
    ~conn host resource_path =
  let headers =
    if not (Http.Header.mem headers "Host") then
      let host =
        match host with
        | host, Some port -> host ^ ":" ^ string_of_int port
        | host, None -> host
      in
      Http.Header.add headers "Host" host
    else headers
  in
  let headers =
    Http.Header.add_unless_exists headers "User-Agent" "cohttp-eio"
  in
  let initial_size = 0x1000 in
  Buf_write.with_flow ~initial_size conn (fun writer ->
      let request = Http.Request.make ?meth ?version ~headers resource_path in
      let request = Http.Request.add_te_trailers request in
      write_request request writer body;
      let reader = Eio.Buf_read.of_flow ~initial_size ~max_size:max_int conn in
      let response = response reader in
      (response, reader))

(*  HTTP Calls with Body Disallowed *)

let get ?version ?headers ~conn host resource_path =
  call ~meth:`GET ?version ?headers ~conn host resource_path

let head ?version ?headers ~conn host resource_path =
  call ~meth:`HEAD ?version ?headers ~conn host resource_path

let delete ?version ?headers ~conn host resource_path =
  call ~meth:`DELETE ?version ?headers ~conn host resource_path

(*  HTTP Calls with Body Allowed *)

let post ?version ?headers ?body ~conn host resource_path =
  call ~meth:`POST ?version ?headers ?body ~conn host resource_path

let put ?version ?headers ?body ~conn host resource_path =
  call ~meth:`PUT ?version ?headers ?body ~conn host resource_path

let patch ?version ?headers ?body ~conn host resource_path =
  call ~meth:`PATCH ?version ?headers ?body ~conn host resource_path

(* Response Body *)

let read_fixed ((response, reader) : Http.Response.t * Buf_read.t) =
  match Http.Response.content_length response with
  | Some content_length -> Buf_read.take content_length reader
  | None -> Buf_read.take_all reader

let read_chunked : response -> (Body.chunk -> unit) -> Http.Header.t option =
 fun (response, reader) f -> Body.read_chunked reader response.headers f

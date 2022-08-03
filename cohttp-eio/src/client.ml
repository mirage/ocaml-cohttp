module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write

type response = Http.Response.t * Buf_read.t
type host = string * int option
type resource_path = string
type 'a conn = unit -> (host * Eio.Flow.two_way as 'a)

type 'a body_disallowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  'a conn ->
  resource_path ->
  response
(** [body_disallowed_call] denotes HTTP client calls where a request is not
    allowed to have a request body. *)

type 'a body_allowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  'a conn ->
  resource_path ->
  response

(* Request line https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1 *)
let write_request writer (meth, version, headers, resource_path, body) =
  Buf_write.string writer (Http.Method.to_string meth);
  Buf_write.char writer ' ';
  Buf_write.string writer resource_path;
  Buf_write.char writer ' ';
  Buf_write.string writer (Http.Version.to_string version);
  Buf_write.string writer "\r\n";
  Rwer.write_headers writer headers;
  Buf_write.string writer "\r\n";
  Body.write_body writer body

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
  match Buf_read.at_end_of_input buf_read with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let version = Rwer.(version <* space) buf_read in
      let status = Rwer.(status_code <* space) buf_read in
      let () = Rwer.(reason_phrase *> crlf *> return ()) buf_read in
      let headers = Rwer.http_headers buf_read in
      Http.Response.make ~version ~status ~headers ()

(* Generic HTTP call *)

let call ?(meth = `GET) ?(version = `HTTP_1_1) ?(headers = Http.Header.init ())
    ?(body = Body.Empty) flow_fn resource_path =
  let (host_name, host_port), flow = flow_fn () in
  let host =
    match host_port with
    | Some port -> host_name ^ ":" ^ string_of_int port
    | None -> host_name
  in
  Buf_write.with_flow ~initial_size:0x1000 flow (fun writer ->
      let headers = Http.Header.add_unless_exists headers "Host" host in
      write_request writer (meth, version, headers, resource_path, body);
      let reader =
        Eio.Buf_read.of_flow ~initial_size:0x1000 ~max_size:max_int flow
      in
      let response = response reader in
      (response, reader))

(*  HTTP Calls with Body Disallowed *)

let get ?version ?headers stream uri =
  call ~meth:`GET ?version ?headers stream uri

let head ?version ?headers stream uri =
  call ~meth:`HEAD ?version ?headers stream uri

let delete ?version ?headers stream uri =
  call ~meth:`DELETE ?version ?headers stream uri

(*  HTTP Calls with Body Allowed *)

let post ?version ?headers ?body stream uri =
  call ~meth:`POST ?version ?headers ?body stream uri

let put ?version ?headers ?body stream uri =
  call ~meth:`PUT ?version ?headers ?body stream uri

let patch ?version ?headers ?body stream uri =
  call ~meth:`PATCH ?version ?headers ?body stream uri

(* Response Body *)

let read_fixed ((response, reader) : Http.Response.t * Buf_read.t) =
  Body.read_fixed reader response.headers

let read_chunked : response -> (Body.chunk -> unit) -> Http.Header.t option =
 fun (response, reader) f -> Body.read_chunked reader response.headers f

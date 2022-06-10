module Buf_read = Eio.Buf_read

type response = Http.Response.t * Buf_read.t
type env = < net : Eio.Net.t >

type 'a body_disallowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  (< env ; .. > as 'a) ->
  Eio.Switch.t ->
  Eio.Net.Sockaddr.stream ->
  Uri.t ->
  response
(** [body_disallowed_call] denotes HTTP client calls where a request is not
    allowed to have a request body. *)

type 'a body_allowed_call =
  ?version:Http.Version.t ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  (< env ; .. > as 'a) ->
  Eio.Switch.t ->
  Eio.Net.Sockaddr.stream ->
  Uri.t ->
  response

(* Request line https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1 *)
let write_request writer (meth, version, headers, uri, body) =
  Writer.write_string writer (Http.Method.to_string meth);
  Writer.write_char writer ' ';
  Writer.write_string writer (Uri.path_and_query uri);
  Writer.write_char writer ' ';
  Writer.write_string writer (Http.Version.to_string version);
  Writer.write_string writer "\r\n";
  Writer.write_headers writer headers;
  Writer.write_string writer "\r\n";
  Writer.write_body writer body

(* response parser *)

let is_digit = function '0' .. '9' -> true | _ -> false

open Buf_read.Syntax

let status_code =
  let open Parser in
  let+ status = take_while1 is_digit in
  Http.Status.of_int (int_of_string status)

let reason_phrase =
  Buf_read.take_while (function
    | '\x21' .. '\x7E' | '\t' | ' ' -> true
    | _ -> false)

(* https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.2 *)
let response buf_read =
  match Buf_read.at_end_of_input buf_read with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let version = Parser.(version <* space) buf_read in
      let status = Parser.(status_code <* space) buf_read in
      let () = Parser.(reason_phrase *> crlf *> return ()) buf_read in
      let headers = Parser.http_headers buf_read in
      Http.Response.make ~version ~status ~headers ()

(* Generic HTTP call *)

let call ?(meth = `GET) ?(version = `HTTP_1_1) ?(headers = Http.Header.init ())
    ?(body = Body.Empty) env sw stream uri =
  let open Eio in
  let flow = Net.connect ~sw (Stdenv.net env) stream in
  let writer = Writer.create (flow :> Flow.sink) in
  Fiber.fork ~sw (fun () -> Writer.run writer);
  write_request writer (meth, version, headers, uri, body);
  Writer.wakeup writer;
  let reader =
    Eio.Buf_read.of_flow ~initial_size:0x1000 ~max_size:max_int
      (flow :> Eio.Flow.source)
  in
  let response = response reader in
  (response, reader)

(*  HTTP Calls with Body Disallowed *)

let get ?version ?headers env sw stream uri =
  call ~meth:`GET ?version ?headers env sw stream uri

let head ?version ?headers env sw stream uri =
  call ~meth:`HEAD ?version ?headers env sw stream uri

let delete ?version ?headers env sw stream uri =
  call ~meth:`DELETE ?version ?headers env sw stream uri

(*  HTTP Calls with Body Allowed *)

let post ?version ?headers ?body env sw stream uri =
  call ~meth:`POST ?version ?headers ?body env sw stream uri

let put ?version ?headers ?body env sw stream uri =
  call ~meth:`PUT ?version ?headers ?body env sw stream uri

let patch ?version ?headers ?body env sw stream uri =
  call ~meth:`PATCH ?version ?headers ?body env sw stream uri

(* Response Body *)

let read_fixed ((response, reader) : Http.Response.t * Buf_read.t) =
  Body.read_fixed reader response.headers

let read_chunked : response -> (Body.chunk -> unit) -> Http.Header.t option =
 fun (response, reader) f -> Body.read_chunked reader response.headers f

class virtual t =
  object
    method virtual version : Http.Version.t
    method virtual headers : Http.Header.t
    method virtual status : Http.Status.t
  end

let version (t : #t) = t#version
let headers (t : #t) = t#headers
let status (t : #t) = t#status

exception Closed

class client_response version headers status buf_read =
  let closed = ref false in
  object
    inherit t
    inherit Body.reader
    method version = version
    method headers = headers
    method status = status
    method buf_read = if !closed then raise Closed else buf_read
    method body_closed = !closed
    method close_body = closed := true
  end

(* https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.2 *)

open Buf_read.Syntax

let is_digit = function '0' .. '9' -> true | _ -> false

let status_code =
  let+ status = Buf_read.take_while1 is_digit in
  Http.Status.of_int (int_of_string status)

let reason_phrase =
  Buf_read.take_while (function
    | '\x21' .. '\x7E' | '\t' | ' ' -> true
    | _ -> false)

let parse buf_read =
  let open Eio.Buf_read.Syntax in
  let version = Buf_read.(version <* space) buf_read in
  let status = Buf_read.(status_code <* space) buf_read in
  Buf_read.(reason_phrase *> crlf *> return ()) buf_read;
  let headers = Buf_read.http_headers buf_read in
  (version, headers, status)

let close_body (t : #client_response) = t#close_body
let body_closed (t : #client_response) = t#body_closed

class virtual server_response =
  object
    inherit t
    inherit Body.writer
  end

let server_response ?(version = `HTTP_1_1) ?(headers = Http.Header.init ())
    ?(status = `OK) (body : Body.writer) : server_response =
  object
    method version = version
    method headers = headers
    method status = status
    method write_body = body#write_body
    method write_header = body#write_header
  end

let chunked_response ~ua_supports_trailer write_chunk write_trailer =
  let w = Chunked_body.writer ~ua_supports_trailer write_chunk write_trailer in
  server_response w

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

let write_header w ~name ~value = Buf_write.write_header w name value

let write (t : #server_response) (clock : #Eio.Time.clock) w =
  let version = Http.Version.to_string t#version in
  let status = Http.Status.to_string t#status in
  Buf_write.string w version;
  Buf_write.char w ' ';
  Buf_write.string w status;
  Buf_write.string w "\r\n";
  (* https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1 *)
  (match t#status with
  | #Http.Status.informational | #Http.Status.server_error -> ()
  | _ -> Buf_write.write_header w "Date" (http_date clock));
  t#write_header (write_header w);
  Buf_write.write_headers w t#headers;
  Buf_write.string w "\r\n";
  t#write_body w

let text content =
  server_response
    (Body.content_writer ~content ~content_type:"text/plain; charset=UTF-8")

let html content =
  server_response
    (Body.content_writer ~content ~content_type:"text/html; charset=UTF-8")

let none_body_response status =
  let headers = Http.Header.init_with "Content-Length" "0" in
  server_response ~headers ~status (Body.none :> Body.writer)

let not_found = none_body_response `Not_found
let internal_server_error = none_body_response `Internal_server_error
let bad_request = none_body_response `Bad_request

let field lbl v =
  let open Easy_format in
  let lbl = Atom (lbl ^ ": ", atom) in
  let v = Atom (v, atom) in
  Label ((lbl, label), v)

let pp fmt (t : #t) =
  let open Easy_format in
  let fields =
    [
      field "Version" (Http.Version.to_string t#version);
      field "Status" (Http.Status.to_string t#status);
      Label
        ( (Atom ("Headers :", atom), { label with label_break = `Always }),
          Header.easy_fmt t#headers );
    ]
  in
  let list_p =
    {
      list with
      align_closing = true;
      indent_body = 2;
      wrap_body = `Force_breaks_rec;
    }
  in
  Pretty.to_formatter fmt (List (("{", ";", "}", list_p), fields))

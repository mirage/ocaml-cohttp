module P = struct
  type 'a t = Reader.t -> 'a

  exception Parse_failure of string

  let return v _ = v
  let fail err _ = Stdlib.raise_notrace (Parse_failure err)
  let commit rdr = Reader.commit rdr
  let ( <?> ) p err rdr = try p rdr with Parse_failure _e -> fail err rdr

  let ( >>= ) p f rdr =
    let a = p rdr in
    f a rdr

  let ( let* ) = ( >>= )

  let ( >>| ) p f rdr =
    let v = p rdr in
    f v

  let ( let+ ) = ( >>| )

  let ( <* ) p q rdr =
    let a = p rdr in
    let _ = q rdr in
    a

  let ( *> ) p q rdr =
    let _ = p rdr in
    q rdr

  let ( <|> ) p q rdr =
    let old_pos = Reader.pos rdr in
    let old_committed = Reader.committed_bytes rdr in
    try p rdr
    with Parse_failure _ as ex ->
      if old_committed < Reader.committed_bytes rdr then raise_notrace ex
      else (
        rdr.pos <- old_pos;
        q rdr)

  let lift f p = p >>| f

  let lift2 f p q rdr =
    let a = p rdr in
    let b = q rdr in
    f a b

  let rec ensure rdr len =
    if Reader.(length rdr < pos rdr + len) then (
      ignore (Reader.fill rdr len);
      ensure rdr len)

  let pos rdr = Reader.pos rdr

  let end_of_input rdr =
    try
      ensure rdr 1;
      false
    with End_of_file -> true

  let option : 'a -> 'a t -> 'a t = fun x p -> p <|> return x

  let peek_char rdr =
    let open Reader in
    if pos rdr < length rdr then unsafe_get rdr (pos rdr)
    else (
      ensure rdr 1;
      unsafe_get rdr rdr.pos)

  let peek_string n rdr =
    try
      ensure rdr n;
      Reader.substring rdr ~off:rdr.pos ~len:n
    with End_of_file -> fail "[peek_string] not enough input" rdr

  let sprintf = Printf.sprintf

  let char c rdr =
    let c' = peek_char rdr in
    if c = c' then Reader.incr_pos rdr
    else fail (sprintf "[char] expected %C, got %C" c c') rdr

  let any_char rdr =
    ensure rdr 1;
    let c = Reader.unsafe_get rdr rdr.pos in
    Reader.incr_pos rdr;
    c

  let satisfy f rdr =
    let c = peek_char rdr in
    if f c then (
      Reader.incr_pos rdr;
      c)
    else fail "[satisfy]" rdr

  let string s rdr =
    let len = String.length s in
    ensure rdr len;
    let pos = pos rdr in
    let i = ref 0 in
    while
      !i < len
      && Char.equal (Reader.unsafe_get rdr (pos + !i)) (String.unsafe_get s !i)
    do
      incr i
    done;
    if len = !i then Reader.incr_pos ~n:len rdr else fail "[string]" rdr

  let count_while rdr f =
    let i = ref 0 in
    let continue = ref true in
    while !continue do
      try
        ensure rdr (!i + 1);
        let c = Reader.(unsafe_get rdr (pos rdr + !i)) in
        if f c then incr i else continue := false
      with End_of_file -> continue := false
    done;
    !i

  let take_while1 f rdr =
    let count = count_while rdr f in
    if count < 1 then fail "[take_while1] count is less than 1" rdr
    else
      let s = Reader.(substring rdr ~off:(pos rdr) ~len:count) in
      Reader.incr_pos ~n:count rdr;
      s

  let take_while f rdr =
    let count = count_while rdr f in
    if count > 0 then (
      let s = Reader.(substring rdr ~off:(pos rdr) ~len:count) in
      Reader.incr_pos ~n:count rdr;
      s)
    else ""

  let take_bigstring : int -> Bigstringaf.t t =
   fun n rdr ->
    try
      ensure rdr n;
      let s = Reader.(copy rdr ~off:(pos rdr) ~len:n) in
      Reader.incr_pos ~n rdr;
      s
    with End_of_file -> fail "[take_bigstring] not enough input" rdr

  let take : int -> string t =
   fun n rdr ->
    try
      ensure rdr n;
      let s = Reader.(substring rdr ~off:(pos rdr) ~len:n) in
      Reader.incr_pos ~n rdr;
      s
    with End_of_file -> fail "[take] not enough input" rdr

  let take_till f = take_while (fun c -> not (f c))

  let rec many : 'a t -> 'a list t =
   fun p rdr ->
    try
      let a = p rdr in
      a :: many p rdr
    with Parse_failure _ | End_of_file -> []

  let rec many_till : 'a t -> _ t -> 'a list t =
   fun p t rdr ->
    try
      let _ = t rdr in
      let a = p rdr in
      a :: many_till p t rdr
    with Parse_failure _ -> []

  let skip f rdr =
    ensure rdr 1;
    let c = Reader.(unsafe_get rdr (pos rdr)) in
    if f c then Reader.incr_pos rdr else fail "[skip]" rdr

  let skip_while f rdr =
    let count = count_while rdr f in
    Reader.incr_pos ~n:count rdr

  let rec skip_many p rdr =
    match p rdr with _ -> skip_many p rdr | exception Parse_failure _ -> ()
end

open Angstrom

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let space = char '\x20'
let htab = char '\t'
let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let optional x = option None (x >>| Option.some)
let is_vchar = function '\x21' .. '\x7E' -> true | _ -> false
let vchar = satisfy (function '\x21' .. '\x7E' -> true | _ -> false)
let digit = satisfy (function '0' .. '9' -> true | _ -> false)
let is_cr = function '\r' -> true | _ -> false
let crlf = string "\r\n" <?> "[crlf]"

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-3.2 --*)
let header =
  lift2
    (fun key value -> (key, value))
    (token <* char ':' <* ows)
    (take_till is_cr <* crlf)

let headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
      let _emp = return [] in
      let _rec = lift2 cons header headers in
      peek_char_fail >>= function '\r' -> _emp | _ -> _rec)
  >>| Http.Header.of_list

let version =
  let* v = string "HTTP/1." *> digit <* crlf in
  match v with
  | '1' -> return `HTTP_1_1
  | '0' -> return `HTTP_1_0
  | _ -> fail (Format.sprintf "Invalid HTTP version: %c" v)

(*-- request-line = method SP request-target SP HTTP-version CRLF HTTP headers *)
let[@warning "-3"] request =
  let request =
    let* meth = token >>| Http.Method.of_string <* space in
    let* resource = take_while1 (fun c -> c != ' ') <* space in
    let* version = version <* commit in
    let+ headers = headers <* crlf <* commit in
    {
      Http.Request.headers;
      meth;
      scheme = None;
      resource;
      version;
      encoding = Http.Header.get_transfer_encoding headers;
    }
  in
  let eof = end_of_input >>| fun () -> raise End_of_file in
  eof <|> request

(* Chunked encoding parser *)

let hex_digit = function
  | '0' .. '9' -> true
  | 'a' .. 'f' -> true
  | 'A' .. 'F' -> true
  | _ -> false

let quoted_pair = char '\\' *> (space <|> htab <|> vchar)

(*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
let qdtext =
  htab
  <|> space
  <|> char '\x21'
  <|> satisfy (function
        | '\x23' .. '\x5B' -> true
        | '\x5D' .. '\x7E' -> true
        | _ -> false)

(*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
let quoted_string =
  let dquote = char '"' in
  let+ chars = dquote *> many_till (qdtext <|> quoted_pair) dquote <* dquote in
  String.of_seq @@ List.to_seq chars

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 --*)
let chunk_exts =
  let chunk_ext_name = token in
  let chunk_ext_val = quoted_string <|> token in
  many
    (lift2
       (fun name value : Chunk.extension -> { name; value })
       (char ';' *> chunk_ext_name)
       (optional (char '=' *> chunk_ext_val)))

let chunk_size =
  let* sz = take_while1 hex_digit in
  try return (Format.sprintf "0x%s" sz |> int_of_string)
  with _ -> fail (Format.sprintf "Invalid chunk_size: %s" sz)

(* Be strict about headers allowed in trailer headers to minimize security
   issues, eg. request smuggling attack -
   https://portswigger.net/web-security/request-smuggling
   Allowed headers are defined in 2nd paragraph of
   https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.2 *)
let is_trailer_header_allowed h =
  match String.lowercase_ascii h with
  | "transfer-encoding" | "content-length" | "host"
  (* Request control headers are not allowed. *)
  | "cache-control" | "expect" | "max-forwards" | "pragma" | "range" | "te"
  (* Authentication headers are not allowed. *)
  | "www-authenticate" | "authorization" | "proxy-authenticate"
  | "proxy-authorization"
  (* Cookie headers are not allowed. *)
  | "cookie" | "set-cookie"
  (* Response control data headers are not allowed. *)
  | "age" | "expires" | "date" | "location" | "retry-after" | "vary" | "warning"
  (* Headers to process the payload are not allowed. *)
  | "content-encoding" | "content-type" | "content-range" | "trailer" ->
      false
  | _ -> true

(* Request indiates which headers will be sent in chunk trailer part by
   specifying the headers in comma separated value in 'Trailer' header. *)
let request_trailer_headers (req : Http.Request.t) =
  match Http.Header.get req.headers "Trailer" with
  | Some v -> List.map String.trim @@ String.split_on_char ',' v
  | None -> []

(* Chunk decoding algorithm is explained at
   https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3 *)
let chunk (total_read : int) (req : Http.Request.t) =
  let* sz = chunk_size in
  match sz with
  | sz when sz > 0 ->
      let* extensions = chunk_exts <* crlf in
      let* data = take_bigstring sz <* crlf >>| Cstruct.of_bigarray in
      return @@ `Chunk (sz, data, extensions)
  | 0 ->
      let* extensions = chunk_exts <* crlf in
      (* Read trailer headers if any and append those to request headers.

         Only headers names appearing in 'Trailer' request headers and "allowed" trailer
         headers are appended to request.

         The spec at https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3
         specifies that 'Content-Length' and 'Transfer-Encoding' headers must be
         updated. *)
      let* trailer_headers = headers <* commit in
      let request_trailer_headers = request_trailer_headers req in
      let request_headers = Http.Request.headers req in
      let trailer_headers =
        List.filter
          (fun (name, _) ->
            List.mem name request_trailer_headers
            && is_trailer_header_allowed name)
          (Http.Header.to_list trailer_headers)
      in
      let request_headers =
        List.fold_left
          (fun h (key, v) -> Http.Header.add h key v)
          request_headers trailer_headers
      in
      (* Remove either just the 'chunked' from Transfer-Encoding header value or
         remove the header entirely if value is empty. *)
      let te_header = "Transfer-Encoding" in
      let request_headers =
        match Http.Header.get request_headers te_header with
        | Some header_value ->
            let new_header_value =
              String.split_on_char ',' header_value
              |> List.map String.trim
              |> List.filter (fun v ->
                     let v = String.lowercase_ascii v in
                     not (String.equal v "chunked"))
              |> String.concat ","
            in
            if String.length new_header_value > 0 then
              Http.Header.replace request_headers te_header new_header_value
            else Http.Header.remove request_headers te_header
        | None -> assert false
      in
      (* Remove 'Trailer' from request headers. *)
      let request_headers = Http.Header.remove request_headers "Trailer" in
      (* Add Content-Length header *)
      let request_headers =
        Http.Header.add request_headers "Content-Length"
          (string_of_int total_read)
      in
      let updated_request = { req with headers = request_headers } in
      return @@ `Last_chunk (extensions, updated_request)
  | sz -> fail (Format.sprintf "Invalid chunk size: %d" sz)

let fixed_body content_length =
  if content_length > 0 then
    take_bigstring content_length >>| fun body -> Cstruct.buffer body
  else return Cstruct.empty

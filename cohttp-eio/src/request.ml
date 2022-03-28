type t = {
  reader : Reader.t;
  headers : Http.Header.t;
  version : Http.Version.t;
  meth : Http.Method.t;
  resource : string;
}

let reader t = t.reader
let headers t = t.headers
let meth t = t.meth
let resource t = t.resource
let version t = t.version

let is_keep_alive t =
  match Http.Header.get t.headers "connection" with
  | Some "close" -> false
  | Some "keep-alive" -> true
  | _ -> Http.Version.(compare t.version `HTTP_1_1) >= 0

module P = Parser

let token =
  P.(
    take_while1 (function
      | '0' .. '9'
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
      | '`' | '|' | '~' ->
          true
      | _ -> false))

let ows = P.(skip_while (function ' ' | '\t' -> true | _ -> false))
let crlf = P.(string "\r\n")
let is_cr = function '\r' -> true | _ -> false
let space = P.(char '\x20')
let p_meth = P.(token <* space >>| Http.Method.of_string)
let p_resource = P.(take_while1 (fun c -> c != ' ') <* space)

let p_version =
  P.(
    string "HTTP/1." *> any_char <* crlf >>= function
    | '1' -> return `HTTP_1_1
    | '0' -> return `HTTP_1_0
    | v -> fail (Format.sprintf "Invalid HTTP version: %C" v))

let header =
  P.(
    lift2
      (fun key value -> (key, value))
      (token <* char ':' <* ows)
      (take_till is_cr <* crlf))

let p_headers =
  let cons x xs = x :: xs in
  P.(
    fix (fun headers ->
        let _emp = return [] in
        let _rec = lift2 cons header headers in
        peek_char >>= function '\r' -> _emp | _ -> _rec)
    >>| Http.Header.of_list
    <* crlf)

let parse reader =
  let open P in
  match end_of_input reader with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let meth = p_meth reader in
      let resource = p_resource reader in
      let version = p_version reader in
      let headers = p_headers reader in
      commit reader;
      { reader; meth; resource; version; headers }

let read_fixed =
  let read_complete = ref false in
  fun t ->
    if !read_complete then Error "End of file"
    else
      match Http.Header.get t.headers "content-length" with
      | Some v -> (
          try
            let content_length = int_of_string v in
            let content = Parser.take content_length t.reader in
            read_complete := true;
            Ok content
          with e -> Error (Printexc.to_string e))
      | None -> Error "Request is not a fixed content body"

(* Chunked encoding parser *)

let hex_digit = function
  | '0' .. '9' -> true
  | 'a' .. 'f' -> true
  | 'A' .. 'F' -> true
  | _ -> false

let quoted_pair =
  P.(
    char '\\'
    *> satisfy (function ' ' | '\t' | '\x21' .. '\x7E' -> true | _ -> false))

(*-- qdtext = HTAB / SP /%x21 / %x23-5B / %x5D-7E / obs-text -- *)
let qdtext =
  P.(
    satisfy (function
      | '\t' | ' ' | '\x21' | '\x23' .. '\x5B' -> true
      | '\x5D' .. '\x7E' -> true
      | _ -> false))

(*-- quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE --*)
let quoted_string =
  let open P in
  let dquote = char '"' in
  let+ chars = dquote *> many_till (qdtext <|> quoted_pair) dquote <* dquote in
  String.of_seq @@ List.to_seq chars

let optional x = P.(option None (x >>| Option.some))

(*-- https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 --*)
let chunk_exts =
  let open P in
  let chunk_ext_name = token in
  let chunk_ext_val = quoted_string <|> token in
  many
    (lift2
       (fun name value : Chunk.extension -> { name; value })
       (char ';' *> chunk_ext_name)
       (optional (char '=' *> chunk_ext_val)))

let chunk_size =
  let open P in
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
let request_trailer_headers t =
  match Http.Header.get t.headers "Trailer" with
  | Some v -> List.map String.trim @@ String.split_on_char ',' v
  | None -> []

(* Chunk decoding algorithm is explained at
   https://datatracker.ietf.org/doc/html/rfc7230#section-4.1.3 *)
let chunk (total_read : int) (t : t) =
  let open P in
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
      let* trailer_headers = p_headers <* commit in
      let request_trailer_headers = request_trailer_headers t in
      let request_headers = headers t in
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
      let updated_request = { t with headers = request_headers } in
      return @@ `Last_chunk (extensions, updated_request)
  | sz -> fail (Format.sprintf "Invalid chunk size: %d" sz)

let read_chunk t =
  match Http.Header.get_transfer_encoding t.headers with
  | Http.Transfer.Chunked ->
      let total_read = ref 0 in
      let read_complete = ref false in
      let rec chunk_loop f =
        if !read_complete then Error "End of file"
        else
          let chunk = chunk !total_read t t.reader in
          match chunk with
          | `Chunk (size, data, extensions) ->
              f (Chunk.Chunk { size; data; extensions });
              total_read := !total_read + size;
              (chunk_loop [@tailcall]) f
          | `Last_chunk (extensions, updated_request) ->
              read_complete := true;
              f (Chunk.Last_chunk extensions);
              Ok updated_request
      in
      chunk_loop
  | _ -> fun _ -> Error "Request is not a chunked request"

let pp_method fmt meth = Format.fprintf fmt "%s" (Http.Method.to_string meth)
let pp_version fmt v = Format.fprintf fmt "%s" (Http.Version.to_string v)

let pp fmt t =
  let fields =
    [
      Fmt.field "meth" (fun t -> t.meth) pp_method;
      Fmt.field "resource" (fun t -> t.resource) Fmt.string;
      Fmt.field "version" (fun t -> t.version) pp_version;
      Fmt.field "headers" (fun t -> t.headers) Http.Header.pp_hum;
    ]
  in
  Fmt.record fields fmt t

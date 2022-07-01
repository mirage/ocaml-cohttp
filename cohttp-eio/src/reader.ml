open Eio.Buf_read
open Eio.Buf_read.Syntax

let return v _ = v

let take_while1 p r =
  match take_while p r with
  | "" -> failwith "[take_while1] count is less than 1"
  | x -> x

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = string "\r\n"
let not_cr = function '\r' -> false | _ -> true
let space = char '\x20'

let p_meth =
  let+ meth = token <* space in
  Http.Method.of_string meth

let p_resource = take_while1 (fun c -> c != ' ') <* space

let p_version =
  let* v = string "HTTP/1." *> any_char <* crlf in
  match v with
  | '1' -> return `HTTP_1_1
  | '0' -> return `HTTP_1_0
  | v -> failwith (Format.sprintf "Invalid HTTP version: %C" v)

let header =
  let+ key = token <* char ':' <* ows and+ value = take_while not_cr <* crlf in
  (key, value)

let http_headers r =
  let rec aux () =
    match peek_char r with
    | Some '\r' ->
        crlf r;
        []
    | _ ->
        let h = header r in
        h :: aux ()
  in
  Http.Header.of_list (aux ())

let[@warning "-3"] http_request t =
  match at_end_of_input t with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let meth = p_meth t in
      let resource = p_resource t in
      let version = p_version t in
      let headers = http_headers t in
      Http.Request.make ~meth ~version ~headers resource

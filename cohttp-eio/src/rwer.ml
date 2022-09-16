(* This modules encapsulates refactored - common - readers and writers
   used by the Client and Server modules.

    rwer.ml => (R)eader (W)riter + er
*)

module Buf_read = Eio.Buf_read
module Buf_write = Eio.Buf_write

let take_while1 p r =
  match Buf_read.take_while p r with "" -> raise End_of_file | x -> x

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = Buf_read.skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = Buf_read.string "\r\n"
let not_cr = function '\r' -> false | _ -> true
let space = Buf_read.char '\x20'

let version =
  let open Eio.Buf_read.Syntax in
  let* v = Buf_read.string "HTTP/1." *> Buf_read.any_char in
  match v with
  | '1' -> Buf_read.return `HTTP_1_1
  | '0' -> Buf_read.return `HTTP_1_0
  | v -> failwith (Format.sprintf "Invalid HTTP version: %C" v)

let header =
  let open Eio.Buf_read.Syntax in
  let+ key = token <* Buf_read.char ':' <* ows
  and+ value = Buf_read.take_while not_cr <* crlf in
  (key, value)

let http_headers r =
  let[@tail_mod_cons] rec aux () =
    match Buf_read.peek_char r with
    | Some '\r' ->
        crlf r;
        []
    | _ ->
        let h = header r in
        h :: aux ()
  in
  Http.Header.of_list (aux ())

let write_headers writer headers =
  let headers = Http.Header.clean_dup headers in
  Http.Header.iter
    (fun k v ->
      Buf_write.string writer k;
      Buf_write.string writer ": ";
      Buf_write.string writer v;
      Buf_write.string writer "\r\n")
    headers

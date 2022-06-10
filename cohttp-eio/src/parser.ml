(* Encapsulate refactored/common parser between Client and Server module. *)

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

let version =
  let* v = string "HTTP/1." *> any_char in
  match v with
  | '1' -> return `HTTP_1_1
  | '0' -> return `HTTP_1_0
  | v -> failwith (Format.sprintf "Invalid HTTP version: %C" v)

let header =
  let+ key = token <* char ':' <* ows and+ value = take_while not_cr <* crlf in
  (key, value)

let http_headers r =
  let[@tail_mod_cons] rec aux () =
    match peek_char r with
    | Some '\r' ->
        crlf r;
        []
    | _ ->
        let h = header r in
        h :: aux ()
  in
  Http.Header.of_list (aux ())

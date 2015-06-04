(*
  Copyright (C) 2012, David Sheets <sheets@alum.mit.edu>

  Permission to use, copy, modify, and/or distribute this software for
  any purpose with or without fee is hereby granted, provided that the
  above copyright notice and this permission notice appear in all
  copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
*)
(* TODO: handle exceptions better *)
(* TODO: sorting? *)

open Printf
include Accept_types
module Parser = Accept_parser
module Lexer = Accept_lexer

let parse_using p s = p Lexer.header_value (Lexing.from_string s)
let media_ranges = function
  | Some s -> parse_using Parser.media_ranges s
  | None -> [1000,(AnyMedia, [])]
let charsets = function
  | Some s -> parse_using Parser.charsets s
  | None -> [1000,AnyCharset]
let encodings = function
  | Some s -> parse_using Parser.encodings s
  | None -> [1000,AnyEncoding]
let languages = function
  | Some s -> parse_using Parser.languages s
  | None -> [1000,AnyLanguage]

let rec string_of_pl = function
  | [] -> ""
  | (k,T v)::r -> sprintf ";%s=%s%s" k v (string_of_pl r)
  | (k,S v)::r -> sprintf ";%s=\"%s\"%s" k (Stringext.quote v) (string_of_pl r)

let accept_el el pl q =
  sprintf "%s;q=%.3f%s" el ((float q)/.1000.) (string_of_pl pl)

let string_of_media_range = function
  | (MediaType (t,st),pl) -> accept_el (sprintf "%s/%s" t st) pl
  | (AnyMediaSubtype (t),pl) -> accept_el (sprintf "%s/*" t) pl
  | (AnyMedia,pl) -> accept_el "*/*" pl

let string_of_charset = function
  | Charset c -> accept_el c []
  | AnyCharset -> accept_el "*" []

let string_of_encoding = function
  | Encoding e -> accept_el e []
  | Gzip -> accept_el "gzip" []
  | Compress -> accept_el "compress" []
  | Deflate -> accept_el "deflate" []
  | Identity -> accept_el "identity" []
  | AnyEncoding -> accept_el "*" []

let string_of_language = function
  | Language langl -> accept_el (String.concat "-" langl) []
  | AnyLanguage -> accept_el "*" []

let string_of_list s_of_el =
  let rec aux s = function
    | (q,el)::[] -> s^(s_of_el el q)
    | [] -> s
    | (q,el)::r -> aux (s^(s_of_el el q)^",") r
  in aux ""

let string_of_media_ranges = string_of_list string_of_media_range
let string_of_charsets = string_of_list string_of_charset
let string_of_encodings = string_of_list string_of_encoding
let string_of_languages = string_of_list string_of_language

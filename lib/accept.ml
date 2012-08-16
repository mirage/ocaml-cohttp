open Printf
include Accept_types
module Parser = Accept_parser
module Lexer = Accept_lexer
module Str = Re_str

let parse_using p s = p Lexer.header_value (Lexing.from_string s)
let media_ranges = parse_using Parser.media_ranges
let charsets = parse_using Parser.charsets
let encodings = parse_using Parser.encodings
let languages = parse_using Parser.languages

let rec string_of_pl = function
  | [] -> ""
  | (k,T v)::r -> sprintf ";%s=%s%s" k v (string_of_pl r)
  | (k,S v)::r -> sprintf ";%s=\"%s\"%s" k (Str.quote v) (string_of_pl r)

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

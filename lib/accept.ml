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

let quote = Str.global_replace (Str.regexp "\"") "\\\""
let rec string_of_pl = function
  | [] -> ""
  | (k,T v)::r -> sprintf ";%s=%s%s" k v (string_of_pl r)
  | (k,S v)::r -> sprintf ";%s=\"%s\"%s" k (quote v) (string_of_pl r)

let string_of_media_range = function
  | (q,MediaType (t,st),pl) ->
      sprintf "%s/%s;q=%.3f%s" t st ((float q)/.1000.) (string_of_pl pl)
  | (q,AnyMediaSubtype (t),pl) ->
      sprintf "%s/*;q=%.3f%s" t ((float q)/.1000.) (string_of_pl pl)
  | (q,AnyMedia,pl) -> sprintf "*/*;q=%.3f%s" ((float q)/.1000.) (string_of_pl pl)

let string_of_media_ranges =
  let rec aux s = function
    | mr::[] -> s^(string_of_media_range mr)
    | [] -> s
    | mr::r -> aux (s^(string_of_media_range mr)^",") r
  in aux ""

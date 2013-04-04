type pv = Accept_types.pv = T of string | S of string
type p = string * pv
type media_range =
  Accept_types.media_range =
    MediaType of string * string
  | AnyMediaSubtype of string
  | AnyMedia
type charset = Accept_types.charset = Charset of string | AnyCharset
type encoding =
  Accept_types.encoding =
    Encoding of string
  | Gzip
  | Compress
  | Deflate
  | Identity
  | AnyEncoding
type language = Accept_types.language = Language of string list | AnyLanguage
type q = int
type 'a qlist = (q * 'a) list

val media_ranges :
  string option -> (media_range * p list) qlist
val charsets : string option -> charset qlist
val encodings : string option -> encoding qlist
val languages : string option -> language qlist

val string_of_media_range : media_range * (string * pv) list -> q -> string
val string_of_charset : charset -> q -> string
val string_of_encoding : encoding -> q -> string
val string_of_language : language -> q -> string

val string_of_media_ranges : (media_range * p list) qlist -> string
val string_of_charsets : charset qlist -> string
val string_of_encodings : encoding qlist -> string
val string_of_languages : language qlist -> string

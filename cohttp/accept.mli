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

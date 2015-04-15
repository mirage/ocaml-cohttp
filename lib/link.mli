(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** RFC 5988 ("Web Linking") and RFC 5987 ("Character Set and Language
    Encoding for Hypertext Transfer Protocol (HTTP) Header Field Parameters") *)

module Rel : sig
  type t =
    | Extension of Uri.t
    | Alternate
    | Appendix
    | Bookmark
    | Chapter
    | Contents
    | Copyright
    | Current
    | Described_by
    | Edit
    | Edit_media
    | Enclosure
    | First
    | Glossary
    | Help
    | Hub
    | Index
    | Last
    | Latest_version
    | License
    | Next
    | Next_archive
    | Payment
    | Predecessor_version
    | Prev
    | Prev_archive
    | Related
    | Replies
    | Section
    | Self
    | Service
    | Start
    | Stylesheet
    | Subsection
    | Successor_version
    | Up
    | Version_history
    | Via
    | Working_copy
    | Working_copy_of
  with sexp
end

module Language : sig
  type t = private string with sexp

  val to_string : t -> string
  val of_string : string -> t
end

module Charset : sig
  type t = private string with sexp

  val to_string : t -> string
  val of_string : string -> t
end

module Ext : sig
  type 'a t with sexp

  val charset : 'a t -> Charset.t
  val language : 'a t -> Language.t
  val value : 'a t -> 'a

  val make : ?charset:Charset.t -> ?language:Language.t -> 'a -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Arc : sig
  type t = {
    reverse : bool;
    relation : Rel.t list;
    hreflang : string option;
    media : string option;
    title : string option;
    title_ext : string Ext.t option;
    media_type : (string * string) option;
    extensions : (string * string) list;
    extension_exts : (string * string Ext.t) list;
  }

  val empty : t
end

type t = {
  context : Uri.t;
  arc : Arc.t;
  target : Uri.t;
} with sexp

val empty : t

val of_string : string -> t list

val to_string : t -> string

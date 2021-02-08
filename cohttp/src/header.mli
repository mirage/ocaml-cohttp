(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

(** Associative list of HTTP headers pair of key and value. Order is preserved,
    meaning duplicated keys are neither removed or concataned by default (see
    [clean_dup] to do it). *)
type t [@@deriving sexp]
(** The type for HTTP headers. *)

val init : unit -> t
(** [init ()] constructs a fresh, empty map of HTTP headers. *)

val is_empty : t -> bool
(** [is_empty h] tests whether HTTP headers are empty or not. *)

val of_list : (string * string) list -> t
(** [of_list l] creates an header structure with same content and order than l,
    meaning the invariant [to_list (of_list l) = l] is true. *)

val to_list : t -> (string * string) list
(** [to_list h] convert HTTP headers h to a list. Order is preserved. *)

val init_with : string -> string -> t
(** [init_with k v] construct a fresh map of HTTP headers with a single pair of
    key and value [(k, v)]. *)

val add : t -> string -> string -> t
(** [add h k v] adds a key and value to an existing header list. *)

val add_list : t -> (string * string) list -> t
(** [add_list h l] adds each key and value pairs in [l] to the header list [h]
    in order, meaning [to_list (add_list h l) = to_list h @ l] *)

val add_multi : t -> string -> string list -> t
(** [add_multi h k vs] add multiple values to a key in an existing header map by
    calling [add h k v] (without concatenate the values).

    Invariant : [get_multi (add_multi h k vs) k = existing @ vs] if
    [get_multi h k = existing] *)

val add_opt : t option -> string -> string -> t
(** [add_opt hopt k v] adds the pair [(k, v)] to [h] if [hopt] is [Some h], or
    constructs a fresh header list with this pair if [hopt] is [None]. *)

val add_unless_exists : t -> string -> string -> t
(** [add_unless_exists h k v] adds [(k, v)] to [h] unless the key is already
    present in the header.

    Invariant : [add_unless_exists h k _ = h if mem h k = true] *)

val add_opt_unless_exists : t option -> string -> string -> t
(** [add_opt_unless_exists h k v] adds [(k, v)] to [h] if [hopt] is [Some h]
    unless the key is already present in the header. If [h] is [None] then a
    fresh header is allocated containing the pair [(k,
   v)]. *)

val remove : t -> string -> t
(** [remove h k] removes every pair with [k] as key from [h] and return a fresh
    header set. *)

val replace : t -> string -> string -> t
(** [replace h k v] replaces the last added value of [k] from [h] and removed
    all other occurences of [k] if it exists. Otherwise it adds [(k, v)] to [h]. *)

val update : t -> string -> (string option -> string option) -> t
(** [update h k f] returns a associative list containing the same headers as
    [h], except for the header [k]. Depending on the value of [v] where [v] is
    [f (get_multi_concat h k)], the header [k] is added, removed or updated.

    - If [v] is [None], every occurences of the header in [h] and all its value
      is removed;

    - If [v] is [Some z] then [k] is associated to [z] (and only [z]) in the
      resulting headers;

    - If [k] was already associated in [h] to a value that is physically equal
      to [z], [h] is returned unchanged.

    In case [k] should not have multiple values, but has multiple occurences in
    [h], the use of [clean_dup] may be needed before calling this function to
    prevent the values of this header to get concatenated. *)

val update_last : t -> string -> (string option -> string option) -> t
(** [update h k f] does the same work than [update h k f] except only the last
    value [v] associated to [k] is used and affected, meaning [f] is called with
    [get h k] and only the pair [(k, v)] is potentially removed or updated
    depending of the result of [f (get h
   k)]. *)

val mem : t -> string -> bool
(** [mem h k] returns [true] if the header name [k] appears in [h] and [false]
    otherwise. *)

val compare : t -> t -> int
(** [compare h h'] is the structural comparison of two [Header] values. *)

val get : t -> string -> string option
(** [get h k] returns [Some v] where [v] is the last added value associated with
    [k] in [h] if it exists and [None] otherwise *)

val get_multi : t -> string -> string list
(** [get_multi h k] returns a list of all values associated with [k] in the
    header list [h]. *)

val iter : (string -> string -> unit) -> t -> unit
val map : (string -> string -> string) -> t -> t
val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

val to_lines : t -> string list
(** [to_lines h] returns header fieds as a list of lines. Beware that each line
    ends with "\r\n" characters. *)

val to_frames : t -> string list
(** [to_frames h] returns the same as {!to_lines} but lines do not end with
    "\r\n" characters. *)

val to_string : t -> string

(* Header management functions *)

val clean_dup : t -> t
(** [clean_dup h] cleans duplicates in h : if the duplicated headers can not
    have multiple values, only the last value is kept. Otherwise, the values are
    concatenated and place at the first position this header is encountered in
    [h]. *)

val get_multi_concat : ?list_value_only:bool -> t -> string -> string option
(** [get_multi_concat h k] returns all the values paired with [k] in [h],
    concatenated and separated by a comma. The optional argument
    [?list_value_only] is [false] by default. If it is [true], then the returned
    string can contain multiple values only if the searched header can have
    multiple values (like transfer-encoding or accept). *)

val get_content_range : t -> Int64.t option
val get_media_type : t -> string option
val get_connection_close : t -> bool

val get_acceptable_media_ranges :
  t -> (Accept.media_range * Accept.p list) Accept.qlist

val get_acceptable_charsets : t -> Accept.charset Accept.qlist
val get_acceptable_encodings : t -> Accept.encoding Accept.qlist
val get_acceptable_languages : t -> Accept.language Accept.qlist
val get_transfer_encoding : t -> Transfer.encoding
val add_transfer_encoding : t -> Transfer.encoding -> t
val add_authorization : t -> Auth.credential -> t
val get_authorization : t -> Auth.credential option
val add_authorization_req : t -> Auth.challenge -> t
val is_form : t -> bool
val get_location : t -> Uri.t option
val add_links : t -> Link.t list -> t
val get_links : t -> Link.t list

val user_agent : string
(** The User-Agent header used by this library, including the version of cohttp. *)

val prepend_user_agent : t -> string -> t
(** Prepend [user_agent] to the product token already declared in the
    "User-Agent" field (if any). *)

val connection : t -> [ `Keep_alive | `Close | `Unknown of string ] option

val pp_hum : Format.formatter -> t -> unit
(** Human-readable output, used by the toplevel printer *)

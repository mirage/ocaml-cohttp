(** [Header]

    An extendable and type-safe HTTP Header. *)

(** {1 Name Value} *)

type name = private string
(** [name] represents HTTP header name value in a canonical format, i.e. the
    first letter and any letter following a hypen([-]) symbol are converted to
    upper case. For example, the canonical header name of [accept-encoding] is
    [Accept-Encoding]. *)

type lname = private string
(** [lname] represents HTTP header name in lowercase form, e.g.
    [Content-Type -> content-type], [Date -> date],
    [Transfer-Encoding -> transfer-encoding] etc. See {!val:lname}. *)

type value = string
(** [value] is an untyped HTTP header value, eg 10, text/html, chunked etc *)

val canonical_name : string -> name
(** [canonical_name s] converts [s] to a canonical header name value. See
    {!type:name}. *)

val lname : string -> lname
(** [lname s] converts [s] to {!type:lname} *)

(** {1 A Typed Header} *)

type 'a header = ..
(** [header] represents a type-safe HTTP header where an individual variant
    represents a specific HTTP header abstraction. For request or response
    specific headers please see {!val:Request.header} or {!val:Response.header}
    respectively. The headers defined here are common across both HTTP requests
    and responses.

    The common HTTP headers defined are as follows:

    - {!val:Content_length} represents [Content-Length]
    - {!val:Transfer_encoding} represents [Transfer-Encoding].
    - {!val:H} represents a generic and untyped header. The [cohttp-eio] request
      parser uses this value if a typed header for a HTTP header is not defined
      or found.

    Users should extend this type to define custom headers along with a custom
    {!class:Codec.t} instance. Example,

    {[
      type 'a Header.header +=
        | Header1 : string Header.header
        | Header2 : int Header.header
    ]}

    See {!class:Codec.t}. *)

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | H : lname -> value header  (** A generic header. *)

(** {1 Header Encoder & Decoder} *)

type 'a decoder = value -> 'a
(** [decoder] converts {!type:value} to type ['a]. To denote an error while
    decoding, an OCaml exception value is raised. *)

type 'a encoder = 'a -> value
(** [encoder] converts a typed value ['a] to its string representation. *)

type 'a undecoded
(** ['a undecoded] represents a lazy value that is as yet undecoded. See
    {!val:decode}. *)

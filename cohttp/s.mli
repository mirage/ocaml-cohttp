module type Http_io = sig
  type t
  module IO : IO.S

  val read : IO.ic -> [ `Eof | `Invalid of string | `Ok of t ] IO.t
  val has_body : t -> [ `No | `Unknown | `Yes ]
  val read_body_chunk : t -> IO.ic -> Transfer.chunk IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t
end

module type Request = sig
  type t = {
    mutable headers: Header.t;    (** HTTP request headers *)
    mutable meth: Code.meth;      (** HTTP request method *)
    mutable uri: Uri.t;           (** Full HTTP request uri *)
    mutable version: Code.version; (** HTTP version, usually 1.1 *)
    mutable encoding: Transfer.encoding; (** transfer encoding of this HTTP request *)
  } with fields, sexp

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t ->
    Uri.t -> t
  (** Return true whether the connection should be reused *)
  val is_keep_alive : t -> bool

  val make_for_client:
    ?headers:Header.t ->
    ?chunked:bool ->
    ?body_length:int ->
    Code.meth -> Uri.t -> t
end

module type Response = sig
  type t = {
    mutable encoding: Transfer.encoding; (** Transfer encoding of this HTTP response *)
    mutable headers: Header.t;    (** response HTTP headers *)
    mutable version: Code.version; (** (** HTTP version, usually 1.1 *) *)
    mutable status: Code.status_code; (** HTTP status code of the response *)
    mutable flush: bool;
  } with fields, sexp

  val make :
    ?version:Code.version -> 
    ?status:Code.status_code ->
    ?flush:bool ->
    ?encoding:Transfer.encoding -> 
    ?headers:Header.t -> 
    unit -> t
end

open Core.Std
open Async.Std

type t = [
  | Cohttp.Body.t
  | `Pipe of string Pipe.Reader.t
] with sexp_of

include Cohttp.S.Body with type t := t
val drain : t -> unit Deferred.t
val is_empty : t -> bool Deferred.t
val to_string : t -> string Deferred.t
val to_string_list : t -> string list Deferred.t
val to_pipe : t -> string Pipe.Reader.t
val of_pipe : string Pipe.Reader.t -> t
val map : t -> f:(string -> string) -> t
val as_pipe : t -> f:(string Pipe.Reader.t -> string Pipe.Reader.t) -> t


(** Internal functions *)

val write : ('a -> 'b -> unit Deferred.t) ->
  [< `Empty
  | `Pipe of 'b Pipe.Reader.t
  | `String of 'b
  | `Strings of 'b Deferred.List.t ] ->
  'a -> unit Deferred.t

val disable_chunked_encoding : t -> (t * int64) Deferred.t

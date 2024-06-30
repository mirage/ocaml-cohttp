open! Base
open! Async_kernel

type t = [ Cohttp.Body.t | `Pipe of string Pipe.Reader.t ] [@@deriving sexp_of]

include Cohttp.S.Body with type t := t

val to_string : t -> string Deferred.t
val to_string_list : t -> string list Deferred.t
val to_pipe : t -> string Pipe.Reader.t
val of_pipe : string Pipe.Reader.t -> t
val map : t -> f:(string -> string) -> t
val as_pipe : t -> f:(string Pipe.Reader.t -> string Pipe.Reader.t) -> t
val to_form : t -> (string * string list) list Deferred.t
val is_empty : t -> [ `True | `False | `Unknown ]

module Private : sig
  val write_body :
    ('a -> string -> unit Deferred.t) -> t -> 'a -> unit Deferred.t

  val pipe_of_body :
    ('a -> Cohttp.Transfer.chunk Deferred.t) -> 'a -> string Pipe.Reader.t

  val disable_chunked_encoding : t -> (t * int64) Deferred.t
  val drain : t -> unit Deferred.t
end

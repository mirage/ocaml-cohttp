module Body : sig
  module Encoding : sig
    type t

    val fixed : int64 -> t
    val chunked : t
  end

  type t

  val string : ?encoding:Encoding.t -> string -> t

  module Substring : sig
    type t = { base : string; pos : int; len : int }
  end

  val stream : ?encoding:Encoding.t -> (unit -> Substring.t option Lwt.t) -> t
end

module Context : sig
  type t

  val request : t -> Http.Request.t
  val read_body : t -> string Lwt.t
  val discard_body : t -> unit Lwt.t
  val respond : t -> Http.Response.t -> Body.t -> unit Lwt.t
end

type t

val create : ?on_exn:(exn -> unit) -> (Context.t -> unit Lwt.t) -> t

val handle_connection :
  t -> Lwt_io.input_channel * Lwt_io.output_channel -> unit Lwt.t

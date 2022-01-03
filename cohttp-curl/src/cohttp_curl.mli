module Private : sig
  module Sink : sig
    type 'a t

    val string : string t
    val discard : unit t
  end

  module Source : sig
    type t

    val empty : t
    val string : string -> t
  end

  module Request : sig
    type 'a t

    val curl : _ t -> Curl.t

    val body : 'a t -> 'a
    (** [body t] this must be called after curl completes the requests. it can
        only be called once *)

    val create :
      ?timeout_ms:int ->
      ?headers:Http.Header.t ->
      Http.Method.t ->
      uri:string ->
      input:Source.t ->
      output:'a Sink.t ->
      on_response:(Http.Response.t -> unit) ->
      'a t
  end
end

(** Curl & Async based client *)

module Sink : sig
  (** A sink defines where the response body may be written *)

  type 'a t

  val string : string t
  val discard : unit t
end

module Source : sig
  (** A source defines where the request body is read from *)

  type t

  val empty : t
  val string : string -> t
end

module Context : sig
  (** A context shares the curl event handling logic for all curl requests
      associated to it *)

  type t

  val create : unit -> t
end

module Response : sig
  (** Response for the http requests *)

  type 'a t
  (** ['a t] represents a response for a request. ['a] determines how the
      response body is handled *)

  val response : _ t -> Http.Response.t Async_kernel.Deferred.t
  val body : 'a t -> 'a Async_kernel.Deferred.t
  val cancel : _ t -> unit

  module Expert : sig
    val curl : _ t -> Curl.t
  end
end

module Request : sig
  (** Http requests *)

  type 'a t
  (** ['a t] represents an http request ['a] determines how the response body is
      handled. *)

  val create :
    ?timeout:Core.Time_float.Span.t (** timeout for the request *) ->
    ?headers:Http.Header.t (** http headers *) ->
    Http.Method.t (** http method *) ->
    uri:string (** uri *) ->
    input:Source.t (** request body *) ->
    output:'a Sink.t (** response body *) ->
    'a t

  module Expert : sig
    val curl : _ t -> Curl.t
  end
end

val submit : Context.t -> 'a Request.t -> 'a Response.t
(** [submit ctx request] submits a request and returns the response. Once a
    request is submitted, it may not be submitted again. *)

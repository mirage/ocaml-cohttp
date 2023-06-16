module type S = sig
  module IO : S.IO

  type body
  type conn = IO.conn * Connection.t [@@warning "-3"]

  type response_action =
    [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit IO.t)
    | `Response of Http.Response.t * body ]
  (** A request handler can respond in two ways:

      - Using [`Response], with a {!Response.t} and a {!body}.
      - Using [`Expert], with a {!Response.t} and an IO function that is
        expected to write the response body. The IO function has access to the
        underlying {!IO.ic} and {!IO.oc}, which allows writing a response body
        more efficiently, stream a response or to switch protocols entirely
        (e.g. websockets). Processing of pipelined requests continue after the
        {!unit IO.t} is resolved. The connection can be closed by closing the
        {!IO.ic}. *)

  type t

  val make_response_action :
    ?conn_closed:(conn -> unit) ->
    callback:(conn -> Http.Request.t -> body -> response_action IO.t) ->
    unit ->
    t

  val make_expert :
    ?conn_closed:(conn -> unit) ->
    callback:
      (conn ->
      Http.Request.t ->
      body ->
      (Http.Response.t * (IO.ic -> IO.oc -> unit IO.t)) IO.t) ->
    unit ->
    t

  val make :
    ?conn_closed:(conn -> unit) ->
    callback:(conn -> Http.Request.t -> body -> (Http.Response.t * body) IO.t) ->
    unit ->
    t

  val respond :
    ?headers:Http.Header.t ->
    ?flush:bool ->
    status:Http.Status.t ->
    body:body ->
    unit ->
    (Http.Response.t * body) IO.t
  (** [respond ?headers ?flush ~status ~body] will respond to an HTTP request
      with the given [status] code and response [body]. If [flush] is true, then
      every response chunk will be flushed to the network rather than being
      buffered. [flush] is true by default. The transfer encoding will be
      detected from the [body] value and set to chunked encoding if it cannot be
      determined immediately. You can override the encoding by supplying an
      appropriate [Content-length] or [Transfer-encoding] in the [headers]
      parameter. *)

  val respond_string :
    ?headers:Http.Header.t ->
    ?flush:bool ->
    status:Http.Status.t ->
    body:string ->
    unit ->
    (Http.Response.t * body) IO.t

  val callback : t -> IO.conn -> IO.ic -> IO.oc -> unit IO.t
end

type writer

include
  Cohttp.Generic.Server.S
    with module IO = Io.IO
     and type body = Body.t
     and type response = writer -> unit

val respond :
  ?headers:Http.Header.t ->
  status:Http.Status.t ->
  body:_ Eio.Flow.source ->
  unit ->
  response IO.t

val run :
  ?max_connections:int ->
  ?additional_domains:_ Eio__Domain_manager.t * int ->
  ?stop:'a Eio.Promise.t ->
  on_error:(exn -> unit) ->
  _ Eio.Net.listening_socket ->
  t ->
  'a

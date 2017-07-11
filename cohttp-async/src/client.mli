open! Core
open! Async

(** Send an HTTP request with an arbitrary body
    The request is sent as-is. *)
val request :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?uri:Uri.t ->
  ?body:Body.t ->
  Cohttp.Request.t ->
  (Cohttp.Response.t * Body.t) Deferred.t

(** Send an HTTP request with arbitrary method and a body
    Infers the transfer encoding *)
val call :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Cohttp.Code.meth ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Deferred.t

val callv :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  Uri.t ->
  (Cohttp.Request.t * Body.t) Pipe.Reader.t ->
  (Cohttp.Response.t * Body.t) Pipe.Reader.t Deferred.t

(** Send an HTTP GET request *)
val get :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Deferred.t

(** Send an HTTP HEAD request *)
val head :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  Cohttp.Response.t Deferred.t

(** Send an HTTP DELETE request *)
val delete :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Deferred.t

(** Send an HTTP POST request.
    [chunked] encoding is off by default as not many servers support it
*)
val post :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Deferred.t

(** Send an HTTP PUT request.
    [chunked] encoding is off by default as not many servers support it
*)
val put :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Response.t * Body.t) Deferred.t

(** Send an HTTP PATCH request.
    [chunked] encoding is off by default as not many servers support it
*)
val patch :
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Response.t * Body.t) Deferred.t

(** Send an HTTP POST request in form format *)
val post_form:
  ?interrupt:unit Deferred.t ->
  ?ssl_config:Conduit_async.Ssl.config ->
  ?headers:Cohttp.Header.t ->
  params:(string * string list) list ->
  Uri.t ->
  (Response.t * Body.t) Deferred.t

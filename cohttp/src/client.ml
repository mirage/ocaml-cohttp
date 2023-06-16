(** The [Client] module is a collection of convenience functions for
    constructing and processing requests. *)
module type BASE = sig
  type +'a io
  type 'a with_context
  type body

  val map_context : 'a with_context -> ('a -> 'b) -> 'b with_context

  val call :
    (?headers:Http.Header.t ->
    ?body:body ->
    ?chunked:bool ->
    Http.Method.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context
  (** [call ?headers ?body ?chunked meth uri]

      @return
        [(response, response_body)] Consume [response_body] in a timely fashion.
        Please see {!val:call} about how and why.
      @param chunked
        use chunked encoding if [true]. The default is [false] for compatibility
        reasons. *)
end

module type S = sig
  include BASE

  val head :
    (?headers:Http.Header.t -> Uri.t -> Http.Response.t io) with_context

  val get :
    (?headers:Http.Header.t -> Uri.t -> (Http.Response.t * body) io)
    with_context

  val delete :
    (?body:body ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context

  val post :
    (?body:body ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context

  val put :
    (?body:body ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context

  val patch :
    (?body:body ->
    ?chunked:bool ->
    ?headers:Http.Header.t ->
    Uri.t ->
    (Http.Response.t * body) io)
    with_context
end

module Make (Base : BASE) (IO : S.IO with type 'a t = 'a Base.io) = struct
  include Base
  open IO

  let call =
    map_context call (fun call ?headers ?body ?chunked meth uri ->
        let () =
          Logs.info (fun m -> m "%a %a" Http.Method.pp meth Uri.pp uri)
        in
        call ?headers ?body ?chunked meth uri)

  let delete =
    map_context call (fun call ?body ?chunked ?headers uri ->
        call ?body ?chunked ?headers `DELETE uri)

  let get = map_context call (fun call ?headers uri -> call ?headers `GET uri)

  let head =
    map_context call (fun call ?headers uri ->
        call ?headers `HEAD uri >>= fun (response, _body) -> return response)

  let patch =
    map_context call (fun call ?body ?chunked ?headers uri ->
        call ?body ?chunked ?headers `PATCH uri)

  let post =
    map_context call (fun call ?body ?chunked ?headers uri ->
        call ?body ?chunked ?headers `POST uri)

  let put =
    map_context call (fun call ?body ?chunked ?headers uri ->
        call ?body ?chunked ?headers `PUT uri)
end

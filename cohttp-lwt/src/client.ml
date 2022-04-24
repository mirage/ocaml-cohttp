open Lwt.Infix
module Header = Cohttp.Header

module Make (Connection : S.Connection) =
struct
  module Net = Connection.Net
  module No_cache = Connection_cache.Make_no_cache ( Connection )
  module Request = Make.Request (Net.IO)

  type ctx = Net.ctx

  let is_meth_chunked = function
    | `HEAD -> false
    | `GET -> false
    | `DELETE -> false
    | _ -> true

  let cache = ref No_cache.(request (create ()))

  let set_cache c = cache := c

  let request ?ctx =
    match ctx with
    | None -> !cache
    | Some ctx -> No_cache.(request (create ~ctx ()))

  let call ?ctx ?headers ?(body = `Empty) ?chunked meth uri
    =
    let headers = match headers with None -> Header.init () | Some h -> h in
    let chunked =
      match chunked with None -> is_meth_chunked meth | Some v -> v
    in
    begin
      if chunked
      then
        Lwt.return
          (Request.make_for_client ~headers ~chunked meth uri,
           body)
      else
        (* If chunked is not allowed, then obtain the body length and
           insert header *)
        Body.length body >|= fun (body_length, buf) ->
        (Request.make_for_client ~headers ~chunked ~body_length meth uri,
         buf)
    end >>= fun (req,body) ->
    request ?ctx ~body req

  (* The HEAD should not have a response body *)
  let head ?ctx ?headers uri = call ?ctx ?headers `HEAD uri >|= fst
  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri

  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri

  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri

  let post_form ?ctx ?headers ~params uri =
    let headers =
      Header.add_opt_unless_exists headers "content-type"
        "application/x-www-form-urlencoded"
    in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  let callv ?(ctx = Net.default_ctx) uri reqs =
    let mutex = Lwt_mutex.create () in
    Net.resolve ~ctx uri
    >>= Connection.connect ~ctx
    >>= fun connection ->
    Lwt.return @@ Lwt_stream.from @@ fun () ->
    Lwt_stream.get reqs >>= function
    | None ->
      Connection.close connection |> ignore;
      Lwt.return_none
    | Some (req, body) ->
      Lwt_mutex.with_lock mutex @@ fun () ->
      Connection.request connection ~body req >|= Option.some
end

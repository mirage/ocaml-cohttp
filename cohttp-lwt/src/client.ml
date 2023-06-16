open Lwt.Infix
module Header = Cohttp.Header

module Make (Connection : S.Connection) = struct
  module Net = Connection.Net
  module No_cache = Connection_cache.Make_no_cache (Connection)
  module Request = Make.Request (Net.IO)

  let cache = ref No_cache.(call (create ()))
  let set_cache c = cache := c

  type ctx = Net.ctx

  let cache ?ctx =
    match ctx with
    | None -> !cache
    | Some ctx -> No_cache.(call (create ~ctx ()))

  include
    Cohttp.Client.Make
      (struct
        type 'a io = 'a Lwt.t
        type body = Body.t
        type 'a with_context = ?ctx:ctx -> 'a

        let map_context v f ?ctx = f (v ?ctx)

        let call ?ctx ?headers ?body ?chunked meth uri =
          let add_transfer =
            Header.add_transfer_encoding
              (Option.value ~default:(Header.init ()) headers)
          in
          match chunked with
          | None -> cache ?ctx ?headers ?body meth uri
          | Some true ->
              let headers = add_transfer Cohttp.Transfer.Chunked in
              cache ?ctx ~headers ?body meth uri
          | Some false ->
              Option.value ~default:`Empty body |> Body.length
              >>= fun (length, body) ->
              let headers = add_transfer (Cohttp.Transfer.Fixed length) in
              cache ?ctx ~headers ~body meth uri
      end)
      (Connection.Net.IO)

  let post_form ?ctx ?headers ~params uri =
    let headers =
      Header.add_opt_unless_exists headers "content-type"
        "application/x-www-form-urlencoded"
    in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  let callv ?(ctx = Net.default_ctx) uri reqs =
    let mutex = Lwt_mutex.create () in
    Net.resolve ~ctx uri >>= Connection.connect ~ctx >>= fun connection ->
    Lwt.return
    @@ Lwt_stream.from
    @@ fun () ->
    Lwt_stream.get reqs >>= function
    | None ->
        Connection.close connection |> ignore;
        Lwt.return_none
    | Some (req, body) ->
        Lwt_mutex.with_lock mutex @@ fun () ->
        let headers, meth, uri, enc =
          Request.(headers req, meth req, uri req, encoding req)
        in
        let headers = Header.add_transfer_encoding headers enc in
        Connection.call connection ~headers ~body meth uri >|= Option.some
end

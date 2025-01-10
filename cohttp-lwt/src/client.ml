open Lwt.Infix
module Header = Cohttp.Header

module Make (Connection : S.Connection) = struct
  module Net = Connection.Net
  module No_cache = Connection_cache.Make_no_cache (Connection)
  module Request = Make.Request (Net.IO)

  type ctx = S.call

  let cache = ref No_cache.(call (create ()))
  let set_cache c = cache := c

  include
    Cohttp.Generic.Client.Make
      (struct
        type 'a io = 'a Lwt.t
        type body = Body.t
        type 'a with_context = ?ctx:ctx -> 'a

        let map_context v f ?ctx = f (v ?ctx)

        let call ?(ctx :ctx option) ?headers ?body ?chunked meth uri =
          let cache =
            match ctx with
            | None -> !cache
            | Some ctx -> ctx
          in
          let add_transfer =
            Header.add_transfer_encoding
              (Option.value ~default:(Header.init ()) headers)
          in
          match chunked with
          | None -> cache ?headers ?body meth uri
          | Some true ->
              let headers = add_transfer Cohttp.Transfer.Chunked in
              cache ~headers ?body meth uri
          | Some false ->
              Option.value ~default:`Empty body |> Body.length
              >>= fun (length, body) ->
              let headers = add_transfer (Cohttp.Transfer.Fixed length) in
              cache ~headers ~body meth uri
      end)
      (Connection.Net.IO)

  let post_form ?ctx ?headers ~params uri =
    let headers =
      Header.add_opt_unless_exists headers "content-type"
        "application/x-www-form-urlencoded"
    in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri
end

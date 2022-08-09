open Cohttp_eio

let read_body req reader =
  let body = Server.read_fixed req reader in
  Server.text_response @@ Fmt.str "%a\n\n%s" Http.Request.pp req body

let app (req, reader, _client_addr) =
  match Http.Request.resource req with
  | "/get" -> Server.text_response (Fmt.to_to_string Http.Request.pp req)
  | "/get_error" -> (
      try
        let _ = Server.read_fixed req reader in
        assert false
      with Invalid_argument e -> Server.text_response e)
  | "/post" -> read_body req reader
  | _ -> Server.bad_request_response

let () = Eio_main.run @@ fun env -> Server.run ~port:8080 env app

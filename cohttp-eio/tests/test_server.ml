open Cohttp_eio

let app (req, reader, _) =
  match Http.Request.resource req with
  | "/get" ->
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.Request.pp fmt req;
      Format.fprintf fmt "%!";
      Server.text_response (Buffer.contents buf)
  | "/get_error" -> (
      match Server.read_fixed req reader with
      | Some _ -> Server.text_response "FAIL"
      | None -> Server.text_response "PASS")
  | "/post" ->
      let body = Server.read_fixed req reader |> Option.get in
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.Request.pp fmt req;
      Format.fprintf fmt "\n\n%s%!" body;
      Server.text_response (Buffer.contents buf)
  | "/chunk" -> (
      let dump_chunk buf chunk =
        let s = Format.asprintf "\n%a" Body.pp_chunk chunk in
        Buffer.add_string buf s
      in
      let chunk_buf = Buffer.create 0 in
      match Server.read_chunked req reader (dump_chunk chunk_buf) with
      | Some headers ->
          let req = { req with headers } in
          Buffer.contents chunk_buf
          |> Format.asprintf "%a@ %s%!" Http.Request.pp req
          |> Server.text_response
      | None -> Server.bad_request_response)
  | _ -> Server.bad_request_response

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env -> Server.run ~port:!port env app

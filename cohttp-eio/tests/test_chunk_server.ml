open Cohttp_eio.Server

let dump_chunk buf chunk =
  let s = Format.asprintf "\n%a" Chunk.pp chunk in
  Buffer.add_string buf s

let app req =
  match Request.resource req with
  | "/" -> (
      let chunk_buf = Buffer.create 0 in
      match Request.read_chunk req (dump_chunk chunk_buf) with
      | Ok req ->
          Buffer.contents chunk_buf
          |> Format.asprintf "%a@ %s%!" Request.pp req
          |> Response.text
      | Error s -> Response.text s)
  | _ -> Response.not_found

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> run ~port:!port env sw app

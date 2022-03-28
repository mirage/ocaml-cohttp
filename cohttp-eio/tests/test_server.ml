open Cohttp_eio.Server

let app req =
  let body = match Request.read_fixed req with Ok s -> s | Error _ -> "" in
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Request.pp fmt req;
  Format.fprintf fmt "\n\n%s%!" body;
  Response.text (Buffer.contents buf)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> run ~port:8080 env sw app

open Cohttp_eio

let read_body (req, reader) =
  let body = Server.read_fixed (req, reader) in
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Http.Request.pp fmt req;
  Format.fprintf fmt "\n\n%s%!" body;
  Server.text_response (Buffer.contents buf)

let app (req, reader) =
  match Http.Request.resource req with
  | "/get" ->
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.Request.pp fmt req;
      Format.fprintf fmt "%!";
      Server.text_response (Buffer.contents buf)
  | "/get_error" -> (
      try
        let _ = Server.read_fixed (req, reader) in
        assert false
      with Invalid_argument e -> Server.text_response e)
  | "/post" -> read_body (req, reader)
  | _ -> Server.bad_request_response

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> Server.run ~port:8080 env sw app

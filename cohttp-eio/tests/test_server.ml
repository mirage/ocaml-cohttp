open Cohttp_eio

let pp_method fmt meth = Format.fprintf fmt "%s" (Http.Method.to_string meth)
let pp_version fmt v = Format.fprintf fmt "%s" (Http.Version.to_string v)

let pp fmt (req : Http.Request.t) =
  let fields =
    [
      Fmt.field "meth" (fun (t : Http.Request.t) -> t.meth) pp_method;
      Fmt.field "resource" (fun (t : Http.Request.t) -> t.resource) Fmt.string;
      Fmt.field "version" (fun (t : Http.Request.t) -> t.version) pp_version;
      Fmt.field "headers"
        (fun (t : Http.Request.t) -> t.headers)
        Http.Header.pp_hum;
    ]
  in
  Fmt.record fields fmt req

let read_body (req, reader) =
  let body = Server.read_fixed (req, reader) in
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt req;
  Format.fprintf fmt "\n\n%s%!" (Bytes.unsafe_to_string body);
  Server.text_response (Buffer.contents buf)

let app (req, reader) =
  match Http.Request.resource req with
  | "/get" ->
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      pp fmt req;
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

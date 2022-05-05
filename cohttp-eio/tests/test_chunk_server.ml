open Cohttp_eio
open Cohttp_eio.Server

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

let dump_chunk buf chunk =
  let s = Format.asprintf "\n%a" Body.pp_chunk chunk in
  Buffer.add_string buf s

let app (req, reader) =
  match Http.Request.resource req with
  | "/" -> (
      let chunk_buf = Buffer.create 0 in
      match Server.read_chunked (req, reader) (dump_chunk chunk_buf) with
      | headers ->
          let req = { req with headers } in
          Buffer.contents chunk_buf
          |> Format.asprintf "%a@ %s%!" pp req
          |> Server.text_response
      | exception Invalid_argument _ -> Server.bad_request_response)
  | _ -> Server.not_found_response

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> run ~port:!port env sw app

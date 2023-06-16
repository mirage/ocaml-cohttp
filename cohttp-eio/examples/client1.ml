open Cohttp_eio

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let resp, body =
    Client.get ~sw env#net (Uri.of_string "http://example.com")
  in
  if Http.Status.compare resp.status `OK = 0 then
    Fmt.string Format.std_formatter
    @@ Eio.Buf_read.(take_all @@ of_flow ~max_size:max_int body)
  else Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status

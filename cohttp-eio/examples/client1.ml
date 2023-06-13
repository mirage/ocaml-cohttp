open Cohttp_eio

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Client.get ~sw env#net (Uri.of_string "http://example.com") with
  | Result.Ok (resp, body) when Http.Status.compare resp.status `OK = 0 ->
      Fmt.string Format.std_formatter
      @@ Eio.Buf_read.(take_all @@ of_flow ~max_size:max_int body)
  | Result.Ok (resp, _) ->
      Fmt.epr "Unexpected HTTP status: %a" Http.Status.pp resp.status
  | Result.Error e -> Fmt.epr "HTTP error: %s" e

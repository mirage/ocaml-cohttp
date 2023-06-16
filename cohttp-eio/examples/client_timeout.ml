open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  (* Increment/decrement this value to see success/failure. *)
  let timeout_s = 0.01 in
  Eio.Time.with_timeout env#clock timeout_s (fun () ->
      Eio.Switch.run @@ fun sw ->
      let _, body =
        Client.get env#net ~sw (Uri.of_string "http://www.example.org")
      in
      Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) |> Result.ok)
  |> function
  | Ok s -> print_string s
  | Error (`Fatal e) -> Fmt.epr "fatal error: %s@." e
  | Error `Timeout -> Fmt.epr "Connection timed out@."

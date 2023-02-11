open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = Client.make sw env#net in
  let res = Client.get client "www.example.org" in
  match Body.read_content res with
  | Some body -> print_string body
  | None -> print_string "no body"

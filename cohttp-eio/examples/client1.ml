open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  let res = Client.get env ~host:"www.example.org" "/" in
  print_string @@ Client.read_fixed res

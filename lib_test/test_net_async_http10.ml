open Core.Std
open Async.Std
open Cohttp_async

let test () =
  print_endline "starting fetch";
  "http://www.rustorka.com:2710/announce?info_hash=/00%00%00%00%00%00%0000%000%00%00%00%00%00%000&peer_id=00000000000000000000"
  |> Uri.of_string
  |> Client.get
  >>| snd
  >>= Body.to_string
  >>| print_endline

let _ =
  Command.async ~summary:"Test HTTP/1.0 response"
    Command.Spec.empty test
  |> Command.run

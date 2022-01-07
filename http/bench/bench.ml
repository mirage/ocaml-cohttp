module Command = Core.Command
module Staged = Core.Staged
open Core_bench

let header_mem =
  let variations =
    [
      [ ("foo", "bar") ];
      [ ("transfer-encoding", "bar") ];
      [ ("traNsfer-eNcoding", "bar") ];
      [ ("traNsfer eNcoding", "bar") ];
    ]
  in
  let key = "transfer-encoding" in
  List.mapi
    (fun i header ->
      let header = Http.Header.of_list header in
      Bench.Test.create
        ~name:("Header.mem - " ^ string_of_int i)
        (fun () ->
          let (_ : bool) = Http.Header.mem header key in
          ()))
    variations

let () = Command.run @@ Bench.make_command header_mem

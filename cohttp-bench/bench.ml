module Command = Core.Command
module Staged = Core.Staged
open Core_bench

let header_names =
  [
    "Accept";
    "Accept-Encoding";
    "Accept-Language";
    "Cache-Control";
    "Connection";
    "Host";
    "If-Modified-Since";
    "If-None-Match";
    "Origin";
    "Referer";
    "Sec-Fetch-Dest";
    "Sec-Fetch-Mode";
    "Sec-Fetch-Site";
  ]

let header =
  header_names |> List.map (fun s -> (s, "value")) |> Http.Header.of_list

let bench_header_mem =
  Bench.Test.create ~name:"Header.mem" (fun () ->
      List.iter (fun key -> assert (Http.Header.mem header key)) header_names)

let () = Command_unix.run @@ Bench.make_command [ bench_header_mem ]

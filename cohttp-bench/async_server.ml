open Core
open Async
module Server = Cohttp_async.Server

let length = 2053
let text = String.make length 'a'
let headers = Cohttp.Header.of_list [ ("content-length", Int.to_string length) ]
let handler ~body:_ _sock _req = Server.respond_string ~headers text

let start_server port () =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    handler
  >>= fun server ->
  Deferred.forever () (fun () ->
      after Time_float.Span.(of_sec 0.5) >>| fun () ->
      Log.Global.printf "Active connections: %d" (Server.num_connections server));
  Deferred.never ()

let () =
  let module Command = Async_command in
  Command.async_spec ~summary:"Start a hello world Async server"
    Command.Spec.(
      empty
      +> flag "-p"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on")
    start_server
  |> Command_unix.run

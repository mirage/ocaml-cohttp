open Core
open Async
open Cohttp_async

let text = String.make 2053 'a'
let handler ~body:_ _sock _req = Server.respond_string text

let start_server port () =
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    handler
  >>= fun server ->
  Deferred.forever () (fun () ->
      after Time.Span.(of_sec 0.5) >>| fun () ->
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
  |> Command.run

open Lwt.Syntax
module Context = Cohttp_server_lwt_unix.Context
module Body = Cohttp_server_lwt_unix.Body

let text = String.make 2053 'a'

let server_callback ctx =
  Lwt.join
    [
      Context.discard_body ctx;
      Context.respond ctx (Http.Response.make ()) (Body.string text);
    ]

let main () =
  let* _server =
    let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
    let server =
      Cohttp_server_lwt_unix.create
        ~on_exn:(fun exn ->
          Format.eprintf "unexpected:@.%s@." (Printexc.to_string exn))
        server_callback
    in
    Lwt_io.establish_server_with_client_address ~backlog:10_000 listen_address
      (fun _addr ch -> Cohttp_server_lwt_unix.handle_connection server ch)
  in
  let forever, _ = Lwt.wait () in
  forever

let () =
  Printexc.record_backtrace true;
  ignore (Lwt_main.run (main ()))

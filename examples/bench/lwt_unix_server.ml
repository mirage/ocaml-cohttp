open Cohttp_lwt_unix

let text = String.make 2053 'a'

let server_callback _conn _req _body =
  Server.respond_string ~status:`OK ~body:text ()

let main () =
  Server.create ~backlog:11_000 (Server.make ~callback:server_callback ())

let () =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  ignore (Lwt_main.run (main ()))

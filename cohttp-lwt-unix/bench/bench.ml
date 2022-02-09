open Cohttp_lwt_unix

let handler _ req _ =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" -> Server.respond_string ~status:`OK ~body:(String.make 2053 'a') ()
  | _ -> Server.respond_string ~status:`Not_found ~body:"Route not found" ()

let start_server port =
  Format.eprintf "Listening for HTTP on port %d@." port;
  Format.eprintf "Try 'curl http://localhost:%d/'@.%!" port;
  Server.create
    ~ctx:(Cohttp_lwt_unix.Net.init ())
    ~mode:(`TCP (`Port port))
    (Server.make ~callback:handler ())

let () =
  Lwt_engine.(set (new libev ~backend:Ev_backend.kqueue ()));
  Lwt_main.run (start_server 8080)


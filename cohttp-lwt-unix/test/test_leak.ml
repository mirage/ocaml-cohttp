(*
  This test is meant to be used in the following way:
  - `make` the whole repo
  - `dune exec ./_build/default/cohttp-lwt-unix/test/test_leak.exe`
  - `curl -s 'localhost:8080/sleep'` in a different console
  - observe the first console having a stream of "sleep messages"
  - when stopping (CTRL+C) the `curl` request, the first console
  should show a closing connection message; if it does, then the
  test is successful, otherwise (and the server keeps sleeping),
  the test failed.
*)

open Lwt
open Cohttp_lwt_unix

let port = 8080

let callback (_, con) req _body =
  (* Record connection established *)
  let con_string = Cohttp.Connection.to_string con in
  Format.printf "Cohttp connection on %s@." con_string;
  (* Match given endpoint *)
  let uri = req |> Request.uri |> Uri.path in
  match uri with
  | "/sleep" ->
      (* Continuous sleep *)
      let rec get_busy () =
        Lwt_unix.sleep 1.0 >>= fun () ->
        Format.printf "I slept @.";
        get_busy ()
      in
      get_busy ()
  (* Unknown call *)
  | _ -> Server.respond_string ~status:`Not_found ~body:"Not found" ()

let start_server () =
  let server =
    Server.create
      ~mode:(`TCP (`Port port))
      (Server.make
         ~conn_closed:(fun _ -> Format.printf "Cohttp connection closed\n%!")
         ~callback ())
  in
  Printf.printf "Server running on port %d\n%!" port;
  server

let () = Lwt_main.run (start_server ())

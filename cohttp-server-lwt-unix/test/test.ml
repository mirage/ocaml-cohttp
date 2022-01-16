open Lwt.Syntax

let expected_response = "shutdown received"

let http_server =
  let module Context = Cohttp_server_lwt_unix.Context in
  Cohttp_server_lwt_unix.create (fun ctx ->
      let* () = Context.discard_body ctx in
      let req = Context.request ctx in
      match Http.Request.resource req with
      | "/shutdown" ->
          let resp = Http.Response.make () in
          Context.respond ctx resp
            (Cohttp_server_lwt_unix.Body.string expected_response)
      | _ -> assert false)

let fname = "test-lwt-unix"
let delete_socket () = try Unix.unlink fname with Unix.Unix_error _ -> ()

let server () =
  delete_socket ();
  let sockaddr = Unix.ADDR_UNIX fname in
  Lwt_io.establish_server_with_client_address sockaddr (fun _ conn ->
      Cohttp_server_lwt_unix.handle_connection http_server conn)

let client server =
  let uri = Uri.of_string "http://localhost/shutdown" in
  let ctx =
    let resolver =
      Resolver_lwt.init
        ~service:(fun _ ->
          Lwt.return_some { Resolver.name = "http"; port = 80; tls = false })
        ~rewrites:
          [ ("localhost", fun _ _ -> Lwt.return (`Unix_domain_socket fname)) ]
        ()
    in
    Cohttp_lwt_unix.Net.init ~resolver ()
  in
  let* _, body = Cohttp_lwt_unix.Client.call ~ctx `GET uri in
  let* body = Cohttp_lwt.Body.to_string body in
  assert (String.equal body expected_response);
  Lwt_io.shutdown_server server

let () =
  at_exit delete_socket;
  Lwt_main.run
    (let* server = server () in
     client server)

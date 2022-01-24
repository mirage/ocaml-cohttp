open Lwt.Infix
open Cohttp_lwt_unix

let ctx =
  let resolver =
    let h = Hashtbl.create 1 in
    Hashtbl.add h "docker" (`Unix_domain_socket "/var/run/docker.sock");
    Resolver_lwt_unix.static h
  in
  Cohttp_lwt_unix.Client.custom_ctx ~resolver ()

let print_docker_version () =
  let version =
    Client.get ~ctx (Uri.of_string "http://docker/version")
    |> fun (resp, body) ->
    let open Cohttp in
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "Body of length: %d\n" (String.length body);
    print_endline ("Received body\n" ^ body)
  in
  Lwt_eio.Promise.await_lwt version

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun () -> print_docker_version ()

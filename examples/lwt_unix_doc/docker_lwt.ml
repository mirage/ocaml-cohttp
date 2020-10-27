open Lwt.Infix

let resolve_unix_socket : Conduit_lwt.Endpoint.t -> 'edn option Lwt.t = function
  | IP _ -> Lwt.return_none
  | Domain v -> (
      match Domain_name.to_string v with
      | "docker" -> Lwt.return_some (Unix.ADDR_UNIX "/var/run/docker.sock")
      | _ -> Lwt.return_none )

let t =
  let ctx =
    Conduit_lwt.add ~priority:0 (* highest priority *) Conduit_lwt.TCP.protocol
      resolve_unix_socket Conduit.empty
  in
  Cohttp_lwt_unix.Client.get ~ctx (Uri.of_string "http://docker/version")
  >>= fun (resp, body) ->
  let open Cohttp in
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  print_endline ("Received body\n" ^ body)

let _ = Lwt_main.run t

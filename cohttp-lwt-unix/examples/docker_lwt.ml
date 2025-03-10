open Lwt.Infix

let ctx =
  let resolver =
    let h = Hashtbl.create 1 in
    Hashtbl.add h "docker" (`Unix_domain_socket "/var/run/docker.sock");
    Resolver_lwt_unix.static h
  in
  let net_ctx = Cohttp_lwt_unix.Client.custom_ctx ~resolver () in
  Cohttp_lwt_unix.No_cache.(call (create ~ctx:net_ctx ()))

let t =
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

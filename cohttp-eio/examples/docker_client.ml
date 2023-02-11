open Cohttp_eio

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Unix "/var/run/docker.sock" in
  let conn = Eio.Net.connect ~sw env#net addr in
  let req =
    Request.client_request ~host:"docker" ~resource:"/version" Method.Get
      Body.none
  in
  let res = Client.call ~conn req in
  let code = Response.status res |> Http.Status.to_int in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (Response.headers res |> Http.Header.to_string);
  match Body.read_content res with
  | Some body ->
      Printf.printf "Body of length: %d\n" (String.length body);
      print_endline ("Received body\n" ^ body)
  | None -> print_string "no body"

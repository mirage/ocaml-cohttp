module Switch = Eio.Switch
module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Client = Cohttp_eio.Client
module Response = Http.Response
module Status = Http.Status

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let addr = `Unix "/var/run/docker.sock" in
  let conn = Net.connect ~sw env#net addr in
  let res = Client.get ~conn ("docker", None) "/version" in
  let code = fst res |> Response.status |> Status.to_int in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n"
    (fst res |> Response.headers |> Http.Header.to_string);
  let body = Client.read_fixed res in
  Printf.printf "Body of length: %d\n" (String.length body);
  print_endline ("Received body\n" ^ body)

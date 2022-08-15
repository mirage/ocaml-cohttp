module Switch = Eio.Switch
module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Client = Cohttp_eio.Client
module Response = Http.Response
module Status = Http.Status

let conn env sw resource_path =
  let hostname = "docker" in
  let addr = `Unix "/var/run/docker.sock" in
  let flow = Net.connect ~sw env#net addr in
  let host = (hostname, None) in
  (resource_path, host, flow)

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let res = Client.get (conn env sw) "/version" in
  let code = fst res |> Response.status |> Status.to_int in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n"
    (fst res |> Response.headers |> Http.Header.to_string);
  match Client.read_fixed res with
  | Some body ->
      Printf.printf "Body of length: %d\n" (String.length body);
      print_endline ("Received body\n" ^ body)
  | None -> ()

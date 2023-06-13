module Switch = Eio.Switch
module Net = Eio.Net
module Stdenv = Eio.Stdenv
module Client = Cohttp_eio.Client
module Response = Http.Response
module Status = Http.Status

let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match
    Client.get env#net ~sw
    @@ Uri.make ~scheme:"httpunix" ~host:"/var/run/docker.sock" ~path:"/version"
         ()
  with
  | Result.Error error ->
      Printf.eprintf "fatal error: %sn" error;
      exit 1
  | Result.Ok (response, body) ->
      let code = response |> Response.status |> Status.to_int in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n"
        (response |> Response.headers |> Http.Header.to_string);
      let body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      Printf.printf "Body of length: %d\n" (String.length body);
      print_endline ("Received body\n" ^ body)

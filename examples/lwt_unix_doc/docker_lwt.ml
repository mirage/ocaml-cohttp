open Lwt.Infix
open Cohttp_lwt_unix

let t =
  Client.get (Uri.of_string "http://docker/version") >>= fun (resp, body) ->
  let open Cohttp in
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  print_endline ("Received body\n" ^ body)

let _ = Lwt_main.run t

open Lwt
open Cohttp
open Cohttp_lwt_unix

let body () =
  Lwt_eio.Promise.await_lwt
    ( Client.get (Uri.of_string "https://www.reddit.com/") |> fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Printf.printf "Response code: %d\n" code;
      Printf.printf "Headers: %s\n"
        (resp |> Response.headers |> Header.to_string);
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.printf "Body of length: %d\n" (String.length body);
      body )

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun () ->
  let body = body () in
  print_endline ("Received body\n" ^ body)

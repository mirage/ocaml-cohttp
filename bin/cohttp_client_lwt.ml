open Lwt
open Cohttp
open Cohttp_lwt_unix

let client uri =
  Printf.printf "client with uri %s\n%!" uri ;
  let uri = Uri.of_string uri in
  Printf.printf "client get\n%!" ;
  Client.get uri >>= fun (resp, body) ->
  Printf.printf "get returned\n%!" ;
  Cohttp_lwt_body.to_string body >>= fun s ->
  Printf.printf "body: %d\n" (String.length s) ;
  return_unit

let _ =
  Lwt_main.run
    (match Sys.argv with
     | [| _ ; uri |] -> client uri
     | args -> Printf.printf "%s URI\n%!" args.(0) ; return_unit)

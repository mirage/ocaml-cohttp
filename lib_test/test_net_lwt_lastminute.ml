open Lwt
open Printf

let u1 = "http://www.lastminute.com"
let u2 = "http://www.bbc.co.uk"
let u3 = "http://twitter.com"

(* a simple function to access the content of the response *)
let content = function
  | Some (_, body) -> 
      Cohttp_lwt_body.string_of_body body
  | _ -> return ""

(* launch both requests in parallel *)
let t =
  let uris = List.map Uri.of_string [u1;u2;u3] in
  Lwt_list.map_p (fun uri ->
    printf "fetching %s\n%!" (Uri.to_string uri);
    Cohttp_lwt_unix.Client.get uri
  ) uris

(* maps the result through the content function *)
let t2 = t >>= Lwt_list.map_p content

let t3 = t2 >>= Lwt_list.iter_p (fun x -> Lwt_io.printf "%d bytes\n" (String.length x))

(* launch the event loop *)
let v = Lwt_main.run t3


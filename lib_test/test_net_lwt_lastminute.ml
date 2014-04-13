open Lwt
open Printf

let u1 = "http://www.lastminute.com"
let u2 = "http://www.bbc.co.uk"
let u3 = "http://twitter.com"
let u4 = "http://www.rustorka.com:2710/announce?info_hash=/00%00%00%00%00%00%0000%000%00%00%00%00%00%000&peer_id=00000000000000000000"

(* a simple function to access the content of the response *)
let content (_,body) =
  catch
    (fun () -> Cohttp_lwt_body.to_string body)
    (fun _ -> return "")

(* launch both requests in parallel *)
let t =
  let uris = List.map Uri.of_string [u1;u2;u3;u4] in
  Lwt_list.map_p (fun uri ->
    printf "fetching %s\n%!" (Uri.to_string uri);
    Cohttp_lwt_unix.Client.get uri
  ) uris

(* maps the result through the content function *)
let t2 = t >>= Lwt_list.map_p content

let t3 = t2 >>= Lwt_list.iter_p (fun x -> Lwt_io.printf "%d bytes\n" (String.length x))

(* launch the event loop *)
let v = Lwt_main.run t3


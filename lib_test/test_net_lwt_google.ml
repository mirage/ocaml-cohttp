open Lwt
let body =
  (* you can paste this into utop to interactively mess around *)
  (* do a #require "cohttp.lwt" first *)
  let open Cohttp_lwt_unix in
  Lwt_main.run (
    Client.get Uri.(of_string "http://www.google.com/") >>= fun (r,b) ->
    Cohttp_lwt_body.to_string b
  )

let _ = prerr_endline body

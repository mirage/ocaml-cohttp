let body =
  (* you can paste this into utop to interactively mess around *)
  (* do a #require "cohttp.lwt" first *)
  Lwt.(Cohttp_lwt_unix.(Lwt_unix.run (Client.get Uri.(of_string "http://www.google.com/") >>= function None -> assert false |Some (r,b) -> Cohttp_lwt_body.string_of_body b)));;

let _ = prerr_endline body

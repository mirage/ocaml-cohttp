open Lwt

let fetch () =
  Cohttp_lwt_unix.Client.get (Uri.of_string "http://twitter.com")
  >>= fun _ -> return ()

let rec perform_get n =
  Lwt_io.printf "%d\n" n
  >>= fun () ->
  ignore (fetch ());
  ignore (fetch ());
  ignore (fetch ());
  ignore (fetch ());
  fetch ()
  >>= fun _ ->
  perform_get (n+1)

let _ =
  Lwt_main.run (perform_get 0)

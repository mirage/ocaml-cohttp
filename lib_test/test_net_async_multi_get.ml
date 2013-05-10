open Core.Std
open Async.Std

let fetch () =
  Cohttp_async.Client.get (Uri.of_string "http://twitter.com")
  >>= fun _ -> return ()

let rec perform_get n =
  Printf.printf "%d\n%!" n;
  don't_wait_for (fetch ());
  don't_wait_for (fetch ());
  don't_wait_for (fetch ());
  don't_wait_for (fetch ());
  fetch () 
  >>= fun _ ->
  perform_get (n+1)

let _ =
  let _ = perform_get 0 in
  Printf.printf "start\n%!";
  Scheduler.go ()

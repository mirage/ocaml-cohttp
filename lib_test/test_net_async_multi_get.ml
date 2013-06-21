open Core.Std
open Async.Std

let fetch uri =
  Cohttp_async.Client.get (Uri.of_string uri)
  >>= fun (res, body) ->
  Pipe.to_list body
  >>= fun bufs ->
  let buf = String.concat ~sep:"" bufs in
  printf "%s -> %d bytes\n" uri (String.length buf);
  return ()

let rec perform_get n =
  Printf.printf "%d\n%!" n;
  don't_wait_for (fetch "http://twitter.com");
  don't_wait_for (fetch "http://lastminute.com");
  don't_wait_for (fetch "http://www.bbc.co.uk");
  don't_wait_for (fetch "http://google.com");
  fetch "http://recoil.org"
  >>= fun _ ->
  match n with
  |10 -> return ()
  |n  -> perform_get (n+1)

let _ =
  let _ = perform_get 0 in
  Printf.printf "start\n%!";
  Scheduler.go ()

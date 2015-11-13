open Core.Std
open Async.Std
open Cohttp_async

let fetch uri =
  uri
  |> Uri.of_string
  |> Client.get
  >>| snd
  >>= Body.to_string
  >>| fun b ->
  printf "%s -> %d bytes\n" uri (String.length b)

let rec perform_get n =
  Log.Global.info "%d\n%!" n;
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
  Log.Global.info "start\n%!";
  Scheduler.go ()

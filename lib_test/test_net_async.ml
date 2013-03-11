(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Core.Std
open Async.Std
open Cohttp_async

let show_headers h =
  Cohttp.Header.iter (fun k v -> List.iter v ~f:(Printf.eprintf "%s: %s\n%!" k)) h

let make_net_req () =
  let headers = Cohttp.Header.of_list ["connection","close"] in
  let uri = Uri.of_string "http://anil.recoil.org/index.html" in
  let host = Option.value (Uri.host uri) ~default:"localhost" in
  match Uri_services.tcp_port_of_uri ~default:"http" uri with
  |None -> failwith "unable to resolve"
  |Some port ->
    Tcp.with_connection (Tcp.to_host_and_port host port)
     (fun _ ic oc ->
       Client.call ~headers `GET uri 
       >>= function
       |None -> 
         prerr_endline "<request failed>";
         assert false
       |Some (res, body) ->
         show_headers (Response.headers res);
         Pipe.iter body ~f:(fun b -> prerr_endline ("XX " ^ b); return ())
     )

let test_cases =
  (* TODO: can multiple async tests run with separate Schedulers? Is there
   * an Async-aware oUnit instead? *)
  let _ =  Async_core.Scheduler.within' (
    fun () ->
      Monitor.try_with make_net_req >>=
      function
      |Error exn -> 
        (* TODO: how to dump out top-level errors in a nicer way? *)
        Printf.fprintf stderr "err %s.\n%!" (Exn.backtrace ()); return ()
      |Ok _ -> return ()
  ) in
  Async_unix.Scheduler.go ()

(*
(* Returns true if the result list contains successes only.
   Copied from oUnit source as it isnt exposed by the mli *)
let rec was_successful =
  function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
        was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
        false

let _ =
  let suite = "Parser" >::: test_cases in
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
  exit 1
*)

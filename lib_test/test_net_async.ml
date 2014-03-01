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
  let uri = Uri.of_string "http://anil.recoil.org/" in
  Client.get ~headers uri 
  >>= fun (res, body) ->
   show_headers (Cohttp.Response.headers res);
   body
   |> Body.to_pipe
   |> Pipe.iter ~f:(fun b -> prerr_endline ("XX " ^ b); return ())

(* Create your own code from requestb.in *)
let requestbin_code = "1f6i9op1"

let make_net_post_req () =
  let headers = Cohttp.Header.of_list ["connection","close"] in
  let uri = Uri.of_string ("http://requestb.in/" ^ requestbin_code) in
  (* Create a big old body list *)
  let rec make_body acc =
   function
   |0 -> acc
   |n -> make_body (sprintf "fooooobody%d" n :: acc) (n-1) in
  let body = Body.of_pipe (Pipe.of_list (make_body [] 2)) in
  Client.call ~headers ~body `POST uri 
  >>= fun (res, body) ->
    show_headers (Cohttp.Response.headers res);
    body
    |> Body.to_pipe
    |> Pipe.iter ~f:(fun b -> prerr_endline ("XY " ^ b); return ())

let test_cases =
  (* TODO: can multiple async tests run with separate Schedulers? Is there
   * an Async-aware oUnit instead? *)
  let _ =  Scheduler.within' (
    fun () ->
      Monitor.try_with ( fun () ->
          make_net_req () >>= make_net_post_req) >>=
      function
      |Error exn -> 
        (* TODO: how to dump out top-level errors in a nicer way? *)
        Printf.fprintf stderr "err %s.\n%!" (Exn.backtrace ()); return ()
      |Ok _ ->
	Shutdown.exit 0
  ) in
  Scheduler.go ()


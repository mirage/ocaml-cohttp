(*{{{ Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org
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
  }}}*)

open Core.Std
open Async.Std
open Cohttp_async

let show_headers h =
  Cohttp.Header.iter (fun k v ->
    List.iter v ~f:(Log.Global.info "%s: %s\n%!" k)) h

let make_net_req () =
  let headers = Cohttp.Header.of_list [ "connection", "close" ] in
  let uri = Uri.of_string "http://github.com/" in
  Client.get ~headers uri
  >>= fun (res, body) ->
   show_headers (Cohttp.Response.headers res);
   body
   |> Body.to_pipe
   |> Pipe.iter ~f:(fun b -> return ())
  >>= fun _ ->
  print_endline "make_net_req done";
  return ()

let make_net_ssl_req () =
  let headers = Cohttp.Header.of_list ["connection","close"] in
  let uri = Uri.of_string "https://github.com/" in
  Client.get ~headers uri
  >>= fun (res, body) ->
   show_headers (Cohttp.Response.headers res);
   body
   |> Body.to_pipe
   |> Pipe.iter ~f:(fun b -> return ())
  >>= fun _ ->
  print_endline "make_net_ssl_req done";
  return ()

(* Create your own code from requestb.in *)
let requestbin_code = "1e6jxdg1"

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
    |> Pipe.iter ~f:(fun _b -> return ())
  >>= fun _ ->
  print_endline "make_net_post_req done";
  return ()

let run () =
    make_net_req ()
    >>= make_net_ssl_req

let test_cases =
  let open Command.Spec in
  Command.async_basic ~summary:"Run HTTP Async client tests"
    empty run
  |> Command.run

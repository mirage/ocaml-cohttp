(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

let ls_dir =
  Sys.ls_dir "."
  |> List.filter ~f:(fun fname ->
      match Sys.is_file fname with |`Yes -> true |_ -> false)

open Async.Std
open Cohttp_async

let handler ?body sock req =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/" | "" ->
    (* Get a list of current files and map to HTML *)
    List.map ls_dir ~f:(fun f -> sprintf "<li><a href='/%s'>/%s</a></li>" f f)
    |> String.concat ~sep:"\n"
    |> sprintf "<html><body><ul>
      <li><a href='/hello'>/hello</a></li>
      <li><a href='/hellopipe'>/hellopipe</a></li>
      <li><a href='/timer'>timer</a></li>
      <li><i>Files</i></li>
      %s</ul></body></html>" 
    |> Server.respond_with_string

  | "/hello" ->
    Server.respond_with_string "hello world"

  | "/hellopipe" ->
    let body = Pipe.of_list ["hello";"world"] in
    Server.respond_with_pipe body

  | "/timer" ->
    let rd,wr = Pipe.create () in
    Pipe.write_without_pushback wr "<html><body>";
    Clock.every (Time.Span.of_sec 1.0)
      (fun () ->
         Pipe.write_without_pushback wr (Time.to_string (Time.now ()) ^ "<br>");
      );
    Server.respond_with_pipe rd

  | _ ->
    Server.resolve_local_file ~docroot:"." ~uri
    |> Server.respond_with_file

let make_net_server () =
  Server.create (Tcp.on_port 8080) handler

let _ = 
  let _server = make_net_server () in
  Scheduler.go ()

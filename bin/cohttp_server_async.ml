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
open Async.Std
open Cohttp_async

(** HTTP handler *)
let handler ~info ~docroot ~verbose ~body sock req =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Get a canonical filename from the URL and docroot *)
  let file_name = Server.resolve_local_file ~docroot ~uri in
  Unix.stat file_name
  >>= fun stat ->
  (* Log the request to the console *)
  printf "%s %s %s\n" 
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path
    (match verbose with
     | true -> Sexp.to_string_hum (Unix.Stats.sexp_of_t stat)
     | false -> ""
    );
  match stat.Unix.Stats.kind with
  (* Get a list of current files and map to HTML *)
  | `Directory ->
    Sys.ls_dir file_name
    >>= Deferred.List.map ~f:(fun f ->
        let file_name = Filename.concat file_name f in
        Unix.stat file_name
        >>= fun stat ->
        let li l = sprintf "<li><a href=\"%s\">%s</a></li>" (Uri.to_string l) in
        let link = Uri.with_path uri (Filename.concat path f) in
        match stat.Unix.Stats.kind with
        | `Directory -> return (li link (sprintf "<i>%s/</i>" f))
        | `File -> return (li link f)
        | `Socket|`Block|`Fifo|`Char|`Link -> return (sprintf "<s>%s</s>" f))
    (* Concatenate the HTML into a response *)
    >>= fun html ->
    String.concat ~sep:"\n" html
    |> fun contents -> sprintf "
         <html>
           <body>
           <h2>Directory Listing for %s</h2>
           <ul>%s</ul>
           <hr>%s
           </body>
         </html>" file_name contents info
                       |> Server.respond_with_string
  (* Serve the local file contents *)
  | `File ->
    Server.resolve_local_file ~docroot:"." ~uri
    |> Server.respond_with_file
  (* Any other file type is simply forbidden *)
  | `Socket | `Block | `Fifo | `Char | `Link ->
    Server.respond_with_string ~code:`Forbidden
      "<html><body><h2>Forbidden</h2>
        <p>This is not a normal file or directory</p></body></html>"

let start_server docroot port host verbose () =
  printf "Listening for HTTP requests on: %s %d\n" host port;
  let info = sprintf "Served by Cohttp/Async listening on %s:%d" host port in
  Server.create ~on_handler_error:`Ignore (Tcp.on_port port) (handler ~info ~docroot ~verbose)
  >>= fun _ -> never ()

let _ = 
  Command.async_basic
    ~summary:"Serve the local directory contents via HTTP"
    Command.Spec.(
      empty
      +> anon (maybe_with_default "." ("docroot" %: string))
      +> flag "-p" (optional_with_default 8080 int) ~doc:"TCP port to listen on"
      +> flag "-s" (optional_with_default "0.0.0.0" string) ~doc:"IP address to listen on"
      +> flag "-v" (optional_with_default false bool) ~doc:"Verbose logging output to console"
    ) start_server 
  |> Command.run 

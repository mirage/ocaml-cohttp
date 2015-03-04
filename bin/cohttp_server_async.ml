(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open Cohttp_server

let serve_file ~docroot ~uri =
  Server.resolve_local_file ~docroot ~uri
  |> Server.respond_with_file

(** HTTP handler *)
let rec handler ~info ~docroot ~verbose ~index ~body sock req =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Log the request to the console *)
  printf "%s %s%!"
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path;
  (* Get a canonical filename from the URL and docroot *)
  let file_name = Server.resolve_local_file ~docroot ~uri in
  try_with (fun () ->
    Unix.stat file_name
    >>= fun stat ->
    printf " %s\n%!" (if verbose
      then Sexp.to_string_hum (Unix.Stats.sexp_of_t stat)
      else "");
    match stat.Unix.Stats.kind with
    (* Get a list of current files and map to HTML *)
    | `Directory -> begin
      let path_len = String.length path in
      if path_len <> 0 && path.[path_len - 1] <> '/'
      then Server.respond_with_redirect (Uri.with_path uri (path^"/"))
      (* Check if the index file exists *)
      else Sys.file_exists (file_name / index)
      >>= function
      | `Yes -> (* Serve the index file directly *)
        let uri = Uri.with_path uri (path / index) in
        serve_file ~docroot ~uri
      | `No | `Unknown -> (* Do a directory listing *)
        Sys.ls_dir file_name
        >>= Deferred.List.map ~f:(fun f ->
          let file_name = file_name / f in
          try_with (fun () ->
            Unix.stat file_name
            >>| fun stat -> (Some stat.Unix.Stats.kind, stat.Unix.Stats.size, f)
          ) >>| function Ok v -> v | Error _ -> (None, 0L, f))
        >>= fun listing ->
        html_of_listing uri path (sort ((Some `Directory,0L,"..")::listing)) info
      |> Server.respond_with_string
    end
    (* Serve the local file contents *)
    | `File -> serve_file ~docroot ~uri
    (* Any other file type is simply forbidden *)
    | `Socket | `Block | `Fifo | `Char | `Link ->
      Server.respond_with_string ~code:`Forbidden
        (html_of_forbidden_unnormal path info)
  )
  >>= (function
  | Ok res -> return res
  | Error exn ->
    printf "\n%!"; (* Close the request logging line. *)
    begin match Monitor.extract_exn exn with
    | Unix.Unix_error (Unix.ENOENT, "stat", p) ->
      if p = ("((filename "^file_name^"))") (* Really? *)
      then Server.respond_with_string ~code:`Not_found
        (html_of_not_found path info)
      else raise exn
    | _ -> raise exn
    end
  )


let start_server docroot port host index verbose () =
  printf "Listening for HTTP requests on: %s %d\n%!" host port;
  let info = sprintf "Served by Cohttp/Async listening on %s:%d" host port in
  Unix.Inet_addr.of_string_or_getbyname host
  >>= fun host ->
  let listen_on = Tcp.Where_to_listen.create
      ~socket_type:Socket.Type.tcp
      ~address:(`Inet (host,port))
      ~listening_on:(fun _ -> port)
  in
  Server.create
    ~on_handler_error:`Ignore
    listen_on
    (handler ~info ~docroot ~index ~verbose)
  >>= fun _ -> never ()

let _ =
  Command.async_basic
    ~summary:"Serve the local directory contents via HTTP"
    Command.Spec.(
      empty
      +> anon (maybe_with_default "." ("docroot" %: string))
      +> flag "-p" (optional_with_default 8080 int) ~doc:"port TCP port to listen on"
      +> flag "-s" (optional_with_default "0.0.0.0" string) ~doc:"address IP address to listen on"
      +> flag "-i" (optional_with_default "index.html" string) ~doc:"file Name of index file in directory"
      +> flag "-v" (optional_with_default false bool) ~doc:" Verbose logging output to console"
    ) start_server
  |> Command.run

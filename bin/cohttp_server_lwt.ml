(*
 * Copyright (c) 2014 Romain Calascibetta <romain.calascibetta@gmail.com>
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Cohttp
open Cohttp_lwt_unix

let ( / ) = Filename.concat

let serve_file ~docroot ~uri =
  let fname = Server.resolve_local_file ~docroot ~uri in
  Server.respond_file ~fname ()

let compare_kind = function
  | Some Unix.S_DIR, Some Unix.S_DIR -> 0
  | Some Unix.S_DIR, _               -> -1
  | _              , Some Unix.S_DIR -> 1
  | Some Unix.S_REG, Some Unix.S_REG -> 0
  | Some Unix.S_REG, _               -> 1
  | _              , Some Unix.S_REG -> -1
  | _              , _               -> 0

let sort = List.sort (fun (ka,a) (kb,b) ->
  let c = compare_kind (ka,kb) in
  if c <> 0 then c
  else String.compare (String.lowercase a) (String.lowercase b)
)

let li l = Printf.sprintf "<li><a href=\"%s\">%s</a></li>" (Uri.to_string l)

let ls_dir dir =
  Lwt_stream.to_list
    (Lwt_stream.filter ((<>) ".")
       (Lwt_unix.files_of_directory dir))

let rec handler ~info ~docroot ~verbose ~index sock req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Get a canonical filename from the URL and docroot *)
  let file_name = Server.resolve_local_file ~docroot ~uri in
  Lwt_unix.stat file_name
  >>= fun stat ->
  (* Log the request to the console *)
  Printf.printf "%s %s %s\n%!"
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path
    (match verbose with
     | true -> ""
     | false -> ""
    );
  match stat.Unix.st_kind with
  | Unix.S_DIR -> begin
    match Sys.file_exists (file_name / index) with
    | true -> let uri = Uri.with_path uri (path / index) in
              Server.respond_redirect uri ()
    | false ->
      ls_dir file_name
      >>= Lwt_list.map_s (fun f ->
        let file_name = file_name / f in
        Lwt.try_bind
          (fun () -> Lwt_unix.stat file_name)
          (fun stat -> return (Some stat.Unix.st_kind, f))
          (fun exn -> return (None, f)))
      >>= fun listing ->
      let html = List.map (fun (kind, f) ->
        let link = Uri.with_path uri (path / f) in
        match kind with
        | Some Unix.S_DIR -> li link (Printf.sprintf "<i>%s/</i>" f)
        | Some Unix.S_REG -> li link f
        | Some _ -> Printf.sprintf "<s>%s</s>" f
        | None -> Printf.sprintf "<li>Error with file: %s</li>" path
      ) (sort listing) in
      let contents = String.concat "\n" html in
      let body = Printf.sprintf "
              <html>
                <body>
                <h2>Directory Listing for %s</h2>
                <ul>%s</ul>
                <hr>%s
                </body>
              </html>"
        path contents info in
      Server.respond_string ~status:`OK ~body ()
  end
  | Unix.S_REG -> serve_file ~docroot ~uri
  | _ ->
    Server.respond_string ~status:`Forbidden
      ~body:"<html><body><h2>Forbidden</h2>
        <p>This is not a normal file or directory</p></body>/html>"
      ()

let start_server docroot port host index verbose () =
  Printf.printf "Listening for HTTP request on: %s %d\n%!" host port;
  let info = Printf.sprintf "Served by Cohttp/Lwt listening on %s:%d" host port in
  let conn_closed id () = Printf.printf "connection %s closed\n%!"
      (Connection.to_string id) in
  let callback = handler ~info ~docroot ~verbose ~index in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:host ~port:port config

let host = ref "0.0.0.0"
let port = ref 8080
let index = ref "index.html"
let verbose = ref false
let rest = ref []

let usage = "usage " ^ Sys.argv.(0) ^ " input [-o output]"

let arglist = [
  ("-p", Arg.Int (fun i -> port := i), ": TCP port to listen on");
  ("-s", Arg.String (fun s -> host := s), ": IP address to listen on");
  ("-i", Arg.String (fun s -> index := s), ": Name of index file in directory");
  ("-v", Arg.Bool (fun b -> verbose := b), ": logging output to console");
]

let _ =
  try Arg.parse arglist (fun x -> rest := x :: !rest) usage;
    begin match !rest with
    | [] -> Lwt_unix.run (start_server "." !port !host !index !verbose ())
    | dir::_ -> Lwt_unix.run (start_server dir !port !host !index !verbose ())
    end
  with
  | Failure s -> print_endline s
  | Sys_error s -> print_endline s

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

open Printf

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

let li l = sprintf "<li><a href=\"%s\">%s</a></li>" (Uri.to_string l)

let ls_dir dir =
  Lwt_stream.to_list
    (Lwt_stream.filter ((<>) ".")
       (Lwt_unix.files_of_directory dir))

let handler ~info ~docroot ~verbose ~index (ch,conn) req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Log the request to the console *)
  printf "%s %s %s %s\n%!"
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path
    (match verbose with
    | true -> ""
    | false -> ""
    )
    (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch));
  (* Get a canonical filename from the URL and docroot *)
  let file_name = Server.resolve_local_file ~docroot ~uri in
  catch (fun () ->
    Lwt_unix.stat file_name
    >>= fun stat ->
    match stat.Unix.st_kind with
    | Unix.S_DIR -> begin
      let path_len = String.length path in
      if path_len <> 0 && path.[path_len - 1] <> '/'
      then Server.respond_redirect (Uri.with_path uri (path^"/")) ()
      else match Sys.file_exists (file_name / index) with
      | true -> let uri = Uri.with_path uri (path / index) in
                serve_file ~docroot ~uri
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
          let encoded_f = Uri.pct_encode f in
          match kind with
          | Some Unix.S_DIR ->
            let link = Uri.with_path uri (path / encoded_f / "") in
            li link (sprintf "<i>%s/</i>" f)
          | Some Unix.S_REG ->
            let link = Uri.with_path uri (path / encoded_f) in
            li link f
          | Some _ -> sprintf "<li><s>%s</s></li>" f
          | None -> sprintf "<li>Error with file: %s</li>" f
        ) (sort listing) in
        let contents = String.concat "\n" html in
        let body = sprintf "
              <html>
                <body>
                <h2>Directory Listing for <em>%s</em></h2>
                <ul>%s</ul>
                <hr />%s
                </body>
              </html>"
          (Uri.pct_decode path) contents info in
        Server.respond_string ~status:`OK ~body ()
    end
    | Unix.S_REG -> serve_file ~docroot ~uri
    | _ ->
      Server.respond_string ~status:`Forbidden
        ~body:(sprintf "<html><body><h2>Forbidden</h2>
        <p><b>%s</b> is not a normal file or directory</p>
        <hr />%s</body></html>" path info)
        ()
  ) (function
  | Unix.Unix_error(Unix.ENOENT, "stat", p) as e ->
    if p = file_name
    then Server.respond_string ~status:`Not_found
      ~body:(sprintf "<html><body><h2>Not Found</h2>
      <p><b>%s</b> was not found on this server</p>
      <hr />%s</body></html>" path info)
      ()
    else fail e
  | e -> fail e
  )

let string_of_sockaddr = function
  | Unix.ADDR_UNIX x -> x
  | Unix.ADDR_INET (inet_addr, port) ->
    (Unix.string_of_inet_addr inet_addr) ^ ":" ^ (string_of_int port)

let start_server docroot port host index verbose cert key () =
  printf "Listening for HTTP request on: %s %d\n" host port;
  let info = sprintf "Served by Cohttp/Lwt listening on %s:%d" host port in
  let conn_closed (ch,conn) =
    printf "connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch)) in
  let callback = handler ~info ~docroot ~verbose ~index in
  let config = Server.make ~callback ~conn_closed () in
  let mode = match cert, key with
    | Some c, Some k -> `TLS (`Crt_file_path c, `Key_file_path k, `No_password, `Port port)
    | _ -> `TCP (`Port port)
  in
  Server.create ~mode config

let host = ref "0.0.0.0"
let port = ref 8080
let index = ref "index.html"
let verbose = ref false
let ssl_cert = ref None
let ssl_key = ref None
let rest = ref []

let usage = "usage " ^ Sys.argv.(0) ^ " [DOCROOT]"

let arglist = [
  ("-p", Arg.Int (fun i -> port := i), ": TCP port to listen on");
  ("-s", Arg.String (fun s -> host := s), ": IP address to listen on");
  ("-i", Arg.String (fun s -> index := s), ": Name of index file in directory");
  ("-v", Arg.Bool (fun b -> verbose := b), ": logging output to console");
  ("-c", Arg.String (fun s -> ssl_cert := Some s), ": certificate file");
  ("-k", Arg.String (fun s -> ssl_key := Some s), ": key file");
]

let _ =
  try Arg.parse arglist (fun x -> rest := x :: !rest) usage;
    begin match !rest with
    | [] -> Lwt_unix.run (start_server "." !port !host !index !verbose !ssl_cert !ssl_key ())
    | dir::_ -> Lwt_unix.run (start_server dir !port !host !index !verbose !ssl_cert !ssl_key ())
    end
  with
  | Failure s -> print_endline s
  | Sys_error s -> print_endline s

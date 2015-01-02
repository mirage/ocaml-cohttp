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
      then Server.respond_redirect ~uri:(Uri.with_path uri (path^"/")) ()
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

let lwt_start_server docroot port host index verbose cert key =
  Lwt_main.run (start_server docroot port host index verbose cert key ())

open Cmdliner

let host = 
  let doc = "IP address to listen on." in
  Arg.(value & opt string "0.0.0.0" & info ["s"] ~docv:"HOST" ~doc)

let port =
  let doc = "TCP port to listen on." in
  Arg.(value & opt int 8080 & info ["p"] ~docv:"PORT" ~doc)

let index =
  let doc = "Name of index file in directory." in
  Arg.(value & opt string "index.html" & info ["i"] ~docv:"INDEX" ~doc)

let verb =
  let doc = "Logging output to console." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let ssl_cert =
  let doc = "SSL certificate file." in
  Arg.(value & opt (some string) None & info ["c"] ~docv:"SSL_CERT" ~doc)

let ssl_key =
  let doc = "SSL key file." in
  Arg.(value & opt (some string) None & info ["k"] ~docv:"SSL_KEY" ~doc)

let doc_root = 
  let doc = "Serving directory." in
  Arg.(value & pos 0 dir "." & info [] ~docv:"DOCROOT" ~doc)

let cmd =
  let doc = "a simple http server" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) sets up a simple http server with lwt as backend";
    `S "BUGS";
    `P "Report them to via e-mail to <mirageos-devel@lists.xenproject.org>, or
        on the issue tracker at <https://github.com/mirage/ocaml-cohttp/issues>";
  ] in
  Term.(pure lwt_start_server $ doc_root $ port $ host $ index $ verb $ ssl_cert $ ssl_key),
  Term.info "cohttp-server" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

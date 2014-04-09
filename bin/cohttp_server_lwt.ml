open Lwt
open Cohttp
open Cohttp_lwt_unix

let serve_file ~docroot ~uri =
  Server.resolve_local_file ~docroot ~uri
  |> (fun x -> Server.respond_file ~fname:x ())

let ls_dir dir =
  Lwt_unix.files_of_directory dir
  |> Lwt_stream.to_list

let rec handler ~info ~docroot ~verbose ~index sock req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  (* Get a canonical filename from the URL and docroot *)
  let file_name = Server.resolve_local_file ~docroot ~uri in
  Lwt_unix.stat file_name
  >>= fun stat ->
  (* Log the request to the console *)
  Printf.printf "%s %s %s"
    (Cohttp.(Code.string_of_method (Request.meth req)))
    path
    (match verbose with
     | true -> ""
     | false -> ""
    );
  match stat.Unix.st_kind with
  | Unix.S_DIR -> begin
      match Sys.file_exists (Filename.concat file_name index) with
      | true -> let uri = Uri.with_path uri (Filename.concat path index) in
        Server.respond_redirect uri ()
      | false ->
        ls_dir file_name
        >>= Lwt_list.map_s (fun f ->
            let file_name = Filename.concat file_name f in
            Lwt.try_bind
              (fun () -> Lwt_unix.stat file_name)
              (fun stat ->
                 let li l = Printf.sprintf "<li><a href=\"%s\">%s</a>/</li>" (Uri.to_string l) in
                 let link = Uri.with_path uri (Filename.concat path f) in
                 match stat.Unix.st_kind with
                 | Unix.S_DIR -> Lwt.return (li link (Printf.sprintf "<i>%s/</i>" f))
                 | Unix.S_REG -> Lwt.return (li link f)
                 | _ -> Lwt.return (Printf.sprintf "<s>%s</s>" f))
              (fun exn ->
                 Lwt.return (Printf.sprintf "<li>Error with file: %s</li>" file_name)))
        >>= fun html ->
        String.concat "\n" html
        |> fun contents ->
        Printf.sprintf "
              <html>
                <body>
                <h2>Directory Listing for %s</h2>
                <ul>%s</ul>
                <hr>%s
                </body>
              </html>"
          file_name contents info
        |> (fun x -> Server.respond_string ~status:`OK ~body:x ())
    end
  | Unix.S_REG -> serve_file ~docroot ~uri
  | _ ->
    Server.respond_string ~status:`Forbidden
      ~body:"<html><body><h2>Forbidden</h2>
        <p>This is not a normal file or directory</p></body>/html>"
      ()

let start_server docroot port host index verbose () =
  Printf.printf "Listening for HTTP request on: %s %d\n" host port;
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
    if List.length !rest = 0
    then Lwt_unix.run (start_server "." !port !host !index !verbose ())
    else Lwt_unix.run (start_server (List.hd !rest) !port !host !index !verbose ())
  with
  | Failure s -> print_endline s
  | Sys_error s -> print_endline s

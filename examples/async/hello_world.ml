(* This file is in the public domain *)

(* open Core.Std *)
(* open Async.Std *)
(* open Cohttp_async *)
 
(* given filename: hello_world.ml compile with: 
   $ corebuild hello_world.native -pkg cohttp.async
*)

let (r,w) = Async.Std.Pipe.create ();;

let handler ~body:body _sock req =
  let uri = Cohttp.Request.uri req in                             (* a *)
  let path = Cohttp_async.Server.resolve_local_file "/home/jon/Auchitect/frontend/" uri in
  let meth = Cohttp.Request.meth req in
  match meth with
   | `GET  -> (* Async.Std.print_string "GET request received\n"; *)
              Cohttp_async.Server.respond_with_file path
   | `POST -> let deff = Async.Std.Deferred.bind (Cohttp_async.Body.to_string body)
                (fun text -> Async.Std.Pipe.write w text) in
              let bnmjib = Async.Std.print_string (
                  match (Async.Std.Deferred.peek deff) with
                   | None -> "None"
                   | Some a -> "Something") in
              "POST request received" |> Cohttp_async.Server.respond_with_string
   | _     -> "WHAT?? request received!??" |> Cohttp_async.Server.respond_with_string

(*  match Uri.path uri with                                       (* b *)
  | "/test" ->
       Uri.get_query_param uri "hello"
    |> Core.Std.Option.map ~f:(fun v -> "hello: " ^ v)            (* c *)
    |> Core.Std.Option.value ~default:"No param hello supplied"
    |> Cohttp_async.Server.respond_with_string                    (* d *)
  | _ ->
    Cohttp_async.Server.respond_with_string ~code:`Not_found "Route not found" *)
 
let start_server port () =
  Async.Std.eprintf "Listening for HTTP on port %d\n" port;       (* e *)
  Async.Std.eprintf "Try 'curl http://localhost:%d/auchitect.html'\n%!" port;
  let open Async.Std.Deferred.Monad_infix in (* (>>=) = Async.Std.Deferred.(>>=) in *)                         (* f *)
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Async.Std.Tcp.on_port port) handler                          (* g *)
  >>= fun _ -> Async.Std.Deferred.never ()

let () =
  Async.Std.Command.async_basic                                   (* h *)
    ~summary:"Start a hello world Async server"
    Async.Std.Command.Spec.(empty +>                              (* i *)
      flag "-p" (optional_with_default 8080 int)                  (* j *)
        ~doc:"int Source port to listen on"
    ) start_server
    
  |> Async.Std.Command.run;                                       (* k *)
  Async.Std.print_string "server running\n";
  Async.Std.print_string (match (Async.Std.Pipe.read r) with
                           | `Eof -> "an empty string"
                           | `Ok text -> text)

(*  a   ocaml-cohttp/cohttp/request.mli ->
        ocaml-cohttp/cohttp/s.mli  *)

(*  b   ???  *)

(*  c   core_kernel/lib/option.mli ->
        core_kernel/lib/monad.ml  *)

(*  d   ocaml-cohttp/async/cohttp_async.mli  *)

(*  e   async_unix/lib/std.ml ->
        ???  *)

(*  f   ???  *)

(*  g   ???  *)

(*  h   ???  *)

(*  i   ???  *)

(*  j   ???  *)

(*  k   ???  *)
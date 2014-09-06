(* This file is in the public domain *)

(* open Core.Std *)
(* open Async.Std *)
(* open Cohttp_async *)
 
(* given filename: hello_world.ml compile with: 
   $ corebuild hello_world.native -pkg cohttp.async
*)
 
let handler ~body:_ _sock req =
  let uri = Cohttp.Request.uri req in  (* a *)
  match Uri.path uri with              (* b *)
  | "/test" ->
       Uri.get_query_param uri "hello"
    |> Core.Std.Option.map ~f:(fun v -> "hello: " ^ v)            (* c *)
    |> Core.Std.Option.value ~default:"No param hello supplied"
    |> Cohttp_async.Server.respond_with_string                    (* d *)
  | _ ->
    Cohttp_async.Server.respond_with_string ~code:`Not_found "Route not found"
 
let start_server port () =
  Async.Std.eprintf "Listening for HTTP on port %d\n" port;       (* e *)
  Async.Std.eprintf "Try 'curl http://localhost:%d/test?hello=xyz'\n%!" port;
  let (>>=) = Async.Std.Deferred.(>>=) in                         (* f *)
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
    
  |> Async.Std.Command.run                                        (* k *)


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
(* This file is in the public domain *)
open Core
open Async
open Cohttp_async

(* compile with: $ corebuild receive_post.native -pkg cohttp.async *)

let start_server port () =
  eprintf "Listening for HTTP on port %d\n" port;
  eprintf "Try 'curl -X POST -d 'foo bar' http://localhost:%d\n" port;
  Cohttp_async.Server.create ~on_handler_error:`Raise
    (Tcp.on_port port) (fun ~body _ req ->
      match req |> Cohttp.Request.meth with
      | `POST ->
        (Body.to_string body) >>= (fun body ->
          Log.Global.info "Body: %s" body;
          Server.respond `OK)
      | _ -> Server.respond `Method_not_allowed
    )
  >>= fun _ -> Deferred.never ()

let () =
  Command.async
    ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on"
                 ) start_server
  |> Command.run

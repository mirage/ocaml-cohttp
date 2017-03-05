open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

(* In this example, the [callback] starts a bunch of concurrent threads. As each
   thread completes, its result is streamed to the client. If the client closes
   the connection during the stream, the call to [write] will raise an
   exception. We use that exception to cancel the remaining threads, avoiding
   unnecessary work.
*)

let server =
  let callback _conn req body =
    let body =
      `CustomStream (fun write ->
          let do_work i seconds =
            Lwt_unix.sleep seconds >>= fun () ->
            Lwt_log.notice_f "Writing result %i after %.2f seconds"
              i seconds >>= fun () ->

            Lwt.catch
              (fun () ->
                 write (Printf.sprintf "Result %i\n" i)
              )
              (fun exn ->
                 Lwt_log.error ~exn
                   (Printf.sprintf
                      "Exception while writing result %i" i) >>= fun () ->
                 Lwt.fail exn
              )
          in

          let join_or_cancel ts =
            let () =
              List.iter (fun t ->
                  Lwt.on_failure t (fun exn -> List.iter Lwt.cancel ts)
                ) ts
            in
            Lwt.join ts
          in

          [ 0.5 ; 2.0 ; 1.0 ; 1.5 ]
          |> List.mapi do_work
          |> join_or_cancel
        )
    in
    Server.respond ~status:`OK ~body ()
  in
  Server.create
    ~mode:(`TCP (`Port 8000))
    ~on_exn:(fun exn -> Lwt_log.ign_error ~exn "Uncaught exception")
    (Server.make ~callback ())

let () = ignore (Lwt_main.run server)

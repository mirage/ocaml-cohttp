open Lwt.Infix
module Header = Cohttp.Header

module Make (IO : S.IO) = struct
  module IO = IO
  module Request = Make.Request (IO)
  module Response = Make.Response (IO)

  let src = Logs.Src.create "cohttp.lwt.server" ~doc:"Cohttp Lwt server module"

  module Log = (val Logs.src_log src : Logs.LOG)

  type conn = IO.conn * Cohttp.Connection.t

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Body.t ]

  type t = {
    callback : conn -> Cohttp.Request.t -> Body.t -> response_action Lwt.t;
    conn_closed : conn -> unit;
    sleep_fn : (unit -> unit Lwt.t) option;
  }

  let make_response_action ?(conn_closed = ignore) ?sleep_fn ~callback () =
    { conn_closed; callback; sleep_fn }

  let make ?conn_closed ?sleep_fn ~callback () =
    let callback conn req body =
      callback conn req body >|= fun rsp -> `Response rsp
    in
    make_response_action ?conn_closed ?sleep_fn ~callback ()

  let make_expert ?conn_closed ?sleep_fn ~callback () =
    let callback conn req body =
      callback conn req body >|= fun rsp -> `Expert rsp
    in
    make_response_action ?conn_closed ?sleep_fn ~callback ()

  module Transfer_IO = Cohttp__Transfer_io.Make (IO)

  (* Deprecated *)
  let resolve_local_file ~docroot ~uri =
    Cohttp.Path.resolve_local_file ~docroot ~uri

  let respond ?headers ?(flush = true) ~status ~body () =
    let encoding =
      match headers with
      | None -> Body.transfer_encoding body
      | Some headers -> (
          match Header.get_transfer_encoding headers with
          | Cohttp.Transfer.Unknown -> Body.transfer_encoding body
          | t -> t)
    in
    let res = Response.make ~status ~flush ~encoding ?headers () in
    Lwt.return (res, body)

  let respond_string ?(flush = true) ?headers ~status ~body () =
    let res =
      Response.make ~status ~flush
        ~encoding:(Cohttp.Transfer.Fixed (Int64.of_int (String.length body)))
        ?headers ()
    in
    let body = Body.of_string body in
    Lwt.return (res, body)

  let respond_error ?headers ?(status = `Internal_server_error) ~body () =
    respond_string ?headers ~status ~body:("Error: " ^ body) ()

  let respond_redirect ?headers ~uri () =
    let headers =
      match headers with
      | None -> Header.init_with "location" (Uri.to_string uri)
      | Some h -> Header.add_unless_exists h "location" (Uri.to_string uri)
    in
    respond ~headers ~status:`Found ~body:`Empty ()

  let respond_need_auth ?headers ~auth () =
    let headers = match headers with None -> Header.init () | Some h -> h in
    let headers = Header.add_authorization_req headers auth in
    respond ~headers ~status:`Unauthorized ~body:`Empty ()

  let respond_not_found ?uri () =
    let body =
      match uri with
      | None -> "Not found"
      | Some uri -> "Not found: " ^ Uri.to_string uri
    in
    respond_string ~status:`Not_found ~body ()

  let read_body ic req =
    match Request.has_body req with
    | `Yes ->
        let reader = Request.make_body_reader req ic in
        let body_stream = Body.create_stream Request.read_body_chunk reader in
        Body.of_stream body_stream
    | `No | `Unknown -> `Empty

  let handle_request callback conn req body =
    Log.debug (fun m -> m "Handle request: %a." Request.pp_hum req);
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () -> callback conn req body)
          (function
            | Out_of_memory -> Lwt.fail Out_of_memory
            | exn ->
                Log.err (fun f ->
                    f "Error handling %a: %s" Request.pp_hum req
                      (Printexc.to_string exn));
                respond_error ~body:"Internal Server Error" () >|= fun rsp ->
                `Response rsp))
      (fun () -> Body.drain_body body)

  type conn_action = Call_conn_closed | Call_conn_closed_and_drain of Body.t

  let handle_response ~keep_alive oc res body handle_client =
    IO.catch (fun () ->
        let flush = Response.flush res in
        Response.write ~flush
          (fun writer -> Body.write_body (Response.write_body writer) body)
          res oc)
    >>= function
    | Ok () ->
        if keep_alive then handle_client oc else Lwt.return Call_conn_closed
    | Error e ->
        Log.info (fun m -> m "IO error while writing body: %a" IO.pp_error e);
        Lwt.return (Call_conn_closed_and_drain body)

  let rec handle_client ic oc conn spec =
    Request.read ic >>= function
    | `Eof ->
        Log.debug (fun m ->
            m "Got EOF while handling client: %s"
              (Cohttp.Connection.to_string (snd conn)));
        Lwt.return Call_conn_closed
    | `Invalid data ->
        Log.err (fun m -> m "invalid input %s while handling client" data);
        Lwt.return Call_conn_closed
    | `Ok req -> (
        let body = read_body ic req in
        handle_request spec.callback conn req body >>= function
        | `Response (res, body) ->
            let keep_alive = Request.is_keep_alive req in
            handle_response ~keep_alive oc res body (fun oc ->
                handle_client ic oc conn spec)
        | `Expert (res, io_handler) ->
            Response.write_header res oc >>= fun () ->
            io_handler ic oc >>= fun () -> handle_client ic oc conn spec)

  let callback spec io_id ic oc =
    let conn_id = Cohttp.Connection.create () in
    let conn_closed () = spec.conn_closed (io_id, conn_id) in
    let handle () = handle_client ic oc (io_id, conn_id) spec in
    let is_conn_closed () =
      (* Without a sleep function we cannot safely loop waiting for EOF *)
      match spec.sleep_fn with
      | None -> fst (Lwt.task ()) (* wait to be cancelled *)
      | Some sleep_fn ->
          IO.wait_eof_or_closed io_id ic sleep_fn >>= fun () ->
          Log.debug (fun m ->
              m "Client closed the connection, got EOF for %s"
                (Cohttp.Connection.to_string conn_id));
          Lwt.return Call_conn_closed
    in
    Lwt.catch
      (fun () ->
        IO.catch (fun () ->
            Lwt.pick [ handle (); is_conn_closed () ] >>= function
            | Call_conn_closed ->
                conn_closed ();
                Lwt.return_unit
            | Call_conn_closed_and_drain body ->
                conn_closed ();
                Body.drain_body body)
        >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
            Log.info (fun m ->
                m "IO error while handling client: %a" IO.pp_error e);
            conn_closed ();
            Lwt.return_unit)
      (fun e ->
        conn_closed ();
        Lwt.fail e)
end

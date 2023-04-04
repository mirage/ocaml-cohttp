open Lwt.Infix
module Header = Cohttp.Header
module Connection = Cohttp.Connection [@@warning "-3"]

module Make (IO : S.IO) = struct
  module IO = IO
  module Request = Make.Request (IO)
  module Response = Make.Response (IO)

  let src = Logs.Src.create "cohttp.lwt.server" ~doc:"Cohttp Lwt server module"

  module Log = (val Logs.src_log src : Logs.LOG)

  type conn = IO.conn * Connection.t

  type response_action =
    [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Http.Response.t * Body.t ]

  type t = {
    callback : conn -> Http.Request.t -> Body.t -> response_action Lwt.t;
    conn_closed : conn -> unit;
  }

  let make_response_action ?(conn_closed = ignore) ~callback () =
    { conn_closed; callback }

  let make ?conn_closed ~callback () =
    let callback conn req body =
      callback conn req body >|= fun rsp -> `Response rsp
    in
    make_response_action ?conn_closed ~callback ()

  let make_expert ?conn_closed ~callback () =
    let callback conn req body =
      callback conn req body >|= fun rsp -> `Expert rsp
    in
    make_response_action ?conn_closed ~callback ()

  module Transfer_IO = Cohttp.Private.Transfer_io.Make (IO)

  let resolve_local_file ~docroot ~uri =
    Cohttp.Path.resolve_local_file ~docroot ~uri

  let respond ?headers ?(flush = true) ~status ~body () =
    let encoding =
      match headers with
      | None -> Body.transfer_encoding body
      | Some headers -> (
          match Header.get_transfer_encoding headers with
          | Http.Transfer.Unknown -> Body.transfer_encoding body
          | t -> t)
    in
    let res = Response.make ~status ~flush ~encoding ?headers () in
    Lwt.return (res, body)

  let respond_string ?(flush = true) ?headers ~status ~body () =
    let res =
      Response.make ~status ~flush
        ~encoding:(Http.Transfer.Fixed (Int64.of_int (String.length body)))
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

  let read_body ?send_100_continue_if_needed ic req =
    match Http.Request.has_body req with
    | `Yes ->
        let reader = Request.make_body_reader req ic in
        let body_stream = Body.create_stream Request.read_body_chunk reader in
        let body_stream = match send_100_continue_if_needed with
        | None -> body_stream
        | Some f ->
            (* See RFC7231 5.1.1 Expect
              We MUST send a 100-continue and we MUST NOT wait for the body
              before doing that.
             *)
            let start = Lwt_stream.from @@ fun () ->
              f () >>= fun () ->
              Lwt.return_none
            in
            Lwt_stream.append start body_stream
        in
        Body.of_stream body_stream
    | `No | `Unknown ->
        (* See RFC7231 5.1.1 Expect
           If we know there is no message body we MAY send a 100 Continue, but
           don't have to *)
        `Empty

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

  let res_100_continue = Response.make ~status:`Continue ~flush:true ()

  let send_100_continue oc =
    Some (fun () -> Response.write_header res_100_continue oc)

  let rec handle_client ic oc conn callback =
    Request.read ic >>= function
    | `Eof -> Lwt.return_unit
    | `Invalid data ->
        Log.err (fun m -> m "invalid input %s while handling client" data);
        Lwt.return_unit
    | `Ok req -> (
        let send_100_continue_if_needed =
          (* See RFC7231 5.1.1 Expect
             We MAY respond with 417 on Expect values other than 100-continue,
             but we don't have to, so we can leave that up to the application
             and handle only 100-continue here.

             We do need to check the HTTP version, because we can't respond
             with 100-continue on HTTP/1.0 (and I assume not on 0.9 either?).
           *)
          if Request.version req = `HTTP_1_1
             && Header.get (Request.headers req) "Expect" = Some "100-continue"
          then send_100_continue oc
          else None
        in
        let body = read_body ?send_100_continue_if_needed ic req in
        handle_request callback conn req body >>= function
        | `Response (res, body) ->
            let flush = Response.flush res in
            Response.write ~flush
              (fun writer -> Body.write_body (Response.write_body writer) body)
              res oc
            >>= fun () ->
            if Http.Request.is_keep_alive req && Http.Response.is_keep_alive res
            then handle_client ic oc conn callback
            else Lwt.return_unit
        | `Expert (res, io_handler) ->
            Response.write_header res oc >>= fun () ->
            io_handler ic oc >>= fun () -> handle_client ic oc conn callback)

  let callback spec io_id ic oc =
    let conn_id = Connection.create () in
    let conn_closed () = spec.conn_closed (io_id, conn_id) in
    Lwt.finalize
      (fun () ->
        IO.catch (fun () -> handle_client ic oc (io_id, conn_id) spec.callback)
        >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
            Log.info (fun m ->
                m "IO error while handling client: %a" IO.pp_error e);
            Lwt.return_unit)
      (fun () ->
        (* Clean up resources when the response stream terminates and call
         * the user callback *)
        conn_closed () |> Lwt.return)
end

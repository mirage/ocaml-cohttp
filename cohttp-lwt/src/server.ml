open Lwt.Infix

module Header = Cohttp.Header

module Make(IO:S.IO) = struct
  module IO = IO
  module Request = Make.Request(IO)
  module Response = Make.Response(IO)

  type conn = IO.conn * Cohttp.Connection.t

  type t = {
    callback :
      conn ->
      Cohttp.Request.t ->
      Body.t ->
      (Cohttp.Response.t * Body.t) Lwt.t;
    conn_closed: conn -> unit;
  }

  let make ?(conn_closed=ignore) ~callback () =
    { conn_closed ; callback }

  module Transfer_IO = Cohttp__Transfer_io.Make(IO)

  let resolve_local_file ~docroot ~uri =
    let path = Uri.(pct_decode (path (resolve "http" (of_string "/") uri))) in
    let rel_path = String.sub path 1 (String.length path - 1) in
    Filename.concat docroot rel_path

  let respond ?headers ?(flush=true) ~status ~body () =
    let encoding =
      match headers with
      | None -> Body.transfer_encoding body
      | Some headers ->
        match Header.get_transfer_encoding headers with
        | Cohttp.Transfer.Unknown -> Body.transfer_encoding body
        | t -> t
    in
    let res = Response.make ~status ~flush ~encoding ?headers () in
    Lwt.return (res, body)

  let respond_string ?(flush=true) ?headers ~status ~body () =
    let res = Response.make ~status ~flush
        ~encoding:(Cohttp.Transfer.Fixed
                     (Int64.of_int (String.length body)))
        ?headers () in
    let body = Body.of_string body in
    Lwt.return (res,body)

  let respond_error ?headers ?(status=`Internal_server_error) ~body () =
    respond_string ?headers ~status ~body:("Error: "^body) ()

  let respond_redirect ?headers ~uri () =
    let headers =
      match headers with
      |None -> Header.init_with "location" (Uri.to_string uri)
      |Some h -> Header.add_unless_exists h "location" (Uri.to_string uri)
    in
    respond ~headers ~status:`Found ~body:`Empty ()

  let respond_need_auth ?headers ~auth () =
    let headers = match headers with |None -> Header.init () |Some h -> h in
    let headers = Header.add_authorization_req headers auth in
    respond ~headers ~status:`Unauthorized ~body:`Empty ()

  let respond_not_found ?uri () =
    let body = match uri with
      |None -> "Not found"
      |Some uri -> "Not found: " ^ (Uri.to_string uri) in
    respond_string ~status:`Not_found ~body ()

  let request_stream ic =
    (* don't try to read more from ic until the previous request has
       been fully read an released this mutex *)
    let read_m = Lwt_mutex.create () in
    (* If the request is HTTP version 1.0 then the request stream should be
       considered closed after the first request/response. *)
    let early_close = ref false in
    Lwt_stream.from begin fun () ->
      if !early_close
      then Lwt.return_none
      else
        Lwt_mutex.lock read_m >>= fun () ->
        Request.read ic >>= function
        | `Eof | `Invalid _ -> (* TODO: request logger for invalid req *)
          Lwt_mutex.unlock read_m;
          Lwt.return_none
        | `Ok req -> begin
            early_close := not (Request.is_keep_alive req);
            (* Ensure the input body has been fully read before reading
               again *)
            match Request.has_body req with
            | `Yes ->
              let reader = Request.make_body_reader req ic in
              let body_stream = Body.create_stream
                                  Request.read_body_chunk reader in
              Lwt_stream.on_terminate body_stream
                (fun () -> Lwt_mutex.unlock read_m);
              let body = Body.of_stream body_stream in
              (* The read_m remains locked until the caller reads the body *)
              Lwt.return (Some (req, body))
            (* TODO for now we are just repeating the old behaviour
             * of ignoring the body in the request. Perhaps it should be
             * changed it did for responses *)
            | `No | `Unknown ->
              Lwt_mutex.unlock read_m;
              Lwt.return (Some (req, `Empty))
          end
    end

  let response_stream callback io_id conn_id req_stream =
    Lwt_stream.map_s (fun (req, body) ->
      Lwt.finalize
        (fun () ->
           Lwt.catch
             (fun () -> callback (io_id, conn_id) req body)
             (fun exn ->
               Format.eprintf "Error handling %a: %s\n%!" Request.pp_hum req (Printexc.to_string exn);
               respond_error ~body:"Internal Server Error" ()))
        (fun () -> Body.drain_body body)
    ) req_stream

  let callback spec io_id ic oc =
    let conn_id = Cohttp.Connection.create () in
    let conn_closed () = spec.conn_closed (io_id,conn_id) in
    (* The server operates by reading requests into a Lwt_stream of requests
       and mapping them into a stream of responses serially using [spec]. The
       responses are then sent over the wire *)
    let req_stream = request_stream ic in
    let res_stream = response_stream spec.callback io_id conn_id req_stream in
    Lwt.finalize
      (fun () ->
         (* Transmit the responses *)
         res_stream |> Lwt_stream.iter_s (fun (res,body) ->
             let flush = Response.flush res in
             Response.write ~flush (fun writer ->
                 Body.write_body (Response.write_body writer) body
               ) res oc
           )
      )
      (fun () ->
         (* Clean up resources when the response stream terminates and call
          * the user callback *)
         conn_closed () |> Lwt.return
      )
end

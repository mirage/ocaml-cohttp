(** Raised on failed requests that may be safely retried - even on
    non-idempotent requests. Raised for example on timeout or
    connection suhtdown by remote end. *)
exception Retry

module Make (Net : S.Net) : (S.Connection with module Net = Net) =
struct
  module Net = Net
  module IO = Net.IO
  module Response = Make.Response (IO)
  module Request = Make.Request (IO)
  module Header = Cohttp.Header

  open IO

  let src = Logs.Src.create "cohttp.lwt.client" ~doc:"Cohttp Lwt client"

  module Log = (val Logs.src_log src : Logs.LOG)

  exception Retry = Retry

  type state =
    | Connecting of (IO.ic * IO.oc) Lwt.t
    (* Waiting for the TCP handshake / TLS connection setup *)
    | Full of (IO.ic * IO.oc)
    (* "full-duplex". May send requests, may be waiting for responses / EOF. *)
    | Closing of (IO.ic * IO.oc)
    (* still in "full-duplex", but no new requests may be queued.
     * Will shutdown oc as soon as the last request went out. *)
    | Half of IO.ic
    (* oc has been closed, waiting for outstanding responses on ic. *)
    | Closed
    | Failed of exn
  type req_resr =
    { req :Request.t
    ; body :Body.t
    ; res_r :(Response.t * Body.t) Lwt.u }
  type persistent = [ `True | `False | `Unknown ]
  type t =
    { mutable state :state
    ; mutable persistent :persistent
    ; in_flight :req_resr Queue.t
    ; waiting :req_resr Queue.t
    ; condition :unit Lwt_condition.t (* watching queues *)
    ; finalise :(t -> unit Lwt.t)
    }

  let length connection =
    Queue.length connection.in_flight +
    Queue.length connection.waiting

  let notify connection =
    Lwt_condition.wait connection.condition

  let queue_fail connection q e =
    Queue.iter (fun {res_r; _} -> Lwt.wakeup_later_exn res_r e) q;
    Queue.clear q;
    Lwt_condition.broadcast_exn connection.condition e

  let close connection =
    match connection.state with
    | Connecting channels ->
      connection.state <- Closed;
      Lwt.cancel channels;
      Lwt.on_success channels (fun (ic, oc) -> Net.close ic oc);
      Lwt_condition.broadcast connection.condition ()
    | Closing (ic, oc)
    | Full (ic, oc) ->
      Net.close ic oc;
      connection.state <- Closed;
      Lwt_condition.broadcast connection.condition ()
    | Half ic ->
      Net.close_in ic;
      connection.state <- Closed;
      Lwt_condition.broadcast connection.condition ()
    | Closed | Failed _ -> ()

  let shutdown connection =
    match connection.state with
    | Connecting channels ->
      Lwt.async @@ fun () ->
      channels >>= fun channels ->
      connection.state <- Closing channels;
      Lwt.return_unit
    | Full channels ->
      connection.state <- Closing channels;
    | Closing _ | Half _ | Closed | Failed _ -> ()

  let is_closed connection =
    match connection.state with
    | Full _ -> false
    | Connecting _ -> false
    | Closing _ | Half _ -> true
    | Closed | Failed _ -> true

  let rec reader connection =
    match connection.state with
    | Connecting _ -> assert false
    | Closed | Failed _ -> assert false
    | Full (ic, _) | Closing (ic, _) | Half ic ->
      Response.read ic >>= fun res ->
      match res with
      | `Ok res ->
        if
          connection.persistent = `Unknown &&
          Response.version res = `HTTP_1_1 &&
          not (Header.mem (Response.headers res) "Connection")
        then connection.persistent <- `True;
        (* don't take from queue yet, because body may still be in flight *)
        let {req; res_r; _} = Queue.peek connection.in_flight in

        (* A response header to a HEAD request is indistinguishable from a
         * response header to a GET request. Therefore look at the request. *)
        if
          match Response.has_body res with
          | _ when Request.meth req = `HEAD -> false
          | `No -> false
          | `Yes | `Unknown -> true
        then begin
          let stream =
            Body.create_stream
              Response.read_body_chunk
              (Response.make_body_reader res ic)
          in
          (* finalise could run in a thread different from the lwt main thread.
           * You may therefore not call into Lwt from a finaliser. *)
          let closed = ref false in
          Gc.finalise_last
            begin fun () ->
              if not !closed then
                Log.warn (fun m ->
                    m
                      "Body not consumed, leaking stream! Refer to \
                       https://github.com/mirage/ocaml-cohttp/issues/730 for \
                       additional details")
            end
            stream;
          Lwt.wakeup_later res_r (res, Body.of_stream stream);
          Lwt_stream.closed stream >>= fun () ->
          closed := true;
          Queue.take connection.in_flight |> ignore;
          Lwt_condition.broadcast connection.condition ();
          reader connection
        end
        else begin
          Queue.take connection.in_flight |> ignore;
          Lwt_condition.broadcast connection.condition ();
          Lwt.wakeup_later res_r (res, `Empty);
          reader connection;
        end
      | `Eof ->
        begin match connection.state with
        | Full (_, oc) | Closing (_, oc) ->
          Net.close_out oc;
          connection.state <- Closed
        | Half _ ->
          connection.state <- Closed
        (* Failed is no special case since we read proper EOF. *)
        | Closed | Failed _ -> ()
        | Connecting _ -> assert false
        end;
        connection.finalise connection >>= fun () ->
        queue_fail connection connection.in_flight Retry;
        Lwt.return_unit
      | `Invalid reason ->
        let e = Failure ("Cohttp_lwt failed to read response: " ^ reason) in
        connection.state <- Failed e;
        connection.finalise connection >>= fun () ->
        queue_fail connection connection.in_flight e;
        Lwt.return_unit

  let request connection ?(body = `Empty) req =
    match connection.state with
    | Connecting _ | Full _ ->
      let res, res_r = Lwt.wait () in
      Queue.push {req; body; res_r} connection.waiting;
      Lwt_condition.broadcast connection.condition ();
      res
    | Closing _ | Half _ | Closed | Failed _ -> Lwt.fail Retry

  let rec writer connection =
    match connection.state with
    | Full _
      when Queue.is_empty connection.waiting
        || not (Queue.is_empty connection.in_flight
                || connection.persistent = `True)
      ->
      Lwt.try_bind (fun () -> Lwt_condition.wait connection.condition)
        (fun _ -> writer connection)
        (fun _ -> writer connection)
    | Closing (ic, oc) when Queue.is_empty connection.waiting ->
      connection.state <- Half ic;
      Net.close_out oc;
      Lwt.return_unit
    | Full (ic, oc)
    | Closing (ic, oc) ->
      let {req; body; res_r; _} as work = Queue.take connection.waiting in
      let uri, meth, version, headers =
        Request.(uri req, meth req, version req, headers req) in

      (* select encoding based on header, request, body and make sure
       * the encoding in the request matches the encoding in the header. *)
      begin match Header.get_transfer_encoding (Request.headers req) with
        | Unknown ->
          begin match Request.encoding req with
          | Unknown ->
            begin match Body.transfer_encoding body with
            | Fixed _ as e -> Lwt.return (e, body)
            | Chunked as e when connection.persistent = `True ->
              Lwt.return (e, body)
            | Chunked (* connection.persistent <> `True *) ->
              (* We don't know yet whether chunked encoding is supported.
               * Therefore use fixed length encoding. *)
              Body.length body >>= fun (length, body) ->
              Lwt.return (Cohttp.Transfer.Fixed length, body)
            | Unknown -> assert false
            end
          | e -> Lwt.return (e, body)
          end
        | e -> Lwt.return (e, body)
      end >>= fun (encoding, body) ->

      let headers =
        if connection.persistent = `False
        then Header.add_unless_exists headers "Connection" "close"
        else headers
      in

      let req = Request.make ~meth ~version ~encoding ~headers uri in
      Queue.push { work with req } connection.in_flight;
      Lwt.catch
        begin fun () -> (* try *)
          Request.write (fun writer ->
              Body.write_body (Request.write_body writer) body
            ) req oc
        end
        begin fun e -> (* with *)
          (try Net.close_out oc with _ -> ());
          connection.state <- Half ic;
          Lwt.wakeup_later_exn res_r e;
          queue_fail connection connection.waiting Retry;
          Lwt.return_unit
        end
      >>= fun () ->
      writer connection
    | Failed e ->
      queue_fail connection connection.waiting e;
      Lwt.return_unit
    | Half _ | Closed -> Lwt.return_unit
    | Connecting _ -> assert false

  let create
      ?(finalise=fun _ -> Lwt.return_unit)
      ?persistent
      ?(ctx=Net.default_ctx)
      endp
    =
    let persistent = match persistent with
      | None -> `Unknown
      | Some true -> `True
      | Some false -> `False
    in
    let channels =
      Net.connect_endp ~ctx endp >>= fun (_, ic, oc) ->
      return (ic, oc)
    in
    let connection =
      { finalise
      ; in_flight = Queue.create ()
      ; waiting = Queue.create ()
      ; state = Connecting channels
      ; condition = Lwt_condition.create ()
      ; persistent
      }
    in
    Lwt.on_any channels
      begin fun channels ->
        connection.state <- Full channels;
        Lwt.async (fun () -> reader connection);
        Lwt.async (fun () -> writer connection);
      end
      begin fun e ->
        connection.state <- Failed e
      end;
    connection

  let connect ?finalise ?persistent ?ctx uri =
    let connection = create ?finalise ?persistent ?ctx uri in
    match connection.state with
    | Connecting channels ->
      channels >>= fun _ -> Lwt.return connection
    | _ -> Lwt.return connection
end

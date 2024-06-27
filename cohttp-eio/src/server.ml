open Utils
module IO = Io.IO

type body = Body.t
type conn = IO.conn * Cohttp.Connection.t [@@warning "-3"]
type writer = Http.Request.t * IO.oc
type response = writer -> unit

type response_action =
  [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit)
  | `Response of response ]

type t = {
  conn_closed : conn -> unit;
  handler : conn -> Http.Request.t -> body -> IO.ic -> IO.oc -> unit;
}

let make_response_action ?(conn_closed = fun _ -> ()) ~callback () =
  {
    conn_closed;
    handler =
      (fun conn request body ic oc ->
        match callback conn request body with
        | `Expert (response, handler) ->
            Io.Response.write_header response oc;
            handler ic oc
        | `Response fn -> fn (request, oc));
  }

let make_expert ?conn_closed ~callback () =
  make_response_action ?conn_closed
    ~callback:(fun conn request body ->
      let expert = callback conn request body in
      `Expert expert)
    ()

let make ?(conn_closed = fun _ -> ()) ~callback () =
  {
    conn_closed;
    handler =
      (fun conn request body _ic oc -> callback conn request body (request, oc));
  }

let read input =
  match Io.Request.read input with
  | (`Eof | `Invalid _) as e -> e
  | `Ok request -> (
      match Http.Request.has_body request with
      | `No -> `Ok (request, Eio.Flow.string_source "")
      | _ ->
          let body =
            let reader = Io.Request.make_body_reader request input in
            flow_of_reader (fun () -> Io.Request.read_body_chunk reader)
          in
          `Ok (request, body))

let write output (response : Cohttp.Response.t) body =
  let response =
    let content_length =
      let (Eio.Resource.T (body, ops)) = body in
      let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
      List.find_map
        (function
          | Body.String get -> Some (String.length (get body)) | _ -> None)
        X.read_methods
    in
    (* encoding field might be deprecated but it is still used
       to compute headers and encode the body*)
    match
      (Cohttp.Header.get_transfer_encoding response.headers, content_length)
    with
    | Unknown, None ->
        let headers =
          Cohttp.Header.add_transfer_encoding response.headers Chunked
        in
        { response with headers }
    | Unknown, Some size ->
        let headers =
          Cohttp.Header.add_transfer_encoding response.headers
            (Fixed (Int64.of_int size))
        in
        { response with headers }
    | _, _ -> response
  in
  let () = Logs.debug (fun m -> m "send headers") in
  let () =
    Io.Response.write ~flush:false
      (fun writer ->
        let () =
          Logs.debug (fun m ->
              m "send body (%a)" Cohttp.Transfer.pp_encoding
                (Cohttp.Header.get_transfer_encoding response.headers))
        in
        flow_to_writer body writer Io.Response.write_body)
      response output
  in
  Eio.Buf_write.flush output

let respond ?encoding ?(headers = Cohttp.Header.init ()) ~status ~body ()
    (request, oc) =
  let keep_alive = Http.Request.is_keep_alive request in
  let headers =
    match Cohttp.Header.connection headers with
    | Some _ -> headers
    | None ->
        Http.Header.add headers "connection"
          (if keep_alive then "keep-alive" else "close")
  in
  let response = Cohttp.Response.make ?encoding ~headers ~status () in
  write oc response body

let respond_string ?headers ~status ~body () =
  respond
    ~encoding:(Fixed (String.length body |> Int64.of_int))
    ?headers ~status ~body:(Body.of_string body) ()

let respond ?headers ~status ~body () response =
  respond ?encoding:None ?headers ~status ~body () response

let callback { conn_closed; handler } ((_, peer_address) as conn) input output =
  let id = (Cohttp.Connection.create () [@ocaml.warning "-3"]) in
  let rec handle () =
    match read input with
    | `Eof ->
        let () =
          Logs.info (fun m ->
              m "%a: disconnected" Eio.Net.Sockaddr.pp peer_address)
        in
        conn_closed (conn, id)
    | exception Eio.Io (Eio.Net.E (Connection_reset _), _) ->
        let () =
          Logs.info (fun m ->
              m "%a: connection reset" Eio.Net.Sockaddr.pp peer_address)
        in
        ()
    | `Invalid e ->
        write output
          (Http.Response.make ~status:`Bad_request ())
          (Body.of_string e)
    | `Ok (request, body) ->
        let () =
          try handler (conn, id) request body input output
          with Eio.Io (Eio.Net.E (Connection_reset _), _) ->
            Logs.info (fun m ->
                m "%a: connection reset" Eio.Net.Sockaddr.pp peer_address)
        in
        if Cohttp.Request.is_keep_alive request then handle ()
  in
  handle ()

let run ?max_connections ?additional_domains ?stop ~on_error socket server =
  Eio.Net.run_server socket ?max_connections ?additional_domains ?stop ~on_error
    (fun socket peer_address ->
      Eio.Switch.run @@ fun sw ->
      let () =
        Logs.info (fun m ->
            m "%a: accept connection" Eio.Net.Sockaddr.pp peer_address)
      and input = Eio.Buf_read.of_flow ~max_size:max_int socket in
      try
        Eio.Buf_write.with_flow socket @@ fun output ->
        callback server (sw, peer_address) input output
      with Eio.Io (Eio.Net.E (Connection_reset _), _) ->
        Logs.info (fun m ->
            m "%a: connection reset" Eio.Net.Sockaddr.pp peer_address))

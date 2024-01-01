open Utils
module IO = Io.IO

type body = Body.t
type conn = IO.conn * Cohttp.Connection.t [@@warning "-3"]

type response_action =
  [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit IO.t)
  | `Response of Http.Response.t * body ]

(* type handler =
 *   sw:Eio.Switch.t ->
 *   Eio.Net.Sockaddr.stream ->
 *   Http.Request.t ->
 *   Eio.Flow.source ->
 *   Http.Response.t * Eio.Flow.source *)

type t = {
  conn_closed : conn -> unit;
  handler : conn -> Http.Request.t -> body -> response_action IO.t;
}

let make_response_action ?(conn_closed = fun _ -> ()) ~callback () =
  { conn_closed; handler = callback }

let make_expert ?conn_closed ~callback () =
  make_response_action ?conn_closed
    ~callback:(fun conn request body ->
      IO.(callback conn request body >>= fun expert -> `Expert expert))
    ()

let make ?conn_closed ~callback () =
  make_response_action ?conn_closed
    ~callback:(fun conn request body ->
      IO.(callback conn request body >>= fun response -> `Response response))
    ()

let respond ?headers ?flush ~status ~body () =
  let response = Cohttp.Response.make ?headers ?flush ~status () in
  (response, body)

let respond_string ?headers ?flush ~status ~body () =
  respond ?headers ?flush ~status ~body:(Body.of_string body) ()

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
        { response with encoding = Chunked } [@ocaml.warning "-3"]
    | Unknown, Some size ->
        { response with encoding = Fixed (Int64.of_int size) }
        [@ocaml.warning "-3"]
    | from_headers, _ ->
        { response with encoding = from_headers } [@ocaml.warning "-3"]
  in
  let () = Logs.debug (fun m -> m "send headers") in
  let () =
    Io.Response.write
      (fun writer ->
        let () =
          Logs.debug (fun m ->
              (m "send body (%a)" Cohttp.Transfer.pp_encoding response.encoding
               [@ocaml.warning "-3"]))
        in
        flow_to_writer body writer Io.Response.write_body)
      response output
  in
  Eio.Buf_write.flush output

let callback { conn_closed; handler } conn input output =
  let id = (Cohttp.Connection.create () [@ocaml.warning "-3"]) in
  let rec handle () =
    match read input with
    | `Eof -> conn_closed (conn, id)
    | `Invalid e ->
        write output
          (Http.Response.make ~status:`Bad_request ())
          (Body.of_string e)
    | `Ok (request, body) ->
        let () =
          match handler (conn, id) request body with
          | `Response (response, body) -> write output response body
          | `Expert (response, handler) ->
              let () = Io.Response.write_header response output in
              handler input output
        in
        if Cohttp.Request.is_keep_alive request then handle ()
  in
  handle ()

let run ?max_connections ?additional_domains ?stop ~on_error socket server =
  Eio.Net.run_server socket ?max_connections ?additional_domains ?stop ~on_error
    (fun socket peer_address ->
      try
        Eio.Switch.run @@ fun sw ->
        let () =
          Logs.info (fun m ->
              m "%a: accept connection" Eio.Net.Sockaddr.pp peer_address)
        and input = Eio.Buf_read.of_flow ~max_size:max_int socket in
        Eio.Buf_write.with_flow socket @@ fun output ->
        callback server (sw, peer_address) input output
      with Eio.Io (Eio.Net.E (Connection_reset _), _) ->
        Logs.info (fun m ->
            m "%a: disconnected" Eio.Net.Sockaddr.pp peer_address))

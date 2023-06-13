open Utils

type handler =
  sw:Eio.Switch.t ->
  Eio.Net.Sockaddr.stream ->
  Http.Request.t ->
  Eio.Flow.source ->
  Http.Response.t * Eio.Flow.source

type t = {
  addr : Eio.Net.Sockaddr.t;
  handler : handler;
  socket : Eio.Net.listening_socket;
}

let make net ~sw ?(backlog = 128) ?(host = Eio.Net.Ipaddr.V4.loopback)
    ?(port = 0) handler =
  let socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~reuse_port:true ~backlog
      (`Tcp (host, port))
  in
  let addr =
    (* FIXME: this can be incorrect, fix when
       https://github.com/ocaml-multicore/eio/pull/555 is merged *)
    `Tcp (host, port)
  in
  { addr; handler; socket }

let listening_addr { addr; _ } = addr

let read _peer_address input =
  match Io.Request.read input with
  | (`Eof | `Invalid _) as e -> e
  | `Ok request -> (
      match Http.Request.has_body request with
      | `No -> `Ok (request, Eio.Flow.string_source "")
      | _ ->
          let body =
            let reader = Io.Request.make_body_reader request input in
            flow_of_reader reader Io.Request.read_body_chunk
          in
          `Ok (request, body))

let write output _peer_address response body =
  let response =
    let content_length =
      List.find_map
        (function Body.String s -> Some (String.length s) | _ -> None)
        (Eio.Flow.read_methods body)
    in
    (* encoding field might be deprecated but it is still used
       to compute headers and encode the body*)
    match content_length with
    | None ->
        { response with Cohttp.Response.encoding = Chunked }
        [@ocaml.warning "-3"]
    | Some size ->
        { response with encoding = Fixed (Int64.of_int size) }
        [@ocaml.warning "-3"]
  in
  let () = Logs.debug (fun m -> m "send headers") in
  let () =
    Io.Response.write
      (fun writer ->
        let () = Logs.debug (fun m -> m "send body") in
        flow_to_writer body writer Io.Response.write_body)
      response output
  in
  Eio.Buf_write.flush output

let handle (handler : handler) peer_address input output =
  Eio.Switch.run @@ fun sw ->
  match read peer_address input with
  | (`Eof | `Invalid _) as e -> e
  | `Ok (request, body) ->
      let response, body = handler ~sw peer_address request body in
      let () = write output peer_address response body in
      `Ok

let run { handler; socket; _ } =
  let rec accept () =
    let () =
      Eio.Switch.run @@ fun sw ->
      let socket, peer_address = Eio.Net.accept ~sw socket in
      let () =
        Logs.info (fun m ->
            m "%a: accept connection" Eio.Net.Sockaddr.pp peer_address)
      in
      Eio.Fiber.fork ~sw @@ fun () ->
      let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
      Eio.Buf_write.with_flow socket @@ fun output ->
      match handle handler peer_address input output with
      | `Eof ->
          Logs.info (fun m ->
              m "%a: connection closed" Eio.Net.Sockaddr.pp peer_address)
      | `Invalid error ->
          Logs.warn (fun m ->
              m "%a: invalid request: %s" Eio.Net.Sockaddr.pp peer_address error)
      | `Ok -> ()
    in
    accept ()
  in
  accept ()

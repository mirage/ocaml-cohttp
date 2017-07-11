open Core
open Async

module Request = struct
  include Cohttp.Request
  include (Make(Io) : module type of Make(Io) with type t := t)
end

module Response = struct
  include Cohttp.Response
  include (Make(Io) : module type of Make(Io) with type t := t)
end

type ('address, 'listening_on) t = {
  server: ('address, 'listening_on) Tcp.Server.t sexp_opaque;
} [@@deriving sexp_of]

type response = Response.t * Body.t [@@deriving sexp_of]

let close t = Tcp.Server.close t.server
let close_finished t = Tcp.Server.close_finished t.server
let is_closed t = Tcp.Server.is_closed t.server

let read_body req rd =
  match Request.has_body req with
  (* TODO maybe attempt to read body *)
  | `No | `Unknown -> (`Empty, Deferred.unit)
  | `Yes -> (* Create a Pipe for the body *)
    let reader = Request.make_body_reader req rd in
    let pipe = Body_raw.pipe_of_body Request.read_body_chunk reader in
    (`Pipe pipe, Pipe.closed pipe)

let handle_client handle_request sock rd wr =
  let last_body_pipe_drained = ref Deferred.unit in
  let requests_pipe =
    Reader.read_all rd (fun rd ->
        !last_body_pipe_drained >>= fun () ->
        Request.read rd >>| function
        | `Eof | `Invalid _ -> `Eof
        | `Ok req ->
          let body, finished = read_body req rd in
          last_body_pipe_drained := finished;
          `Ok (req, body)
      ) in
  Pipe.iter requests_pipe ~f:(fun (req, body) ->
      handle_request ~body sock req
      >>= fun (res, res_body) ->
      let keep_alive = Request.is_keep_alive req in
      let flush = Response.flush res in
      let res =
        let headers = Cohttp.Header.add_unless_exists
            (Cohttp.Response.headers res)
            "connection"
            (if keep_alive then "keep-alive" else "close") in
        { res with Response.headers } in
      Response.write ~flush (Body_raw.write_body Response.write_body res_body)
        res wr
      >>= fun () ->
      Writer.(if keep_alive then flushed else close ?force_close:None) wr
      >>= fun () ->
      Body.drain body
    ) >>= fun () ->
  Writer.close wr >>= fun () ->
  Reader.close rd

let respond ?(flush=true) ?(headers=Cohttp.Header.init ())
    ?(body=`Empty) status : response Deferred.t =
  let encoding = Body.transfer_encoding body in
  let resp = Response.make ~status ~flush ~encoding ~headers () in
  return (resp, body)

let respond_with_pipe ?flush ?headers ?(code=`OK) body =
  respond ?flush ?headers ~body:(`Pipe body) code

let respond_with_string ?flush ?headers ?(code=`OK) body =
  respond ?flush ?headers ~body:(`String body) code

let respond_string ?flush ?headers ?(status=`OK) body =
  respond ?flush ?headers ~body:(`String body) status

let respond_with_redirect ?headers uri =
  let headers = Cohttp.Header.add_opt_unless_exists headers "location" (Uri.to_string uri) in
  respond ~flush:false ~headers `Found

let resolve_local_file ~docroot ~uri =
  (* This normalises the Uri and strips out .. characters *)
  Uri.(pct_decode (path (resolve "" (of_string "/") uri)))
  |> Filename.concat docroot

let error_body_default =
  "<html><body><h1>404 Not Found</h1></body></html>"

let respond_with_file ?flush ?headers ?(error_body=error_body_default) filename =
  Monitor.try_with ~run:`Now
    (fun () ->
       Reader.open_file filename
       >>= fun rd ->
       let body = `Pipe (Reader.pipe rd) in
       let mime_type = Magic_mime.lookup filename in
       let headers = Cohttp.Header.add_opt_unless_exists headers "content-type" mime_type in
       respond ?flush ~headers ~body `OK
    )
  >>= function
  |Ok res -> return res
  |Error _exn -> respond_string ~status:`Not_found error_body

let create ?max_connections ?buffer_age_limit ?on_handler_error
    ?(mode=`TCP) where_to_listen handle_request =
  Conduit_async.serve ?max_connections
    ?buffer_age_limit ?on_handler_error mode
    where_to_listen (handle_client handle_request)
  >>| fun server ->
  { server }

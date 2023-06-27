open Base
open Async_kernel
open Async_unix

type ('address, 'listening_on) t = {
  server : ('address, 'listening_on) Tcp.Server.t; [@sexp.opaque]
}
[@@deriving sexp_of]

let num_connections t = Tcp.Server.num_connections t.server

type response = Cohttp.Response.t * Body.t [@@deriving sexp_of]

type response_action =
  [ `Expert of Http.Response.t * (Reader.t -> Writer.t -> unit Deferred.t)
  | `Response of response ]

type 'r respond_t =
  ?flush:bool ->
  ?headers:Http.Header.t ->
  ?body:Body.t ->
  Http.Status.t ->
  'r Deferred.t

let close t = Tcp.Server.close t.server
let close_finished t = Tcp.Server.close_finished t.server
let is_closed t = Tcp.Server.is_closed t.server
let listening_on t = Tcp.Server.listening_on t.server

let read_body req rd =
  match Http.Request.has_body req with
  (* TODO maybe attempt to read body *)
  | `No | `Unknown -> `Empty
  | `Yes ->
      (* Create a Pipe for the body *)
      let reader = Io.Request.make_body_reader req rd in
      let pipe = Body.Private.pipe_of_body Io.Request.read_body_chunk reader in
      `Pipe pipe

let collect_errors writer ~f =
  let monitor = Writer.monitor writer in
  (* don't propagate errors up, we handle them here *)
  Monitor.detach_and_get_error_stream monitor |> (ignore : exn Stream.t -> unit);
  choose
    [
      choice (Monitor.get_next_error monitor) (fun e ->
          Error (Exn.Reraised ("Cohttp_async.Server.collect_errors", e)));
      choice (try_with ~name:"Cohttp_async.Server.collect_errors" f) Fn.id;
    ]

let handle_client handle_request sock rd wr =
  collect_errors wr ~f:(fun () ->
      let rd = Input_channel.create rd in
      let rec loop rd wr sock handle_request =
        if Input_channel.is_closed rd then Deferred.unit
        else
          Io.Request.read rd >>= function
          | `Eof | `Invalid _ -> Deferred.unit
          | `Ok req -> (
              let req_body = read_body req rd in
              handle_request ~body:req_body sock req >>= function
              | `Expert (res, handler) ->
                  Io.Response.write_header res wr >>= fun () ->
                  Input_channel.to_reader
                    (Info.of_string "Cohttp_async.Server.Expert: Create reader")
                    rd
                  >>= fun reader -> handler reader wr
              | `Response (res, res_body) ->
                  (* There are scenarios if a client leaves before consuming the full response,
                     we might have a reference to an async Pipe that doesn't get drained.

                     Not draining or closing a pipe can lead to issues if its holding a resource like
                     a file handle as those resources will never be closed, leading to a leak.

                     Async writers have a promise that's fulfilled whenever they are closed,
                     so we can use it to schedule a close operation on the stream to ensure that we
                     don't leave a stream open if the underlying channels are closed. *)
                  (match res_body with
                  | `Empty | `String _ | `Strings _ -> ()
                  | `Pipe stream ->
                      Deferred.any_unit
                        [ Writer.close_finished wr; Writer.consumer_left wr ]
                      >>> fun () -> Pipe.close_read stream);
                  let keep_alive =
                    Http.Request.is_keep_alive req
                    && Http.Response.is_keep_alive res
                  in
                  let flush = Http.Response.flush res in
                  let res =
                    let headers =
                      Http.Header.add_unless_exists
                        (Http.Response.headers res)
                        "connection"
                        (if keep_alive then "keep-alive" else "close")
                    in
                    { res with Http.Response.headers }
                  in
                  Io.Response.write ~flush
                    (Body.Private.write_body Io.Response.write_body res_body)
                    res wr
                  >>= fun () ->
                  Body.Private.drain req_body >>= fun () ->
                  if keep_alive then loop rd wr sock handle_request
                  else Deferred.unit)
      in
      loop rd wr sock handle_request)
  >>| fun res -> Result.ok_exn res

let respond ?(flush = true) ?(headers = Http.Header.init ()) ?(body = `Empty)
    status : response Deferred.t =
  let encoding = Body.transfer_encoding body in
  let resp = Cohttp.Response.make ~status ~flush ~encoding ~headers () in
  return (resp, body)

let respond_with_pipe ?flush ?headers ?(code = `OK) body =
  respond ?flush ?headers ~body:(`Pipe body) code

let respond_string ?flush ?headers ?(status = `OK) body =
  respond ?flush ?headers ~body:(`String body) status

let respond_with_redirect ?headers uri =
  let headers =
    Http.Header.add_opt_unless_exists headers "location" (Uri.to_string uri)
  in
  respond ~flush:false ~headers `Found

let resolve_local_file ~docroot ~uri =
  Cohttp.Path.resolve_local_file ~docroot ~uri

let error_body_default = "<html><body><h1>404 Not Found</h1></body></html>"

let respond_with_file ?flush ?headers ?(error_body = error_body_default)
    filename =
  Monitor.try_with ~run:`Now (fun () ->
      Reader.open_file filename >>= fun rd ->
      let body = `Pipe (Reader.pipe rd) in
      let mime_type = Magic_mime.lookup filename in
      let headers =
        Http.Header.add_opt_unless_exists headers "content-type" mime_type
      in
      respond ?flush ~headers ~body `OK)
  >>= function
  | Ok res -> return res
  | Error _exn -> respond_string ~status:`Not_found error_body

type mode = Conduit_async.server

let create_raw ?max_connections ?backlog ?buffer_age_limit ?(mode = `TCP)
    ~on_handler_error where_to_listen handle_request =
  Conduit_async.serve ?max_connections ?backlog ?buffer_age_limit
    ~on_handler_error mode where_to_listen
    (handle_client handle_request)
  >>| fun server -> { server }

let create_expert ?max_connections ?backlog ?buffer_age_limit ?(mode = `TCP)
    ~on_handler_error where_to_listen handle_request =
  create_raw ?max_connections ?backlog ?buffer_age_limit ~on_handler_error ~mode
    where_to_listen handle_request

let create ?max_connections ?backlog ?buffer_age_limit ?(mode = `TCP)
    ~on_handler_error where_to_listen handle_request =
  let handle_request ~body address request =
    handle_request ~body address request >>| fun r -> `Response r
  in
  create_raw ?max_connections ?backlog ?buffer_age_limit ~on_handler_error ~mode
    where_to_listen handle_request

module Expert = struct
  let create handle_request addr reader writer =
    let handle_request ~body addr request =
      handle_request ~body addr request >>| fun r -> `Response r
    in
    handle_client handle_request addr reader writer

  let create_with_response_action handle_request addr reader writer =
    handle_client handle_request addr reader writer
end

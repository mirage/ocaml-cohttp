(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Cohttp
open Lwt

module Body = Cohttp_lwt_body

module type IO = S.IO with type 'a t = 'a Lwt.t

module S = Cohttp_lwt_s

module type Client = S.Client
module type Server = S.Server
module type Net = S.Net

module Make_request(IO:IO) = struct
  include Cohttp.Request
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module Make_response(IO:IO) = struct
  include Cohttp.Response
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module Request = Cohttp.Request
module Response = Cohttp.Response

module Make_client
    (IO:IO)
    (Net:Cohttp_lwt_s.Net with module IO = IO) = struct

  module IO = IO
  module Response = Make_response(IO)
  module Request = Make_request(IO)

  type ctx = Net.ctx with sexp_of
  let default_ctx = Net.default_ctx

  let read_response ~closefn ic oc meth =
    Response.read ic >>= begin function
    | `Invalid reason ->
      Lwt.fail (Failure ("Failed to read response: " ^ reason))
    | `Eof -> Lwt.fail (Failure "Client connection was closed")
    | `Ok res -> begin
        let has_body = match meth with
          | `HEAD -> `No
          | _ -> Response.has_body res
        in
        match has_body with
        | `Yes | `Unknown ->
          let reader = Response.make_body_reader res ic in
          let stream = Body.create_stream Response.read_body_chunk reader in
          let closefn = closefn in
          Lwt_stream.on_terminate stream closefn;
          let gcfn st = closefn () in
          Gc.finalise gcfn stream;
          let body = Body.of_stream stream in
          return (res, body)
        | `No -> return (res, `Empty)
      end
    end
    |> fun t ->
    Lwt.on_cancel t closefn;
    Lwt.on_failure t (fun _exn -> closefn ());
    t

  let is_meth_chunked = function
    | `HEAD -> false
    | `GET -> false
    | `DELETE -> false
    | _ -> true

  let call ?(ctx=default_ctx) ?headers ?(body=`Empty) ?chunked meth uri =
    let headers = match headers with None -> Header.init () | Some h -> h in
    Net.connect_uri ~ctx uri >>= fun (conn, ic, oc) ->
    let closefn () = Net.close ic oc in
    let chunked =
      match chunked with
      | None -> is_meth_chunked meth
      | Some v -> v in
    let sent = match chunked with
      | true ->
        let req = Request.make_for_client ~headers ~chunked meth uri in
        Request.write (fun writer ->
          Body.write_body (Request.write_body writer) body) req oc
      | false ->
        (* If chunked is not allowed, then obtain the body length and
           insert header *)
        Body.length body >>= fun (body_length, buf) ->
        let req =
          Request.make_for_client ~headers ~chunked ~body_length meth uri
        in
        Request.write (fun writer ->
          Body.write_body (Request.write_body writer) buf) req oc
    in
    sent >>= fun () ->
    read_response ~closefn ic oc meth

  (* The HEAD should not have a response body *)
  let head ?ctx ?headers uri =
    call ?headers `HEAD uri
    >|= fst

  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri
  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `DELETE uri
  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `POST uri
  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PUT uri
  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?headers ?body ?chunked `PATCH uri

  let post_form ?ctx ?headers ~params uri =
    let headers = Header.add_opt_unless_exists headers
                    "content-type" "application/x-www-form-urlencoded" in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  let callv ?(ctx=default_ctx) uri reqs =
    Net.connect_uri ~ctx uri >>= fun (conn, ic, oc) ->
    (* Serialise the requests out to the wire *)
    let meth_stream = Lwt_stream.map_s (fun (req,body) ->
      Request.write (fun writer ->
        Body.write_body (Request.write_body writer) body
      ) req oc >>= fun () ->
      return (Request.meth req)
    ) reqs in
    (* Read the responses. For each response, ensure that the previous
       response has consumed the body before continuing to the next
       response because HTTP/1.1-pipelining cannot be interleaved. *)
    let read_m = Lwt_mutex.create () in
    let last_body = ref None in
    let closefn () = Lwt_mutex.unlock read_m in
    let resps = Lwt_stream.map_s (fun meth ->
      begin match !last_body with
      | None -> return_unit
      | Some body -> Body.drain_body body
      end >>= fun () ->
      Lwt_mutex.with_lock read_m (fun () -> read_response ~closefn ic oc meth)
      >|= (fun ((_,body) as x) ->
        last_body := Some body;
        x
      )
    ) meth_stream in
    Lwt_stream.on_terminate resps (fun () -> Net.close ic oc);
    return resps
end


module Make_server(IO:IO) = struct
  module IO = IO
  module Request = Make_request(IO)
  module Response = Make_response(IO)

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

  module Transfer_IO = Transfer_io.Make(IO)

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
        | Transfer.Unknown -> Body.transfer_encoding body
        | t -> t
    in
    let res = Response.make ~status ~flush ~encoding ?headers () in
    return (res, body)

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status
                ~encoding:(Transfer.Fixed (Int64.of_int (String.length body)))
                ?headers () in
    let body = Body.of_string body in
    return (res,body)

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
      then return_none
      else
        Lwt_mutex.lock read_m >>= fun () ->
        Request.read ic >>= function
        | `Eof | `Invalid _ -> (* TODO: request logger for invalid req *)
          Lwt_mutex.unlock read_m;
          return_none
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
              return (Some (req, body))
            (* TODO for now we are just repeating the old behaviour
             * of ignoring the body in the request. Perhaps it should be
             * changed it did for responses *)
            | `No | `Unknown ->
              Lwt_mutex.unlock read_m;
              return (Some (req, `Empty))
          end
    end

  let response_stream callback io_id conn_id req_stream =
    Lwt_stream.map_s (fun (req, body) ->
      Lwt.finalize
        (fun () ->
           Lwt.catch
             (fun () -> callback (io_id, conn_id) req body)
             (fun _exn -> respond_error ~body:"Internal Server Error" ()))
        (fun () -> Body.drain_body body)
    ) req_stream

  let callback spec io_id ic oc =
    let conn_id = Connection.create () in
    let conn_closed () = spec.conn_closed (io_id,conn_id) in
    (* The server operates by reading requests into a Lwt_stream of requests
       and mapping them into a stream of responses serially using [spec]. The
       responses are then sent over the wire *)
    let req_stream = request_stream ic in
    let res_stream = response_stream spec.callback io_id conn_id req_stream in
    (* Clean up resources when the response stream terminates and call
     * the user callback *)
    Lwt_stream.on_terminate res_stream conn_closed;
    (* Transmit the responses *)
    res_stream |> Lwt_stream.iter_s (fun (res,body) ->
      let flush = Response.flush res in
      Response.write ~flush (fun writer ->
        Body.write_body (Response.write_body writer) body
      ) res oc
    )
end


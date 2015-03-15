(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
 *)

open Cohttp
open Lwt

module type IO = S.IO with type 'a t = 'a Lwt.t

module type Net = sig
  module IO : IO
  type ctx with sexp_of
  val default_ctx : ctx
  val connect_uri : ctx:ctx -> Uri.t -> (IO.conn * IO.ic * IO.oc) Lwt.t
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit
end

module type Request = sig
  type t = Cohttp.Request.t with sexp
  include Cohttp.S.Request with type t := Cohttp.Request.t
  include Cohttp.S.Http_io with type t := Cohttp.Request.t
                            and type 'a IO.t = 'a Lwt.t
end

module Make_request(IO:IO) = struct
  include Cohttp.Request
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module type Response = sig
  type t = Cohttp.Response.t with sexp
  include Cohttp.S.Response with type t := Cohttp.Response.t
  include Cohttp.S.Http_io with type t := Cohttp.Response.t
                            and type 'a IO.t = 'a Lwt.t
end

module Make_response(IO:IO) = struct
  include Cohttp.Response
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module type Client = sig
  module IO : IO
  module Request : Request with module IO = IO
  module Response : Response with module IO = IO

  type ctx with sexp_of
  val default_ctx : ctx
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit

  val call :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    Cohttp.Code.meth ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val head :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> Response.t Lwt.t

  val get :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val delete :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val put :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val patch :
    ?ctx:ctx ->
    ?body:Cohttp_lwt_body.t ->
    ?chunked:bool ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val post_form :
    ?ctx:ctx ->
    ?headers:Cohttp.Header.t ->
    params:(string * string list) list ->
    Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

  val callv :
    ?ctx:ctx ->
    Uri.t ->
    (Request.t * Cohttp_lwt_body.t) Lwt_stream.t ->
    (Response.t * Cohttp_lwt_body.t) Lwt_stream.t Lwt.t
end

module Make_client
    (IO:IO)
    (Request:Request with module IO = IO)
    (Response:Response with module IO = IO)
    (Net:Net with module IO = IO) = struct

  module IO = IO
  module Request = Request
  module Response = Response

  type ctx = Net.ctx with sexp_of
  let default_ctx = Net.default_ctx
  let close_in = Net.close_in
  let close_out = Net.close_out
  let close = Net.close

  let read_response ?closefn ic oc =
    Response.read ic >>= function
    | `Invalid reason -> Lwt.fail (Failure ("Failed to read response: " ^ reason))
    | `Eof -> Lwt.fail (Failure "Client connection was closed")
    | `Ok res -> begin
        match Response.has_body res with
        | `Yes | `Unknown ->
          let reader = Response.make_body_reader res ic in
          let stream = Cohttp_lwt_body.create_stream Response.read_body_chunk reader in
          (match closefn with
           |Some fn ->
             Lwt_stream.on_terminate stream fn;
             let gcfn st = fn () in
             Gc.finalise gcfn stream
           |None -> ()
          );
          let body = Cohttp_lwt_body.of_stream stream in
          return (res, body)
        | `No ->
          (match closefn with |Some fn -> fn () |None -> ());
          return (res, `Empty)
      end

  let call ?(ctx=default_ctx) ?headers ?(body=`Empty) ?(chunked=true) meth uri =
    let headers = match headers with None -> Header.init () | Some h -> h in
    lwt (conn,ic,oc) = Net.connect_uri ~ctx uri in
    let closefn () = Net.close ic oc in
    match chunked with
    | true ->
      let req = Request.make_for_client ~headers ~chunked meth uri in
      Request.write (fun writer ->
          Cohttp_lwt_body.write_body (Request.write_body writer) body) req oc
      >>= fun () ->
      read_response ~closefn ic oc
    | false ->
      (* If chunked is not allowed, then obtain the body length and insert header *)
      lwt (body_length, buf) = Cohttp_lwt_body.length body in
      let req = Request.make_for_client ~headers ~chunked ~body_length meth uri in
      Request.write (fun writer ->
          Cohttp_lwt_body.write_body (Request.write_body writer) buf) req oc
      >>= fun () ->
      read_response ~closefn ic oc

  (* The HEAD should not have a response body *)
  let head ?ctx ?headers uri =
    call ?headers ~chunked:false `HEAD uri
    >|= fst

  let get ?ctx ?headers uri = call ?ctx ?headers ~chunked:false `GET uri
  let delete ?ctx ?headers uri = call ?ctx ?headers ~chunked:false `DELETE uri
  let post ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `POST uri
  let put ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `PUT uri
  let patch ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `PATCH uri

  let post_form ?ctx ?headers ~params uri =
    let headers = Header.add_opt_unless_exists headers "content-type" "application/x-www-form-urlencoded" in
    let body = Cohttp_lwt_body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  let callv ?(ctx=default_ctx) uri reqs =
    lwt (conn, ic, oc) = Net.connect_uri ~ctx uri in
    (* Serialise the requests out to the wire *)
    let _ = Lwt_stream.iter_s (fun (req,body) ->
        Request.write (fun writer ->
            Cohttp_lwt_body.write_body (Request.write_body writer) body) req oc)
        reqs in
    (* Read the responses. For each response, ensure that the previous response
     * has consumed the body before continuing to the next response, since HTTP/1.1
     * pipelining cannot be interleaved. *)
    let read_m = Lwt_mutex.create () in
    let resps = Lwt_stream.from (fun () ->
        let closefn () = Lwt_mutex.unlock read_m in
        Lwt.catch (fun () ->
          Lwt_mutex.with_lock read_m (fun () -> read_response ~closefn ic oc)
          >|= (fun x -> Some x)
        ) (fun _ -> return_none)
      ) in
    Lwt_stream.on_terminate resps (fun () -> Net.close ic oc);
    return resps
end

(** Configuration of servers. *)
module type Server = sig
  module IO : IO
  module Request : Request with module IO = IO
  module Response : Response with module IO = IO

  type ctx with sexp_of
  val default_ctx : ctx
  val close_in : IO.ic -> unit
  val close_out : IO.oc -> unit
  val close : IO.ic -> IO.oc -> unit

  type conn = IO.conn * Cohttp.Connection.t

  type t

  val make : ?conn_closed:(conn -> unit)
    -> callback:(conn -> Cohttp.Request.t -> Cohttp_lwt_body.t
                 -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t)
    -> unit -> t

  val resolve_local_file : docroot:string -> uri:Uri.t -> string

  val respond :
    ?headers:Cohttp.Header.t ->
    ?flush:bool ->
    status:Cohttp.Code.status_code ->
    body:Cohttp_lwt_body.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_string :
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_error :
    ?headers:Header.t ->
    ?status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_redirect :
    ?headers:Cohttp.Header.t ->
    uri:Uri.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_need_auth :
    ?headers:Cohttp.Header.t ->
    auth:Cohttp.Auth.challenge ->
    unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val respond_not_found :
    ?uri:Uri.t -> unit -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val callback: t -> IO.conn -> IO.ic -> IO.oc -> unit Lwt.t
end


module Make_server(IO:IO)
    (Request:Request with module IO=IO)
    (Response:Response with module IO=IO)
    (Net:Net with module IO=IO) = struct
  module IO = IO
  module Request = Request
  module Response = Response

  type ctx = Net.ctx with sexp_of
  let default_ctx = Net.default_ctx
  let close_in = Net.close_in
  let close_out = Net.close_out
  let close = Net.close

  type conn = IO.conn * Cohttp.Connection.t

  type t = {
    callback :
      conn ->
      Cohttp.Request.t ->
      Cohttp_lwt_body.t ->
      (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t;
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
      | None -> Cohttp_lwt_body.transfer_encoding body
      | Some headers ->
         match Header.get_transfer_encoding headers with
         | Transfer.Unknown -> Cohttp_lwt_body.transfer_encoding body
         | t -> t
    in
    let res = Response.make ~status ~flush ~encoding ?headers () in
    return (res, body)

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status
        ~encoding:(Transfer.Fixed (Int64.of_int (String.length body)))
        ?headers () in
    let body = Cohttp_lwt_body.of_string body in
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

  let callback spec =
    let daemon_callback io_id ic oc =
      let conn_id = Connection.create () in
      let conn_closed () = spec.conn_closed (io_id,conn_id) in
      let read_m = Lwt_mutex.create () in
      (* If the request is HTTP version 1.0 then the request stream should be
         considered closed after the first request/response. *)
      let early_close = ref false in
      (* Read the requests *)
      let req_stream = Lwt_stream.from (
        fun () ->
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
                (* Ensure the input body has been fully read before reading again *)
                match Request.has_body req with
                | `Yes ->
                  let reader = Request.make_body_reader req ic in
                  let body_stream = Cohttp_lwt_body.create_stream Request.read_body_chunk reader in
                  Lwt_stream.on_terminate body_stream (fun () -> Lwt_mutex.unlock read_m);
                  let body = Cohttp_lwt_body.of_stream body_stream in
                  (* The read_m remains locked until the caller reads the body *)
                  return (Some (req, body))
                (* TODO for now we are just repeating the old behaviour
                 * of ignoring the body in the request. Perhaps it should
                 * be changed it did for responses *)
                | `No | `Unknown ->
                  Lwt_mutex.unlock read_m;
                  return (Some (req, `Empty))
              end
        ) in
      (* Map the requests onto a response stream to serialise out *)
      let res_stream =
        Lwt_stream.map_s (fun (req, body) ->
          try_lwt
            spec.callback (io_id,conn_id) req body
          with exn ->
            respond_error ~body:(Printexc.to_string exn) ()
          finally Cohttp_lwt_body.drain_body body
        ) req_stream in
      (* Clean up resources when the response stream terminates and call
       * the user callback *)
      Lwt_stream.on_terminate res_stream conn_closed;
      (* Transmit the responses *)
      for_lwt (res,body) in res_stream do
        let flush = Response.flush res in
        Response.write ~flush (fun writer ->
          Cohttp_lwt_body.write_body (Response.write_body writer) body
        ) res oc
      done
    in daemon_callback
end

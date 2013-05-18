(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
open Cohttp_lwt_make

module Client(IO:Cohttp.IO.S with type 'a t = 'a Lwt.t)
             (ReqIO:Cohttp.Request.S with module IO = IO)
             (ResIO:Cohttp.Response.S with module IO = IO)
             (Net:NET with type oc = ResIO.IO.oc and type ic = ResIO.IO.ic)  = struct
  let read_response ?closefn ic oc =
    match_lwt ResIO.read ic with
    |None -> return None
    |Some res -> begin
      match ResIO.has_body res with
      |true ->
        let stream = Cohttp_lwt_body.create_stream (ResIO.read_body_chunk res) ic in
        (match closefn with 
          |Some fn ->
             Lwt_stream.on_terminate stream fn;
             let gcfn st = fn () in
             Gc.finalise gcfn stream
          |None -> ()
        );
        let body = Cohttp_lwt_body.body_of_stream stream in
        return (Some (res, body))
      |false ->
        (match closefn with |Some fn -> fn () |None -> ());
        return (Some (res, None))
    end
 
  let call ?headers ?(body:Cohttp_lwt_body.t) ?(chunked=true) meth uri =
    let headers = match headers with None -> Header.init () | Some h -> h in
    lwt (ic,oc) = Net.connect_uri uri in
    let closefn () = Net.close ic oc in
    match chunked with
    | true -> 
        let req = Request.make_for_client ~headers ~chunked meth uri in
        ReqIO.write (fun req oc ->
          Cohttp_lwt_body.write_body (ReqIO.write_body req oc) body) req oc
        >>= fun () ->
        read_response ~closefn ic oc
    | false ->
        (* If chunked is not allowed, then obtain the body length and insert header *)
        lwt (body_length, buf) = Cohttp_lwt_body.get_length body in
        let req = Request.make_for_client ~headers ~chunked ~body_length meth uri in
        ReqIO.write (fun req oc ->
          Cohttp_lwt_body.write_body (ReqIO.write_body req oc) buf) req oc 
        >>= fun () ->
        read_response ~closefn ic oc

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PUT uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 

  let post_form ?headers ~params uri =
    let headers = Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
    let q = List.map (fun (k,v) -> k, [v]) (Header.to_list params) in
    let body = Cohttp_lwt_body.body_of_string (Uri.encoded_of_query q) in
    post ~chunked:false ~headers ?body uri

  let callv ?(ssl=false) host port reqs =
    lwt (ic, oc) = Net.connect ~ssl host port in
    (* Serialise the requests out to the wire *)
    let _ = Lwt_stream.iter_s (fun (req,body) -> 
      ReqIO.write (fun req oc ->
        Cohttp_lwt_body.write_body (ReqIO.write_body req oc) body) req oc)
      reqs in
    (* Read the responses. For each response, ensure that the previous response
     * has consumed the body before continuing to the next response, since HTTP/1.1
     * pipelining cannot be interleaved. *)
    let read_m = Lwt_mutex.create () in
    let resps = Lwt_stream.from (fun () ->
       let closefn () = Lwt_mutex.unlock read_m in
       Lwt_mutex.with_lock read_m (fun () -> read_response ~closefn ic oc) 
     ) in
    Lwt_stream.on_terminate resps (fun () -> Net.close ic oc);
    return resps
end

module Server(IO:Cohttp.IO.S with type 'a t = 'a Lwt.t)
             (ReqIO:Cohttp.Request.S with module IO=IO)
             (ResIO:Cohttp.Response.S with module IO=IO)
             (Net:NET with type oc = ResIO.IO.oc and type ic = ResIO.IO.ic)  = struct
  module Transfer_IO = Transfer_io.Make(IO)

  type conn_id = int
  let string_of_conn_id = string_of_int

  type config = {
    callback: conn_id -> ?body:Cohttp_lwt_body.contents -> Request.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t;
    conn_closed : conn_id -> unit -> unit;
  }

  let respond ?headers ~status ~body () =
    let encoding = Cohttp_lwt_body.get_transfer_encoding body in
    let res = Response.make ~status ~encoding ?headers () in
    return (res, body)

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status 
      ~encoding:(Transfer.Fixed (String.length body)) ?headers () in
    let body = Cohttp_lwt_body.body_of_string body in
    return (res,body)

  let respond_error ~status ~body () =
    respond_string ~status ~body:("Error: "^body) ()

  let respond_redirect ?headers ~uri () =
    let headers = 
      match headers with
      |None -> Header.init_with "location" (Uri.to_string uri)
      |Some h -> Header.add h "location" (Uri.to_string uri)
    in
    respond ~headers ~status:`Found ~body:None ()

  let respond_need_auth ?headers ~auth () =
    let headers = match headers with |None -> Header.init () |Some h -> h in
    let headers = Header.add_authorization_req headers auth in
    respond ~headers ~status:`Unauthorized ~body:None ()

  let respond_not_found ?uri () =
    let body = match uri with
     |None -> "Not found"
     |Some uri -> "Not found: " ^ (Uri.to_string uri) in
    respond_string ~status:`Not_found ~body ()

  let callback spec =
    let conn_id = ref 0 in
    let daemon_callback ic oc =
      let conn_id = incr conn_id; !conn_id in
      let read_m = Lwt_mutex.create () in
      (* If the request is HTTP version 1.0 then the request stream should be
         considered closed after the first request/response. *)
      let early_close = ref false in
      (* Read the requests *)
      let req_stream = Lwt_stream.from (fun () ->
        if !early_close
        then return None
        else
          lwt () = Lwt_mutex.lock read_m in
          match_lwt ReqIO.read ic with
          |None ->
            Lwt_mutex.unlock read_m;
            return None
          |Some req -> begin
            early_close := Request.version req = `HTTP_1_0;
            (* Ensure the input body has been fully read before reading again *)
            match ReqIO.has_body req with
            |true ->
              let body_stream = Cohttp_lwt_body.create_stream (ReqIO.read_body_chunk req) ic in
              Lwt_stream.on_terminate body_stream (fun () -> Lwt_mutex.unlock read_m);
              let body = Cohttp_lwt_body.body_of_stream body_stream in
              (* The read_m remains locked until the caller reads the body *)
              return (Some (req, body))
            |false ->
              Lwt_mutex.unlock read_m;
              return (Some (req, None))
          end
      ) in
      (* Map the requests onto a response stream to serialise out *)
      let res_stream = Lwt_stream.map_s (fun (req, body) -> 
        try_lwt 
          spec.callback conn_id ?body req
        with exn ->
          respond_error ~status:`Internal_server_error ~body:(Printexc.to_string exn) ()
        finally Cohttp_lwt_body.drain_body body
      ) req_stream in
      (* Clean up resources when the response stream terminates and call
       * the user callback *)
      Lwt_stream.on_terminate res_stream (spec.conn_closed conn_id);
      (* Transmit the responses *)
      for_lwt (res,body) in res_stream do
        ResIO.write (fun res oc ->
          Cohttp_lwt_body.write_body (ResIO.write_body res oc) body
        ) res oc
      done
    in daemon_callback
end

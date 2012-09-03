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
include Cohttp_lwt_raw

module Body = struct
  (* For now this is just a stream, but it can also be a direct
   * string or byte-buffer eventually, so keep the type abstract
   * for external applications. *)
  type contents = [
   |`Stream of string Lwt_stream.t
   |`String of string
  ]

  type t = contents option

  let stream_of_input_channel read_fn ic =
    let fin = ref false in
    Lwt_stream.from (fun () ->
      match !fin with
      |true -> return None
      |false -> begin
        match_lwt read_fn ic with
        |Transfer.Done -> 
          return None
        |Transfer.Final_chunk c ->
          fin := true;
          return (Some c);
        |Transfer.Chunk c ->
          return (Some c)
      end
    )

  let string_of_body (body:t) =
    match body with
    |None -> return ""
    |Some (`String s) -> return s
    |Some (`Stream s) ->
       let b = Buffer.create 1024 in
       Lwt_stream.iter (Buffer.add_string b) s >>
       return (Buffer.contents b)

  let stream_of_body (body:t) =
    match body with
    |None -> Lwt_stream.of_list []
    |Some (`Stream s) -> s
    |Some (`String s) -> Lwt_stream.of_list [s]
    
  let body_of_string s : t =
    Some (`String s)

  let body_of_string_list l : t =
    Some (`Stream (Lwt_stream.of_list l))

  let body_of_stream s : t =
    Some (`Stream s)

  (* This will consume the body and return a length, and a
   * new body that should be used instead of the input *)
  let get_length (body:t) : (int * t) Lwt.t =
    match body with
    |None ->
      return (0, body)
    |Some (`String s) -> 
      return (String.length s, body)
    |Some (`Stream s) ->
      lwt buf = string_of_body body in
      let len = String.length buf in
      return (len, (Some (`String buf)))
end
  
module Client = struct

  let write_request ?body req oc =
    Request.write (fun req oc ->
      match body with
      |None -> return ()
      |Some (`Stream st) -> Lwt_stream.iter_s (Request.write_body req oc) st
      |Some (`String s) -> Request.write_body req oc s
    ) req oc

  let read_response ?(close=false) ic oc =
    match_lwt Response.read ic with
    |None -> return None
    |Some res -> begin
      match Response.has_body res with
      |true ->
        let stream = Body.stream_of_input_channel (Response.read_body res) ic in
        if close then
          Lwt_stream.on_terminate stream (fun () ->
            Cohttp_lwt_net.close' ic oc);
        let body = Body.body_of_stream stream in
        return (Some (res, body))
      |false ->
        return (Some (res, None))
    end
 
  let call ?headers ?(body:Body.t) ?(chunked=true) meth uri =
    lwt (ic,oc) = Cohttp_lwt_net.connect_uri uri in
    match chunked with
    |true ->
       let req = Request.make ~meth ?headers ?body uri in
       write_request ?body req oc >>
       read_response ~close:true ic oc
    |false ->
       lwt (clen, buf) = Body.get_length body in
       let headers =
         match headers with
         |None -> Header.(add_transfer_encoding (init ()) (Transfer.Fixed clen))
         |Some h -> Header.(add_transfer_encoding h (Transfer.Fixed clen))
       in
       let req = Request.make ~meth ~headers ~body uri in
       write_request ?body req oc >>
       read_response ~close:true ic oc

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 

  let post_form ?headers ~params uri =
    let headers =
      match headers with
      |None -> Header.of_list ["content-type","application/x-www-form-urlencoded"]
      |Some h -> Header.add h "content-type" "application/x-www-form-urlencoded"
    in
    let body = Body.body_of_string (Uri.encoded_of_query (Header.to_list params)) in
    post ~headers ?body uri

  let callv ?(ssl=false) host port reqs =
    lwt (ic, oc) = Cohttp_lwt_net.connect ~ssl host port in
    (* Serialise the requests out to the wire *)
    Lwt_stream.on_terminate reqs (fun () -> Cohttp_lwt_net.close_out oc);
    let _ = Lwt_stream.iter_s (fun (req,body) -> write_request ?body req oc) reqs in
    (* Read the responses *)
    let resps = Lwt_stream.from (fun () -> read_response ic oc) in
    Lwt_stream.on_terminate resps (fun () -> Cohttp_lwt_net.close_in ic);
    return resps
end

module Server = struct

  type conn_id = int
  let string_of_conn_id = string_of_int

  type config = {
    address: string;
    callback: conn_id -> ?body:Body.contents -> Request.t -> (Response.t * Body.t) Lwt.t;
    conn_closed : conn_id -> unit -> unit;
    port: int;
    timeout: int option;
  }

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status 
      ~encoding:(Transfer.Fixed (String.length body)) ?headers () in
    let body = Body.body_of_string body in
    return (res,body)

  let respond_error ~status ~body () =
    respond_string ~status ~body:("Error: "^body) ()

  let daemon_callback spec =
    let conn_id = ref 0 in
    let daemon_callback ic oc =
      let conn_id = incr conn_id; !conn_id in
      (* Read the requests *)
      let req_stream = Lwt_stream.from (fun () ->
        match_lwt Request.read ic with
        |Some req -> begin
          (* Ensure the input body has been fully read before reading again *)
          match Request.has_body req with
          |true ->
            let body_stream = Body.stream_of_input_channel (Request.read_body req) ic in
            let th,u = task () in
            Lwt_stream.on_terminate body_stream (wakeup u);
            let body = Body.body_of_stream body_stream in
            th >>= fun () -> return (Some (req, body))
          |false -> return (Some (req, None))
        end
        |None -> return None
      ) in
      (* Map the requests onto a response stream to serialise out *)
      let res_stream = Lwt_stream.map_s (fun (req, body) -> 
        try_lwt 
          spec.callback conn_id ?body req
        with exn ->
          respond_error ~status:`Internal_server_error ~body:(Printexc.to_string exn) ()
      ) req_stream in
      (* Clean up resources when the response stream terminates and call
       * the user callback *)
      Lwt_stream.on_terminate res_stream (spec.conn_closed conn_id);
      (* Transmit the responses *)
      Lwt_stream.iter_s (fun (res, body) ->
        Response.write (fun res oc ->
          match body with
          |None -> return ()
          |Some (`Stream b) -> Lwt_stream.iter_s (Response.write_body res oc) b
          |Some (`String s) -> Response.write_body res oc s
        ) res oc
      ) res_stream
    in daemon_callback
 
  let main spec =
    lwt sockaddr = Cohttp_lwt_net.build_sockaddr spec.address spec.port in
    Cohttp_lwt_net.Tcp_server.init ~sockaddr ~timeout:spec.timeout (daemon_callback spec)
end

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

module Client(Request:REQUEST)
             (Response:RESPONSE with type oc = Request.oc and type ic = Request.ic)
             (Net:NET with type oc = Response.oc and type ic = Response.ic)  = struct

  let write_request ?body req oc =
    Request.write (fun req oc ->
      Cohttp_lwt_body.write_body (Request.write_body req oc) body) req oc

  let read_response ?closefn ic oc =
    match_lwt Response.read ic with
    |None -> return None
    |Some res -> begin
      match Response.has_body res with
      |true ->
        let stream = Cohttp_lwt_body.create_stream (Response.read_body res) ic in
        (match closefn with |Some fn -> Lwt_stream.on_terminate stream fn |None -> ());
        let body = Cohttp_lwt_body.body_of_stream stream in
        return (Some (res, body))
      |false ->
        return (Some (res, None))
    end
 
  let call ?headers ?(body:Cohttp_lwt_body.t) ?(chunked=true) meth uri =
    lwt (ic,oc) = Net.connect_uri uri in
    let closefn () = Net.close ic oc in
    lwt req =
      match chunked with
      |true -> return (Request.make ~meth ?headers ?body uri)
      |false ->
         (* If chunked is not allowed, then obtain the body length and insert header *)
         lwt (clen, buf) = Cohttp_lwt_body.get_length body in
         let headers = match headers with
           |None -> Header.(add_transfer_encoding (init ()) (Transfer.Fixed clen))
           |Some h -> Header.(add_transfer_encoding h (Transfer.Fixed clen)) in
         return (Request.make ~meth ~headers ~body uri)
    in
    write_request ?body req oc >>
    read_response ~closefn ic oc

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 

  let post_form ?headers ~params uri =
    let headers = Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
    let body = Cohttp_lwt_body.body_of_string (Uri.encoded_of_query (Header.to_list params)) in
    post ~headers ?body uri

  let callv ?(ssl=false) host port reqs =
    lwt (ic, oc) = Net.connect ~ssl host port in
    (* Serialise the requests out to the wire *)
    Lwt_stream.on_terminate reqs (fun () -> Net.close_out oc);
    let _ = Lwt_stream.iter_s (fun (req,body) -> write_request ?body req oc) reqs in
    (* Read the responses *)
    let resps = Lwt_stream.from (fun () -> read_response ic oc) in
    Lwt_stream.on_terminate resps (fun () -> Net.close_in ic);
    return resps
end

module Server(Request:REQUEST)
             (Response:RESPONSE with type oc = Request.oc and type ic = Request.ic)
             (Net:NET with type oc = Response.oc and type ic = Response.ic)  = struct

  type conn_id = int
  let string_of_conn_id = string_of_int

  type config = {
    callback: conn_id -> ?body:Cohttp_lwt_body.contents -> Request.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t;
    conn_closed : conn_id -> unit -> unit;
  }

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status 
      ~encoding:(Transfer.Fixed (String.length body)) ?headers () in
    let body = Cohttp_lwt_body.body_of_string body in
    return (res,body)

  let respond_error ~status ~body () =
    respond_string ~status ~body:("Error: "^body) ()

  let callback spec =
    let conn_id = ref 0 in
    let daemon_callback ic oc =
      let conn_id = incr conn_id; !conn_id in
      (* Read the requests *)
      let req_stream = Lwt_stream.from (fun () ->
        match_lwt Request.read ic with
        |None -> return None
        |Some req -> begin
          (* Ensure the input body has been fully read before reading again *)
          match Request.has_body req with
          |true ->
            let body_stream = Cohttp_lwt_body.create_stream (Request.read_body req) ic in
            let th,u = task () in
            Lwt_stream.on_terminate body_stream (wakeup u);
            let body = Cohttp_lwt_body.body_of_stream body_stream in
            th >>= fun () -> return (Some (req, body))
          |false -> return (Some (req, None))
        end
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
      for_lwt (res,body) in res_stream do
        Response.write (fun res oc ->
          Cohttp_lwt_body.write_body (Response.write_body res oc) body
        ) res oc
      done
    in daemon_callback
end

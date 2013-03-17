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

open Core.Std
open Async.Std
open Cohttp

include Cohttp_async_raw

(* Convert a HTTP body stream into a Pipe *)
let pipe_of_body read_fn ic =
  let rd, wr = Pipe.create () in
  (* Consume from the input channel and write to the new pipe *)
  let rec write () =
    read_fn ic >>= function
    |Transfer.Done ->
      return (Pipe.close wr);
    |Transfer.Final_chunk c -> begin
      Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn c)
      >>| function
        |`Closed -> ()
        |`Ok _ -> Pipe.close wr
    end
    |Transfer.Chunk c -> begin
      Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn c)
      >>= function
        |`Closed -> return ()
        |`Ok _ -> write () 
    end
  in
  don't_wait_for (write ());
  rd

let close_all ic oc =
  Writer.close oc >>= fun () ->
  Reader.close ic

module Client = struct

  let write_request ?body req oc =
    Request.write (fun req oc ->
      match body with
      |None -> return ()
      |Some b -> Pipe.iter b ~f:(Request.write_body req oc)
    ) req oc

  let read_response ?(close=false) ic oc =
    (* TODO Option.bind is awkward here due to parameter order *)
    Response.read ic >>| function
    |None -> None
    |Some res -> begin
      match Response.has_body res with
      |false -> (Some (res, None))
      |true ->
        let body_rd = pipe_of_body (Response.read_body res) ic in
        if close then don't_wait_for (
            Pipe.closed body_rd >>= fun () -> 
            close_all ic oc
        );
        Some (res, Some body_rd)
    end

  let call ?headers ?body meth uri =
    let encoding =
      match body with 
      |None -> Transfer.Fixed 0
      |Some _ -> Transfer.Chunked in
    let req = Request.make ~meth ~encoding ?headers uri in
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    (* TODO: is there an Async equivalent of Option.Monad in Core? *)
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    |None -> return None
    |Some port ->
      Tcp.(connect (to_host_and_port host port) )
      >>= fun (_,ic,oc) -> write_request ?body req oc 
      >>= fun () -> read_response ~close:true ic oc
end

module Server = struct
  type conn_id = int
  let string_of_conn_id = string_of_int 

  type config = {
    callback: conn_id -> ?body:string Pipe.Reader.t -> Request.t -> (Response.t * string Pipe.Reader.t option) Deferred.t;
    port: int;
  }

  let respond_string ?headers ~status ~body () =
    let res = Response.make ~status 
      ~encoding:(Transfer.Fixed (String.length body)) ?headers () in
    let body_rd, body_wr = Pipe.create () in
    don't_wait_for (
      Pipe.write_when_ready body_wr ~f:(fun wrfn -> wrfn body)
      >>| function |`Closed -> () |`Ok _ -> Pipe.close body_wr);
    return (res, (Some body_rd))
 
  let callback spec =
    let conn_id = ref 0 in
    fun sock ic oc ->
      let conn_id = incr conn_id; !conn_id in
      (* Read the requests as a Pipe *)
      let rd_req, wr_req = Pipe.create () in
      let close () = 
        Pipe.close wr_req; return () in
      let rec read_t () =
        Request.read ic >>= function
        |None -> close ()
        |Some req ->
          (* Ensure the input body has been fully consumed before reading another request *)
          (match Request.has_body req with
           |true ->
             let req_body = pipe_of_body (Request.read_body req) ic in
             Pipe.closed req_body 
             >>= fun () -> return (Some req_body)
           |false -> return None
          )
        >>= fun req_body ->
        Pipe.write_when_ready wr_req (fun wrfn -> wrfn (req, req_body))
        >>= function
        |`Closed -> close_all ic oc (* TODO test *)
        |`Ok _ ->
          if Header.get_connection_close (Request.headers req) then 
            close () 
          else
            read_t ()
      in don't_wait_for (read_t ());
      (* Map the requsts onto a response stream to serialise out *)
      let rec write_resps () =
         Pipe.read rd_req >>= function
         |`Eof -> 
            close_all ic oc
         |`Ok (req, body) ->
            spec.callback conn_id ?body req >>= fun (res,body) ->
            Response.write (fun res oc ->
              match body with
              |None -> return () 
              |Some body ->
                Pipe.iter body ~f:(Response.write_body res oc)
            ) res oc >>= fun () ->
            write_resps ()
      in write_resps ()
     
  let main spec =
    let listen_on = Tcp.on_port spec.port in
    Tcp.Server.create listen_on (callback spec)
end

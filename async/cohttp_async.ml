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

include Cohttp_async_raw

let port_of_uri uri =
  match Uri.port uri with
  |None -> begin
     match Uri.scheme uri with 
     |Some "https" -> 443 (* TODO: actually support https *)
     |Some "http" | Some _ |None -> 80
  end
  |Some p -> p

(* Convert a HTTP body stream into a Pipe *)
let pipe_of_body read_fn ic =
  let rd, wr = Pipe.create () in
  (* Consume from the input channel and write to the new pipe *)
  let rec write () =
    read_fn ic >>= function
    |Transfer.Done ->
      Pipe.close wr; return ()
    |Transfer.Final_chunk c -> begin
      Pipe.with_write wr ~f:(fun wrfn -> wrfn c) >>= 
        function
        |`Closed -> return ()
        |`Ok _ -> Pipe.close wr; return ()
    end
    |Transfer.Chunk c -> begin
      Pipe.with_write wr ~f:(fun wrfn -> wrfn c) >>=
        function
        |`Closed -> return ()
        |`Ok _ -> write () 
    end
  in
  whenever (write ());
  rd

module Client = struct

  type response = Response.response * string Pipe.Reader.t option

  let write_request ?body req oc =
    Request.write (fun req oc ->
      match body with
      |None -> return ()
      |Some b -> Pipe.iter b ~f:(fun c -> Request.write_body c req oc)
    ) req oc

  let read_response ?(close=false) ic oc =
    Response.read ic >>| function
    |None -> None
    |Some res -> begin
      match Response.has_body res with
      |false -> (Some (res, None))
      |true ->
        let body_rd = pipe_of_body (Response.read_body res) ic in
        if close then whenever (
            Pipe.closed body_rd >>= fun () -> 
            Reader.close ic >>= fun () -> 
            Writer.close oc
        );
        Some (res, Some body_rd)
    end

  let call ?headers ?body meth uri =
    let encoding = 
      match body with 
      |None -> Transfer.Fixed 0L 
      |Some _ -> Transfer.Chunked in
    let req = Request.make ~meth ~encoding ?headers uri in
    let host = match Uri.host uri with |None -> "localhost" |Some h -> h in
    let port = port_of_uri uri in
    Tcp.connect ~host ~port () >>= fun (ic,oc) ->
    write_request ?body req oc >>= fun () ->
    read_response ~close:true ic oc
end

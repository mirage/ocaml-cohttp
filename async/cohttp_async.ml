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
open Async_core
open Async_unix

module IO = struct

  type 'a t = 'a Deferred.t
  let (>>=) = Deferred.(>>=)
  let return = Deferred.return

  type ic = Reader.t
  type oc = Writer.t

  let iter fn x =
    Deferred.List.iter x ~f:fn 

  let read_line ic =
    Reader.read_line ic >>=
      function
      |`Ok s -> return (Some s)
      |`Eof -> return None
  
  let read = 
    let buf = String.create 4096 in
    fun ic len ->
      Reader.read ic ~len buf >>=
      function
      |`Ok len' -> return (String.sub buf 0 len')
      |`Eof -> return ""

  let read_exactly ic buf pos len =
    Reader.really_read ic ~pos ~len buf >>=
    function
    |`Ok -> return true
    |`Eof _ -> return false

  let write oc buf =
    Writer.write oc buf;
    return ()

  let write_line oc buf =
    Writer.write oc buf;
    Writer.write oc "\r\n";
    return ()
end

module Body = Transfer.M(IO)
module Request = Request.M(IO)
module Response = Response.M(IO)

let port_of_uri uri =
  match Uri.port uri with
  |None -> begin
     match Uri.scheme uri with 
     |Some "https" -> 443 (* TODO: actually support https *)
     |Some "http" | Some _ |None -> 80
  end
  |Some p -> p

open Deferred
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
  (* TODO: how to run write () as a background task? *)
  let _ = write () in
  rd

module Client = struct

  type response = Response.response * string Lwt_stream.t option

  let write_request ?body req oc =
    Request.write (fun req oc ->
      match body with
      |None -> return ()
      |Some b -> Pipe.iter b ~f:(fun c -> Request.write_body c req oc)
    ) req oc

  let read_response ?(close=false) ic oc =
    Response.read ic >>= function
    |None -> return None
    |Some res -> begin
      match Response.has_body res with
      |false -> return (Some (res, None))
      |true ->
        let body_rd = pipe_of_body (Response.read_body res) ic in
        (* TODO: how to run this as a background task properly? *)
        let _ = 
          if close then  (
            Pipe.closed body_rd >>= fun () -> 
            Reader.close ic >>= fun () -> 
            Writer.close oc)
          else return () in
        return (Some (res, Some body_rd))
    end

  let call ?headers ?body meth uri =
    let encoding = 
      match body with 
      |None -> Transfer.Fixed 0L 
      |Some _ -> Transfer.Chunked in
    let req = Request.make ~meth ~encoding ?headers uri in
    let host = match Uri.host uri with |None -> "localhost" |Some h -> h in
    let port = port_of_uri uri in
    Async_extra.Tcp.connect ~host ~port () >>= fun (ic,oc) ->
    write_request ?body req oc >>= fun () ->
    read_response ~close:true ic oc
end

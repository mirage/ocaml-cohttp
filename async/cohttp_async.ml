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
module Parser = Parser.M(IO)
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
module Client = struct

  let call ?headers ?body meth uri =
    let headers =
      match headers with
      |None -> Header.init ()
      |Some h -> h in
    let encoding = 
      match body with 
      |None -> Transfer.Fixed 0L 
      |Some _ -> Transfer.Chunked in
    let req = Request.make ~meth ~encoding headers uri in
    let host = match Uri.host uri with |None -> "localhost" |Some h -> h in
    let port = port_of_uri uri in
    let fn ic oc =
      Request.write (fun _ _ -> return ()) req oc >>= fun () ->
      Response.read ic >>= function
      |None ->
         return None
      |Some res -> 
         let r,w = Pipe.create () in
         
         return (Some (res,pipe))
    in
    Async_extra.Tcp.with_connection ~host ~port fn

end

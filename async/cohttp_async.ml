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

open Core.Std
open Async.Std

module IO = struct

  type 'a t = 'a Deferred.t
  let (>>=) = Deferred.(>>=)
  let (>>) m n = m >>= fun _ -> n
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

module Net = struct
  let connect ~uri ~f =
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    |None -> f `Unknown_service
    |Some port ->
      Tcp.with_connection (Tcp.to_host_and_port host port)
        (fun _ ic oc -> f (`Ok (ic,oc)))
end

module Response = Cohttp.Response.Make(IO)
module Request = Cohttp.Request.Make(IO)
module Client = struct
  include Cohttp.Client.Make(IO)(Request)(Response)

  let call ?headers ?(chunked=false) ?body meth uri =
    let response_body = body in
    let open Response.State_types.PStateIO in
    let ivar = Ivar.create () in
    let response resp =
      get >>= fun `Waiting_for_response ->
      let rd,wr = Pipe.create () in
      Ivar.fill ivar (resp, rd);
      put (`Getting_body wr)
    and body buf =
      get >>= function
      | `Getting_body wr ->
        lift (Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf))
        >>= (function
          |`Closed -> (* Junk rest of the body *)
            put `Junking_body
          |`Ok _ ->
            return ())
      | `Junking_body ->
        return ()
    and body_end =
      get >>= function
      | `Getting_body wr ->
        Pipe.close wr;
        put `Complete
      |`Junking_body ->
        put `Complete
    and failure =
      put `Die
    in
    let open IO in
    Net.connect ~uri ~f:(function
    |`Unknown_service -> return None
    |`Ok (ic,oc) ->
      (* Establish the remote HTTP connection *)
      call ?headers ~chunked ?body:response_body meth uri
        {body; body_end; failure; response} ic oc
      >>= fun () ->
      Ivar.read ivar >>= fun x -> 
      return (Some x)
    )
end

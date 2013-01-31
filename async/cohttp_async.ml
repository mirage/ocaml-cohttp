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
        (fun ic oc -> f (`Ok (ic,oc)))
end

module Client = struct

  include Cohttp.Client.Make
      (IO)
      (Cohttp.Request.Make(IO))
      (Cohttp.Response.Make(IO))

  let call ?headers ?(chunked=false) ?body meth uri =
    let ivar = Ivar.create () in
    let state = ref `Waiting_for_response in
    let signal_handler s =
      match !state,s with
      |`Waiting_for_response, `Response resp ->
        let rd,wr = Pipe.create () in
        state := `Getting_body wr;
        Ivar.fill ivar (resp, rd);
        return ()
      |`Getting_body wr, `Body buf ->
        Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
        >>= (function
        |`Closed -> (* Junk rest of the body *)
          state := `Junking_body;
          return ()
        |`Ok _ -> return ())
      |`Getting_body wr, `Body_end ->
        state := `Complete;
        Pipe.close wr;
        return ()
      |`Junking_body, `Body _ -> return ()
      |`Junking_body, `Body_end ->
        state := `Complete;
        return ()
      |`Waiting_for_response, `Body _
      |`Waiting_for_response, `Body_end
      |_, `Failure
      |`Junking_body, `Response _
      |`Getting_body _, `Response _ ->
        (* TODO warning and non-fatal *)
        assert false
      |`Complete, _ -> return ()
    in 
    Net.connect ~uri ~f:(function
    |`Unknown_service -> return None
    |`Ok (ic,oc) ->
      (* Establish the remote HTTP connection *)
      call ?headers ~chunked ?body meth uri signal_handler ic oc
      >>= fun () ->
      Ivar.read ivar >>= fun x -> 
      return (Some x)
    )
end

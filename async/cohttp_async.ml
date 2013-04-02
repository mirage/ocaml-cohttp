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

module IO = Cohttp_async_io

module Response = Cohttp.Response.Make(IO)
module Request = Cohttp.Request.Make(IO)

module Net =
struct
  let connect ~uri ~f =
    let ctx = Fable_async.init () in
    Monitor.try_with (fun () -> Fable_async.connect ~ctx ~uri) >>= function
      | Error exn -> f `Unknown_service (* TODO: propagate the error *)
      | Ok (flow_state, reader, writer) -> f (`Ok (reader, writer))
end

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
      let ic, oc = IO.create ic oc in
      (* Establish the remote HTTP connection *)
      call ?headers ~chunked ?body:response_body meth uri
      Response.State_types.({body; body_end; failure; response}) ic oc >>= fun () ->
      Ivar.read ivar >>= fun x -> 
      return (Some x)
    )
end

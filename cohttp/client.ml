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

module type S = sig
  module IO : IO.S
  val call :
    ?headers:Header.t ->
    ?chunked:bool ->
    ?body:(unit -> string option) ->
    Code.meth ->
    Uri.t ->
    IO.ic -> IO.oc -> Response.r
end

(*
module Make
  (IO : IO.S)
  (Request:Request.S with module IO=IO)
  (Response:Response.S with module IO=IO) = struct
  open IO

  let read_response {Response.State_types.body; body_end; failure; response} ic =
    let open Response.State_types.PStateIO in
    lift (Response.read ic)
    >>= function
    |None -> failure
    |Some res -> begin
      response res >>= fun () -> 
      match Response.has_body res with
      |false -> 
        body_end
      |true -> 
        Response.read_body res body ic >>= fun () -> 
        body_end >>= fun () -> 
        put `Complete
    end

  let run_response response = 
    Response.State_types.PStateIO.run response `Waiting_for_response >>=
    fun (result, _) -> return result

  let call ?headers ?(chunked=false) ?body meth uri (signal:(_, _) Response.State_types.response_handler) ic oc =
    match body with
    |None ->
       let encoding = Transfer.Fixed 0 in
       let req = Request.make ~meth ~encoding ?headers ~body uri in
       Request.write' req (fun () -> None) oc >>= fun () -> 
       run_response (read_response signal ic)
    |Some body -> begin
       match chunked with
       |true ->
         let req = Request.make ~meth ?headers ~body uri in
         Request.write' req (fun () -> None) oc >>= fun () ->
         run_response (read_response signal ic)
       |false ->
         (* If chunked is not allowed, then call [body_fn] once insert length header *)
         match body () with
         |None ->
           let encoding = Transfer.Fixed 0 in
           let req = Request.make ~meth ~encoding ?headers ~body uri in
           Request.write' req body oc >>= fun () -> 
           run_response (read_response signal ic)
         |Some buf ->
           let clen = String.length buf in
           let encoding = Transfer.Fixed clen in
           let req = Request.make ~meth ~encoding ?headers ~body uri in
           Request.write' req body oc >>= fun () -> 
           run_response (read_response signal ic)
    end

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PUT uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 
end
*)

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

module Make
  (IO:Make.IO)
  (Request:Make.REQUEST with type oc = IO.oc and type ic = IO.ic and type 'a io = 'a IO.t)
  (Response:Make.RESPONSE with type oc = Request.oc and type ic = Request.ic and type 'a io = 'a Request.io) = struct
  open IO


  type input_signal = [
    | `Failure
    | `Response of Response.t
    | `Body of string
    | `Body_end
  ]

  type signal_handler = (input_signal -> unit IO.t)

  let read_response signal ic =
    Response.read ic
    >>= function
    |None -> 
      signal `Failure
    |Some res -> begin
      signal (`Response res) >>= fun () ->
      match Response.has_body res with
      |false -> 
        signal `Body_end
      |true -> 
        Response.read_body res
          (function 
           |None -> signal `Body_end
           |Some b -> signal (`Body b)
          ) ic
    end

  let call ?headers ?(chunked=false) ?body meth uri (signal:signal_handler) ic oc =
    match body with
    |None ->
       let encoding = Transfer.Fixed 0 in
       let req = Request.make ~meth ~encoding ?headers ~body uri in
       Request.write req (fun _ -> None) oc >>= fun () ->
       read_response signal ic
    |Some body -> begin
       match chunked with
       |true ->
         let req = Request.make ~meth ?headers ~body uri in
         Request.write req body oc >>= fun () ->
         read_response signal ic
       |false ->
         (* If chunked is not allowed, then call [body_fn] once insert length header *)
         match body () with
         |None ->
           let encoding = Transfer.Fixed 0 in
           let req = Request.make ~meth ~encoding ?headers ~body uri in
           Request.write req body oc >>= fun () ->
           read_response signal ic
         |Some buf ->
           let clen = String.length buf in
           let encoding = Transfer.Fixed clen in
           let req = Request.make ~meth ~encoding ?headers ~body uri in
           Request.write req body oc >>= fun () ->
           read_response signal ic
    end

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PUT uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 
end


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

module Client(Request:Make.REQUEST)
             (Response:Make.RESPONSE with type oc = Request.oc and type ic = Request.ic) = struct
  let write_request body_fn req oc =
    Request.write (fun req oc ->
      let rec aux () =
        match body_fn () with
        |true -> IO.return ()
        |false -> aux ()
      in aux ()
    )

  let read_response res_fn ic oc =
    Response.read ic >>= function
    |None -> return None
    |Some res -> begin
      match Response.has_body res with
      |true ->
        let rec aux () =
          Response.read_body res ic
          >>= function
          |Transfer.Final_chunk b ->
             res_fn res b;
             IO.return ()
          |Transfer.Chunk b -> 
             res_fn res b;
             aux ()
          |Transfer.Done ->
             IO.return ()
        in aux ()
     end
     |false -> IO.return ()
 
  let call ?headers ?(chunked=false) ?body meth uri res_fn ic oc =
    match body with
    |None ->
       let req = Request.make ~meth ?headers ?body uri in
       write_request req oc >>= fun () ->
       read_response res_fn ic ic
    |Some body -> begin
       match chunked with
       |true ->
         let req = Request.make ~meth ?headers ~body uri in
         write_request req oc >>= fun () ->
         (* Write the request body *)
         (* TODO *)
         read_response res_fn ic oc
       |false ->
         (* If chunked is not allowed, then call [body_fn] once insert length header *)
         body () >>= fun buf ->
         let clen = Cstruct.len buf in
         let headers = match headers with |None -> init () |Some h -> h in
         let headers = add_transfer_encoding h (Transfer.Fixed clen) in
         let req = Request.make ~meth ~headers ~body uri in
         write_requst req oc >>= fun () ->
         read_response res_fn ic oc
    end

  let head ?headers uri = call ?headers `HEAD uri 
  let get ?headers uri = call ?headers `GET uri 
  let delete ?headers uri = call ?headers `DELETE uri 
  let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri 
  let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PUT uri 
  let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri 
end


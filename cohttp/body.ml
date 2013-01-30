(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
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

module Make (IO:Make.IO) = struct
  module TIO = Transfer_io.Make(IO)
  type ic = IO.ic
  type oc = IO.oc
  type 'a io = 'a IO.t
  open IO

  let read encoding fn ic = 
    let rec aux () =
      TIO.read encoding ic
      >>= function
      |Transfer.Done -> fn None
      |Transfer.Final_chunk b -> fn (Some b) >>= fun () -> fn None
      |Transfer.Chunk b -> fn (Some b) >>= fun () -> aux () 
    in aux () 

  let write fn encoding oc =
    let rec aux () =
      match fn () with
      |Some buf ->
         IO.write oc buf >>= fun () ->
         aux ()
      |None -> IO.return ()
    in 
    aux () >>= fun () ->
    match encoding with
    |Transfer.Chunked ->
       (* TODO Trailer header support *)
       IO.write oc "0\r\n\r\n"
    |Transfer.Fixed _ 
    |Transfer.Unknown -> return ()

end

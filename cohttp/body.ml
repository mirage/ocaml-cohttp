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

module type S = sig
  module IO : IO.S
  module StateTypes : StateTypes.S with module IO = IO
  val read : Transfer.encoding -> IO.ic -> StateTypes.chunk_reader ->
             ([ `Working ], [> `Finished ], unit) StateTypes.PStateIO.t
  val write : Transfer.encoding -> (unit -> string option) -> IO.oc -> unit IO.t
end

module Make (IO : IO.S) : S with module IO = IO = struct
  module IO = IO
  open IO
  module TIO = Transfer_io.Make(IO)

  module StateTypes = StateTypes.Make(IO)
  open StateTypes

  let read encoding ic {process} = 
    let open PStateIO in
    let rec aux () =
      lift (TIO.read encoding ic)
      >>= fun chunk -> match chunk with
      | Transfer.Done ->
	process Done
      | Transfer.Final_chunk b ->
	process (Chunk b) >>= fun () ->
	process Done
      | Transfer.Chunk b ->
	process (Chunk b) >>= aux
    in aux () 

  let write encoding fn oc =
    let rec aux () =
      match fn () with
      |Some buf -> IO.write oc buf >>= aux
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

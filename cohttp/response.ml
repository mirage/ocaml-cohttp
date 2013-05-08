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

type r = {
  encoding: Transfer.encoding;
  headers: Header.t;
  version: Code.version;
  status: Code.status_code;
}

module type S = sig
  module IO : IO.S
  type t = r

  val version : t -> Code.version
  val status : t -> Code.status_code
  val headers: t -> Header.t

  val make : ?version:Code.version -> ?status:Code.status_code ->
    ?encoding:Transfer.encoding -> ?headers:Header.t -> unit -> t

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body_chunk :
    t -> IO.ic -> Transfer.chunk IO.t

  val is_form: t -> bool
  val read_form : t -> IO.ic -> (string * string list) list IO.t

  val write_header : t -> IO.oc -> unit IO.t
  val write_body : t -> IO.oc -> string -> unit IO.t
  val write_footer : t -> IO.oc -> unit IO.t
  val write : (t -> IO.oc -> unit IO.t) -> t -> IO.oc -> unit IO.t
end

module Make(IO : IO.S) = struct
  module IO = IO
  open IO

  module Header_IO = Header_io.Make(IO)
  module Body_IO = Body.Make(IO)
  module Transfer_IO = Transfer_io.Make(IO)

  type t = r
  let version r = r.version
  let status r = r.status
  let headers r = r.headers

  let make ?(version=`HTTP_1_1) ?(status=`OK) ?(encoding=Transfer.Chunked) ?headers () =
    let headers = match headers with None -> Header.init () |Some h -> h in
    { encoding; headers; version; status }

  let pieces_sep = Re_str.regexp_string " "
  let header_sep = Re_str.regexp ": *"
  
  let parse_response_fst_line ic =
    let open Code in
    read_line ic >>= function
    |Some response_line -> begin
      match Re_str.split_delim pieces_sep response_line with
      | version_raw :: code_raw :: _ -> begin
         match version_of_string version_raw with
         |Some v -> return (Some (v, (status_of_code (int_of_string code_raw))))
         |_ -> return None
      end
      | _ -> return None
    end
    |None -> return None
 
  let read ic =
    parse_response_fst_line ic >>= function
    |None -> return None
    |Some (version, status) ->
       Header_IO.parse ic >>= fun headers ->
       let encoding = Header.get_transfer_encoding headers in
       return (Some { encoding; headers; version; status })

  let has_body {encoding} = Transfer.has_body encoding
  let read_body_chunk {encoding} ic = Transfer_IO.read encoding ic

  let write_header res oc =
    write oc (Printf.sprintf "%s %s\r\n" (Code.string_of_version res.version) 
      (Code.string_of_status res.status)) >>= fun () ->
    let headers = Header.add_transfer_encoding res.headers res.encoding in
    iter (IO.write oc) (Header.to_lines headers) >>= fun () ->
    IO.write oc "\r\n"

  let write_body {encoding} oc buf =
    Transfer_IO.write encoding oc buf

  let write_footer {encoding} oc =
    match encoding with
    |Transfer.Chunked ->
       (* TODO Trailer header support *)
       IO.write oc "0\r\n\r\n"
    |Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write fn req oc =
    write_header req oc >>= fun () ->
    fn req oc >>= fun () ->
    write_footer req oc

  let is_form req = Header.is_form req.headers
  let read_form req ic = Header_IO.parse_form req.headers ic
end

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

module type S = sig
  module IO : IO.S
  module StateTypes : StateTypes.S with module IO = IO
  type t
  val meth : t -> Code.meth
  val uri : t -> Uri.t
  val version : t -> Code.version

  val path : t -> string
  val header : t -> string -> string option
  val headers : t -> Header.t

  val params : t -> (string * string list) list
  val get_param : t -> string -> string option

  val transfer_encoding : t -> string

  val make : ?meth:Code.meth -> ?version:Code.version -> 
    ?encoding:Transfer.encoding -> ?headers:Header.t ->
    ?body:'a -> Uri.t -> t

  val read : IO.ic -> t option IO.t
  val has_body : t -> bool
  val read_body :
    t -> StateTypes.chunk_handler -> IO.ic ->
    ([ `Working ], [> `Finished ], unit) StateTypes.PStateIO.t

  val write : t -> (unit -> string option) -> IO.oc -> unit IO.t
end

module Make(IO : IO.S) = struct
  module IO = IO
  open IO

  module Header_IO = Header_io.Make(IO)
  module Body_IO = Body.Make(IO) 
  module StateTypes = Body_IO.StateTypes
  
  type t = { 
    headers: Header.t;
    meth: Code.meth;
    uri: Uri.t;
    version: Code.version;
    encoding: Transfer.encoding;
  }
  
  let meth r = r.meth
  let uri r = r.uri
  let version r = r.version
  let path r = Uri.path r.uri
  
  let header req h = Header.get req.headers h
  let headers req = req.headers
  
  let params r = Uri.query r.uri
  let get_param r k =
    try Some (List.(hd (assoc k (params r))))
    with _ -> None

  let transfer_encoding req = Transfer.encoding_to_string req.encoding

  let url_decode url = Uri.pct_decode url

  let pieces_sep = Re_str.regexp_string " "
  let parse_request_fst_line ic =
    let open Code in
    read_line ic >>= function
    |Some request_line -> begin
      match Re_str.split_delim pieces_sep request_line with
      | [ meth_raw; uri_raw; http_ver_raw ] -> begin
          match method_of_string meth_raw, version_of_string http_ver_raw with
          |Some m, Some v -> return (Some (m, (Uri.of_string uri_raw), v))
          |_ -> return None
      end
      | _ -> return None
    end
    |None -> return None

  let read ic =
    parse_request_fst_line ic >>= function
    |None -> return None
    |Some (meth, uri, version) ->
      Header_IO.parse ic >>= fun headers -> 
      let encoding = Header.get_transfer_encoding headers in
      return (Some { headers; meth; uri; version; encoding })

  let has_body req = Transfer.has_body req.encoding
  let read_body req fn ic = Body_IO.read req.encoding ic fn

  let host_of_uri uri = 
    match Uri.host uri with
    |None -> "localhost"
    |Some h -> h

  let make ?(meth=`GET) ?(version=`HTTP_1_1) ?encoding ?headers ?body uri =
    let headers = 
      match headers with
      |None -> Header.init ()
      |Some h -> h in
    let encoding =
      match encoding with
      |None -> begin
        (* Check for a content-length in the supplied headers first *)
        match Header.get_content_range headers with
        |Some clen -> Transfer.Fixed clen
        |None -> begin
          match body with 
          |None -> Transfer.Fixed 0 
          |Some _ -> Transfer.Chunked 
        end
      end
      |Some e -> e
    in
    { meth; version; headers; uri; encoding }

  let write_header req oc =
   let fst_line = Printf.sprintf "%s %s %s\r\n" (Code.string_of_method req.meth)
      (Uri.path_and_query req.uri) (Code.string_of_version req.version) in
    let headers = Header.add req.headers "host" (host_of_uri req.uri) in
    let headers = Header.add_transfer_encoding headers req.encoding in
    IO.write oc fst_line >>= fun () ->
    iter (IO.write oc) (Header.to_lines headers) >>= fun () ->
    IO.write oc "\r\n"

  let write req fn oc =
    write_header req oc >>= fun () ->
    Body_IO.write req.encoding fn oc
end

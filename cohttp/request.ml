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

module Make(IO:Make.IO) = struct

  module Header_IO = Header_io.Make(IO)
  module Transfer_IO = Transfer_io.Make(IO)
  type ic = IO.ic
  type oc = IO.oc
  type 'a io = 'a IO.t
  open IO
  let (>>=) = (>>=)

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
  let read_body req fn ic = 
    let rec aux () =
      Transfer_IO.read req.encoding ic
      >>= function
      |Transfer.Done -> fn None; return ()
      |Transfer.Final_chunk b -> fn (Some b); fn None; return ()
      |Transfer.Chunk b -> fn (Some b); aux () 
    in aux () 

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

  let write_body req oc buf =
    Transfer_IO.write req.encoding oc buf

  let write_footer req oc =
    match req.encoding with
    |Transfer.Chunked ->
       (* TODO Trailer header support *)
       IO.write oc "0\r\n\r\n"
    |Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write fn req oc =
    let rec aux () =
      match fn () with
      |Some buf ->
         IO.write oc buf >>= fun () ->
         aux ()
      |None -> IO.return ()
    in 
    write_header req oc >>= fun () ->
    aux () >>= fun () ->
    write_footer req oc
end

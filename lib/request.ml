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

module M (IO:IO.M) = struct

  module Header_IO = Header.M(IO)
  module Transfer_IO = Transfer.M(IO)
  open IO

  type request = { 
    headers: Header.t;
    meth: Code.meth;
    uri: Uri.t;
    get: Header.t;
    post: Header.t;
    version: Code.version;
    encoding: Transfer.encoding;
  }
  
  let meth r = r.meth
  let uri r = r.uri
  let version r = r.version
  let path r = Uri.path r.uri
  
  let header req h = Header.get req.headers h
  let headers req = req.headers
  
  let params_get r = r.get
  let params_post r = r.post
  let param r p = (Header.get r.post p) @ (Header.get r.get p)

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
      let ctype = Header.get_media_type headers in
      let get = Header.of_list (Uri.query uri) in
      (match meth, ctype with
        |`POST, Some "application/x-www-form-urlencoded" -> 
          (* If the form is query-encoded, then extract those parameters also *)
          let bodylen = match Header.get_content_range headers with
             |None -> 0 |Some x -> x in
          IO.read ic bodylen >>= fun query ->
          let post = Header.of_list (Uri.query_of_encoded query) in
          let encoding = Transfer.Fixed 0L in
          return (post, encoding)
        |`POST, _ ->
          let post = Header.init () in
          let encoding = Transfer.parse_transfer_encoding headers in
          return (post, encoding)
        | _ -> return ((Header.init ()), (Transfer.Fixed 0L)) 
      ) >>= fun (post, encoding) ->
      return (Some { headers; meth; uri; version; post; get; encoding })

  let has_body req = Transfer.has_body req.encoding
  let read_body req ic = Transfer_IO.read req.encoding ic

  let host_of_uri uri = 
    match Uri.host uri with
    |None -> "localhost"
    |Some h -> h

  let make ?(meth=`GET) ?(version=`HTTP_1_1) ?(encoding=Transfer.Chunked) ?headers uri =
    let headers = 
      match headers with
      |None -> Header.init ()
      |Some h -> h in
    let get = Header.of_list (Uri.query uri) in
    let post = Header.init () in
    { meth; version; headers; get; post; uri; encoding }

  let write_header req oc =
   let fst_line = Printf.sprintf "%s %s %s\r\n" (Code.string_of_method req.meth)
      (Uri.path_and_query req.uri) (Code.string_of_version req.version) in
    let headers = Header.add req.headers "host" (host_of_uri req.uri) in
    let headers = Transfer.add_encoding_headers headers req.encoding in
    IO.write oc fst_line >>= fun () ->
    let headers = Header.fold (fun k v acc -> 
      Printf.sprintf "%s: %s\r\n" k v :: acc) headers [] in
    iter (IO.write oc) (List.rev headers) >>= fun () ->
    IO.write oc "\r\n"

  let write_body buf req oc =
    Transfer_IO.write req.encoding oc buf

  let write_footer req oc =
    match req.encoding with
    |Transfer.Chunked ->
       (* TODO Trailer header support *)
       IO.write oc "0\r\n\r\n"
    |Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write fn req oc =
    write_header req oc >>= fun () ->
    fn req oc >>= fun () ->
    write_footer req oc
end

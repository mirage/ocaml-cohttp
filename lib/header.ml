(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011-2012 Martin Jambon <martin@mjambon.com>
 * Copyright (c) 2010 Mika Illouz
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

module StringMap = Map.Make(String)
type t = string StringMap.t

let init () = StringMap.empty
let add h k v = StringMap.add k v h
let remove h k = StringMap.remove k h
let get h k = try Some (StringMap.find k h) with Not_found -> None
let map fn h = StringMap.mapi fn h
let iter fn h = ignore(map fn h)
let fold fn h acc = StringMap.fold fn h acc
let of_list l = List.fold_left (fun a (k,v) -> StringMap.add k v a) StringMap.empty l
let to_list h = StringMap.fold (fun k v acc -> (k,v)::acc) h []

let parse_content_range s =
  try
    let start, fini, total = 
      Scanf.sscanf s "bytes %d-%d/%d" (fun start fini total -> start, fini, total) in
    Some (start, fini, total)
  with Scanf.Scan_failure _ -> None
  
(* If we see a "Content-Range" header, than we should limit the
   number of bytes we attempt to read *)
let get_content_range headers = 
  match get headers "content-length" with
  | Some clen -> (try Some (int_of_string clen) with _ -> None)
  | None -> begin
    match get headers "content-range" with
    | Some range_s -> begin
      match parse_content_range range_s with
      | Some (start, fini, total) ->
        (* some sanity checking before we act on these values *)
        if fini < total && start <= total && 0 <= start && 0 <= total then (
          let num_bytes_to_read = fini - start + 1 in
          Some num_bytes_to_read
        ) else None
      | None -> None
    end
    | None -> None
  end
  
let get_media_type =
  (* Grab "foo/bar" from " foo/bar ; charset=UTF-8" *)
  let media_type_re = Re_str.regexp "[ \t]*\\([^ \t;]+\\)" in
  fun headers ->
    match get headers "content-type" with
    | Some s ->
      if Re_str.string_match media_type_re s 0 then
        Some (Re_str.matched_group 1 s)
      else
        None
    | None -> None

let get_acceptable_media_ranges headers =
  Accept.media_ranges (get headers "accept")

let get_acceptable_charsets headers =
  Accept.charsets (get headers "accept-charset")

let get_acceptable_encodings headers =
  Accept.encodings (get headers "accept-encoding")

let get_acceptable_languages headers =
  Accept.languages (get headers "accept-language")

(* Parse the transfer-encoding and content-length headers to
 * determine how to decode a body *)
let get_transfer_encoding headers =
  match get headers "transfer-encoding" with
  | Some "chunked" -> Transfer.Chunked
  | Some _ | None -> begin
    match get_content_range headers with
    |Some len -> Transfer.Fixed (Int64.of_int len)
    |None -> Transfer.Unknown
  end

let add_transfer_encoding headers = 
  function
  |Transfer.Chunked -> add headers "transfer-encoding" "chunked"
  |Transfer.Fixed len -> add headers "content-length" (Int64.to_string len)
  |Transfer.Unknown -> headers
 
let is_form headers =
  get_media_type headers = (Some "application/x-www-form-urlencoded")

module M(IO:IO.M) = struct
  open IO
  module Transfer_IO = Transfer.M(IO)

  let header_sep = Re_str.regexp ": *"
  let parse ic =
    (* consume also trailing "^\r\n$" line *)
    let rec parse_headers' headers =
      read_line ic >>= function
      |Some "" | None -> return headers
      |Some line -> begin
          match Re_str.bounded_split_delim header_sep line 2 with
          | [hd;tl] ->
              let header = String.lowercase hd in
              parse_headers' (add headers header tl);
          | _ -> return headers
      end
    in parse_headers' (init ())

  let parse_form headers ic =
    (* If the form is query-encoded, then extract those parameters also *)
    let encoding = get_transfer_encoding headers in
    Transfer_IO.to_string encoding ic >>= fun body ->
    return (Uri.query_of_encoded body)
end

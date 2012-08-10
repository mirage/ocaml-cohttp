(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> Anil Madhavapeddy <anil@recoil.org>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Printf
open Code

module M (IO:IO.M) = struct
  open IO
  
  let bindings_sep = Re_str.regexp_string "&"
  let binding_sep = Re_str.regexp_string "="
  let pieces_sep = Re_str.regexp_string " "
  let header_sep = Re_str.regexp ": *"
  
  let url_decode url = Uri.pct_decode url
  
  let parse_request_fst_line ic =
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
  
  let parse_response_fst_line ic =
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
  
  let parse_headers ic =
    (* consume also trailing "^\r\n$" line *)
    let headers = Header.init () in
    let rec parse_headers' () =
      read_line ic >>= function
      |Some "" | None -> return headers
      |Some line -> begin
          match Re_str.bounded_split_delim header_sep line 2 with
          | [hd;tl] -> 
              let header = String.lowercase hd in
              Header.add headers header tl;
              parse_headers' ()
          | _ -> return headers
      end
    in parse_headers' ()
  
  let parse_content_range s =
    try
      let start, fini, total = Scanf.sscanf s "bytes %d-%d/%d" 
        (fun start fini total -> start, fini, total) in
      Some (start, fini, total)
    with Scanf.Scan_failure _ -> None
  
  (* If we see a "Content-Range" header, than we should limit the
     number of bytes we attempt to read *)
  let parse_content_range headers = 
    (* assuming header keys were downcased in previous step *)
    match Header.get headers "content-length" with
    |clen::_ -> (try Some (int_of_string clen) with _ -> None)
    |_ -> begin
      match Header.get headers "content-range" with
      |range_s::_ -> begin
        match parse_content_range range_s with
        |Some (start, fini, total) ->
          (* some sanity checking before we act on these values *)
          if fini < total && start <= total && 0 <= start && 0 <= total then (
            let num_bytes_to_read = fini - start + 1 in
            Some num_bytes_to_read
          ) else None
        |None -> None
      end
      |_ -> None
   end
  
  let media_type_re =
    (* Grab "foo/bar" from " foo/bar ; charset=UTF-8" *)
    Re_str.regexp "[ \t]*\\([^ \t;]+\\)"
  
  let parse_media_type s =
    if Re_str.string_match media_type_re s 0 then
      Some (Re_str.matched_group 1 s)
    else
      None
  
end

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2012> Anil Madhavapeddy <anil@recoil.org>
  Copyright (C) <2009> David Sheets <sheets@alum.mit.edu>

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

(* We need a non-Unix date/time implementation to support other expiration
   types *)
type expiration = [ `Session ]

type cookie = string * string

module Set_cookie_hdr = struct
	type t = {
		cookie: cookie;
		expiration : expiration;
		domain : string option;
		path : string option;
		secure : bool }

	(* Does not check the contents of name or value for ';', ',', '\s', or name[0]='$' *)
	let make ?(expiry=`Session) ?path ?domain ?(secure=false) cookie =
	  { cookie = cookie;
		expiration = expiry; domain = domain;
		path = path; secure = secure }
    
	let serialize_1_1 c =
	  let attrs = ["Version=1"] in
	  let attrs = if c.secure then ("Secure" :: attrs) else attrs in
	  let attrs = match c.path with None -> attrs
	    | Some p -> ("Path=" ^ p) :: attrs in
	  let attrs = match c.expiration with
	    | `Session -> "Discard" :: attrs in
	  let attrs = match c.domain with None -> attrs
	    | Some d -> ("Domain=" ^ d) :: attrs in
	    ("Set-Cookie2", String.concat "; " attrs)
  
	let serialize_1_0 c =
	  let attrs = if c.secure then ["secure"] else [] in
	  let attrs = match c.path with None -> attrs
	    | Some p -> ("path=" ^ p) :: attrs in
	  let attrs = match c.domain with None -> attrs
	    | Some d -> ("domain=" ^ d) :: attrs in
	  let attrs = match c.expiration with
	    | `Session -> attrs in
	  let n, c = c.cookie in
	  let attrs = (n ^ (match c with "" -> ""
			      | v -> "=" ^ v)) :: attrs in
	    ("Set-Cookie", String.concat "; " attrs)

	let serialize ?(version=`HTTP_1_0) c =
	  match version with
	    | `HTTP_1_0 -> serialize_1_0 c
	    | `HTTP_1_1 -> serialize_1_1 c
end

module Cookie_hdr = struct
	let cookie_re = Re_str.regexp "(\\?:;\\|,)([ \t])"
	let equals_re = Re_str.regexp_string "="

	let extract hdr =
	  List.fold_left
	    (fun acc header ->
	        let comps = Re_str.split_delim cookie_re header in
	        let cookies = List.filter (fun s -> s.[0] != '$') comps in
	        let split_pair nvp =
	          match Re_str.split_delim equals_re nvp with
	          | [] -> ("","")
	          | n :: [] -> (n, "")
	          | n :: v :: _ -> (n, v)
	        in (List.map split_pair cookies) @ acc
	    ) [] (Header.get_multi hdr "Cookie")
end

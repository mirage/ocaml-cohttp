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
  let make ?(expiration=`Session) ?path ?domain ?(secure=false) cookie =
    { cookie ; expiration ; domain ; path ; secure }
    
  (* TODO: deprecated by RFC 6265 and almost certainly buggy without
     reference to cookie field *)
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
    (* TODO: may be buggy, some UAs will ignore cookie-strings without '='*)
    let attrs = (n ^ (match c with "" -> ""
            | v -> "=" ^ v)) :: attrs in
      ("Set-Cookie", String.concat "; " attrs)

  let serialize ?(version=`HTTP_1_0) c =
    match version with
      | `HTTP_1_0 -> serialize_1_0 c
      | `HTTP_1_1 -> serialize_1_1 c

  (* TODO: implement *)
  let extract_1_1 cstr alist = alist

  let extract_1_0 cstr alist =
    let attrs = Re_str.split_delim (Re_str.regexp ";[ \t]*") cstr in
    let attrs = List.map (fun attr ->
      match Re_str.split_delim (Re_str.regexp_string "=") attr with
        | [] -> ("","")
        | n::v -> (n,String.concat "=" v)
    ) attrs in
    try
      let cookie = List.hd attrs in
      let attrs = List.map (fun (n,v) -> (String.lowercase n, v))
        (List.tl attrs) in
      let path =
        try
          let v = List.assoc "path" attrs in
          if v = "" || v.[0] <> '/'
          then raise Not_found
          else Some v
        with Not_found -> None
      in
      let domain =
        try
          let v = List.assoc "domain" attrs in
          if v = "" then raise Not_found
          else Some
            (String.lowercase
               (if v.[0] = '.' then Re_str.string_after v 1 else v))
        with Not_found -> None
      in
      (* TODO: trim wsp *)
      (fst cookie, {
        cookie;
        (* TODO: respect expires attribute *)
        expiration = `Session;
        domain;
        path;
        secure = List.mem_assoc "secure" attrs;
      })::alist
    with (Failure "hd") -> alist

  (* TODO: check dupes+order *)
  let extract hdr =
    Header.fold (function
      | "set-cookie" -> extract_1_0
      | "set-cookie2" -> extract_1_1
      | _ -> (fun _ a -> a)
    ) hdr []

  let cookie { cookie } = cookie
  let expiration { expiration } = expiration
  let domain { domain } = domain
  let path { path } = path
  let secure { secure } = secure
  let value { cookie=(_,v) } = v
end

module Cookie_hdr = struct
  (* RFC 2965 has
    cookie          =  "Cookie:" cookie-version 1*((";" | ",") cookie-value)
    cookie-value    =  NAME "=" VALUE [";" path] [";" domain] [";" port]
    cookie-version  =  "$Version" "=" value
    NAME            =  attr
    VALUE           =  value
    path            =  "$Path" "=" value
    domain          =  "$Domain" "=" value
    port            =  "$Port" [ "=" <"> value <"> ]
  *)

  let cookie_re = Re_str.regexp "[;,][ \t]*"
  let equals_re = Re_str.regexp_string "="

  let extract hdr =
    List.fold_left
      (fun acc header ->
          let comps = Re_str.split_delim cookie_re header in
          (* We don't handle $Path, $Domain, $Port, $Version (or $anything
             $else) *)
          let cookies = List.filter (fun s -> s.[0] != '$') comps in
          let split_pair nvp =
            (* TODO: This is buggy for cookies with '=' in values *)
            match Re_str.split_delim equals_re nvp with
            | [] -> ("","")
            | n :: [] -> (n, "")
            | n :: v :: _ -> (n, v)
          in (List.map split_pair cookies) @ acc
      ) [] (Header.get_multi hdr "cookie")

  let serialize cookies =
    "cookie", String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) cookies)
end

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009-2011> Anil Madhavapeddy <anil@recoil.org>
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

open Printf
open Code

type request = { 
  ic: IO.ic;
  headers: (string * string) list;
  meth: meth;
  uri: Uri.t;
  get_params: (string * string) list;
  post_params: (string * string) list;
  version: version;
  bodylen: int64;
}

let meth r = r.meth
let uri r = r.uri
let version r = r.version

let path r = Uri.path r.uri

(* TODO repeated headers handled correctly *)
let header ~name r =
  try [ List.assoc name r.headers ]
  with Not_found -> []

let content_length headers = 
  try Int64.of_string (List.assoc "content-length" headers)
  with Not_found -> 0L

let content_type headers =
  try List.assoc "content-type" headers
  with Not_found -> ""

let params_get r = r.get_params
let params_post r = r.post_params

let param p r =
  try Some (List.assoc p r.post_params) with
  Not_found ->
    (try Some (List.assoc p r.get_params) with
    Not_found -> None)

open IO.M

let parse ic =
  Parser.parse_request_fst_line ic >>=
    function
    |None -> return None
    |Some (meth, uri, version) ->
       Parser.parse_headers ic >>= fun headers ->
       let ctype = content_type headers in
       let bodylen = content_length headers in
       let get_params = Uri.query uri in
       match meth, ctype with
       |`POST, "application/x-www-form-urlencoded" -> 
         (* If the form is query-encoded, then extract those parameters also *)
         IO.read (Int64.to_int bodylen) ic >>= fun query ->
         let post_params = Uri.query_of_encoded query in
         return (Some { ic; headers; meth; uri; version; bodylen; post_params; get_params })
       |_ -> (* Parse content length and generate an iterator for the body *)
         let post_params = [] in
         return (Some { ic; headers; meth; uri; version; bodylen; post_params; get_params })

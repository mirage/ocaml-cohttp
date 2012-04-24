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
open Lwt

open Common
open Types

type request = {
  r_msg: Message.message;
  r_params: (string, string) Hashtbl.t;
  r_get_params: (string * string) list;
  r_post_params: (string * string) list;
  r_meth: meth;
  r_uri: Uri.t;
  r_version: version;
  r_path: string;
}

exception Length_required (* HTTP 411 *)

let media_type_re =
  (* Grab "foo/bar" from " foo/bar ; charset=UTF-8" *)
  Re_str.regexp "[ \t]*\\([^ \t;]+\\)"

let extract_media_type s =
  if Re_str.string_match media_type_re s 0 then
    Re_str.matched_group 1 s
  else
    ""

let init_request ~clisockaddr ~srvsockaddr finished ic =
  lwt meth, uri, version = Parser.parse_request_fst_line ic in
  let path = Uri.path uri in
  let query_get_params = Uri.query uri in
  lwt headers = Parser.parse_headers ic in
  let headers = List.map (fun (h,v) -> (String.lowercase h, v)) headers in
  lwt body = 
    match meth with
    |`POST -> begin
      let limit =
        try Some (Int64.of_string (List.assoc "content-length" headers))
        with Not_found -> None 
      in
      match limit with 
      |None -> Lwt_io.read ic >|= (fun s -> Lwt.wakeup finished (); [`String s])
      |Some count -> return [`Inchan (count, ic, finished)]
    end
    |_ ->  (* TODO empty body for methods other than POST, is ok? *)
      Lwt.wakeup finished ();
      return [`String ""]
  in
  lwt query_post_params, body =
    match meth with
    | `POST -> begin
         try
           let mt = extract_media_type (List.assoc "content-type" headers) in
           if mt = "application/x-www-form-urlencoded" then
             Message.string_of_body body >|=
             (fun s -> Uri.query_of_encoded s, [`String s])
           else return ([], body)
         with Not_found ->
           return ([], body)
    end
    | _ -> return ([], body)
  in
  let params = query_post_params @ query_get_params in (* prefers POST params *)
  let msg = Message.init ~body ~headers ~version ~clisockaddr ~srvsockaddr in
  let params_tbl =
    let tbl = Hashtbl.create (List.length params) in
    List.iter (fun (n,v) -> Hashtbl.add tbl n v) params;
    tbl
  in
  return { r_msg=msg; r_params=params_tbl; r_get_params = query_get_params; 
    r_post_params = query_post_params; r_uri=uri; r_meth=meth; 
    r_version=version; r_path=path }

let meth r = r.r_meth
let uri r = r.r_uri
let path r = r.r_path
let body r = Message.body r.r_msg
let header r ~name = Message.header r.r_msg ~name
let client_addr r = Message.client_addr r.r_msg

let param ?meth ?default r name =
  try
    (match meth with
     | None -> Hashtbl.find r.r_params name
     | Some `GET -> List.assoc name r.r_get_params
     | Some `POST -> List.assoc name r.r_post_params)
   with Not_found ->
        (match default with
        | None -> raise (Param_not_found name)
        | Some value -> value)

let param_all ?meth r name =
  (match (meth: meth option) with
   | None -> List.rev (Hashtbl.find_all r.r_params name)
   | Some `DELETE
   | Some `HEAD
   | Some `GET -> Misc.list_assoc_all name r.r_get_params
   | Some `POST -> Misc.list_assoc_all name r.r_post_params)

let params r = r.r_params
let params_get r = r.r_get_params
let params_post r = r.r_post_params

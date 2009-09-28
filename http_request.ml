
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

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

open Printf;;

open Http_common;;
open Http_types;;

let debug_dump_request path params =
  debug_print ("request path = " ^ path);
  debug_print (
    sprintf"request params = %s"
      (String.concat ";"
        (List.map (fun (h,v) -> String.concat "=" [h;v]) params)))

let auth_sep_RE = Pcre.regexp ":"
let basic_auth_RE = Pcre.regexp "^Basic\\s+"

exception Fallback;;  (* used internally by request class *)

class request ic =
  let (meth, uri, version) = Http_parser.parse_request_fst_line ic in
  let uri_str = Neturl.string_of_url uri in
  let path = Http_parser.parse_path uri in
  let query_get_params = Http_parser.parse_query_get_params uri in
  let (headers, body) =
    (match version with
    | None -> [], ""  (* No version given, use request's 1st line only *)
    | Some version -> (* Version specified, parse also headers and body *)
        let headers =
          List.map  (* lowercase header names to ease lookups before having a
                    request object *)
            (fun (h,v) -> (String.lowercase h, v))
            (Http_parser.parse_headers ic) (* trailing \r\n consumed! *)
        in
        let body =
            (* TODO fallback on size defined in Transfer-Encoding if
              Content-Length isn't defined *)
          if meth = `POST then
            Buffer.contents
              (try  (* read only Content-Length bytes *)
                let limit_raw =
                  (try
                    List.assoc "content-length" headers
                  with Not_found -> raise Fallback)
                in
                let limit =
                  (try  (* TODO supports only a maximum content-length of 1Gb *)
                    int_of_string limit_raw
                  with Failure "int_of_string" ->
                    raise (Invalid_header ("content-length: " ^ limit_raw)))
                in
                Http_misc.buf_of_inchan ~limit ic
              with Fallback -> Http_misc.buf_of_inchan ic)  (* read until EOF *)
          else  (* TODO empty body for methods other than POST, is ok? *)
            ""
        in
        (headers, body))
  in
  let query_post_params =
    match meth with
    | `POST ->
        let ct = try List.assoc "content-type" headers with Not_found -> "" in
        if ct = "application/x-www-form-urlencoded" then
          Http_parser.split_query_params body
        else []
    | _ -> []
  in
  let params = query_post_params @ query_get_params in (* prefers POST params *)
  let _ = debug_dump_request path params in
  let (clisockaddr, srvsockaddr) =
    (Http_misc.peername_of_in_channel ic, Http_misc.sockname_of_in_channel ic)
  in

  object (self)

    inherit
      Http_message.message ~body ~headers ~version ~clisockaddr ~srvsockaddr

    val params_tbl =
      let tbl = Hashtbl.create (List.length params) in
      List.iter (fun (n,v) -> Hashtbl.add tbl n v) params;
      tbl

    method meth = meth
    method uri = uri_str
    method path = path
    method param ?(meth: meth option) ?(default: string option) name =
      try
        (match meth with
        | None -> Hashtbl.find params_tbl name
        | Some `GET -> List.assoc name query_get_params
        | Some `POST -> List.assoc name query_post_params)
      with Not_found ->
        (match default with
        | None -> raise (Param_not_found name)
        | Some value -> value)
    method paramAll ?meth name =
      (match (meth: meth option) with
      | None -> List.rev (Hashtbl.find_all params_tbl name)
      | Some `GET -> Http_misc.list_assoc_all name query_get_params
      | Some `POST -> Http_misc.list_assoc_all name query_post_params)
    method params = params
    method params_GET = query_get_params
    method params_POST = query_post_params

    method private fstLineToString =
      let method_string = string_of_method self#meth in
      match self#version with
      | Some version ->
          sprintf "%s %s %s" method_string self#uri (string_of_version version)
      | None -> sprintf "%s %s" method_string self#uri

    method authorization: auth_info option =
      try
        let credentials =
          Netencoding.Base64.decode
            (Pcre.replace ~rex:basic_auth_RE (self#header "authorization"))
        in
        debug_print ("HTTP Basic auth credentials: " ^ credentials);
        (match Pcre.split ~rex:auth_sep_RE credentials with
        | [username; password] -> Some (`Basic (username, password))
        | l -> raise Exit)
      with Header_not_found _ | Invalid_argument _ | Exit -> None

  end


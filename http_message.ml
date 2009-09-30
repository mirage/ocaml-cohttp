
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

open Http_common
open Http_constants
open Http_types
open Printf

  (* remove all bindings of 'name' from hashtbl 'tbl' *)
let rec hashtbl_remove_all tbl name =
  if not (Hashtbl.mem tbl name) then
    raise (Header_not_found name);
  Hashtbl.remove tbl name;
  if Hashtbl.mem tbl name then hashtbl_remove_all tbl name

type message = {
  m_contents : Buffer.t;
  m_headers : (string, string) Hashtbl.t;
  mutable m_version : version option;
  m_cliaddr : string;
  m_cliport : int;
  m_srvaddr : string;
  m_srvport : int;
} 

let body msg = Buffer.contents msg.m_contents
let body_buf msg = msg.m_contents
let set_body msg =
  Buffer.clear msg.m_contents;
  Buffer.add_string msg.m_contents
let set_body_buf msg = 
  Buffer.clear msg.m_contents;
  Buffer.add_buffer msg.m_contents
let add_body msg =
  Buffer.add_string msg.m_contents
let add_body_buf msg =
  Buffer.add_buffer msg.m_contents
let add_header msg ~name ~value =
  let name = String.lowercase name in
  Http_parser_sanity.heal_header (name, value);
  Hashtbl.add msg.m_headers name value
let add_headers msg =
  List.iter (fun (name, value) -> add_header msg ~name ~value)
let replace_header msg ~name ~value =
  let name = String.lowercase name in
  Http_parser_sanity.heal_header (name, value);
  Hashtbl.replace msg.m_headers name value
let replace_headers msg =
  List.iter (fun (name, value) -> replace_header msg ~name ~value)
let remove_header msg ~name =
  let name = String.lowercase name in
  hashtbl_remove_all msg.m_headers name
let has_header msg ~name =
  Hashtbl.mem msg.m_headers name
let header msg ~name =
  match has_header msg ~name with
  |false -> None
  |true ->
    let name = String.lowercase name in
    let r = String.concat ", " (List.rev (Hashtbl.find_all msg.m_headers name)) in
    Some r
let headers msg =
  List.rev (
    Hashtbl.fold 
      (fun name _ headers -> 
         match header msg ~name with
         |None -> headers
         |Some h -> (name, h) :: headers
      ) msg.m_headers [])

let client_addr msg = msg.m_cliaddr
let server_addr msg = msg.m_srvaddr
let client_port msg = msg.m_cliport
let server_port msg = msg.m_srvport

let version msg = msg.m_version
let set_version msg v = msg.m_version <- Some v

let init ~body ~headers ~version ~clisockaddr ~srvsockaddr =
  let ((cliaddr, cliport), (srvaddr, srvport)) =
    (Http_misc.explode_sockaddr clisockaddr,
     Http_misc.explode_sockaddr srvsockaddr) in
  let msg = { m_contents = Buffer.create 1024;
    m_headers = Hashtbl.create 11;
    m_version = version;
    m_cliaddr = cliaddr;
    m_cliport = cliport;
    m_srvaddr = srvaddr;
    m_srvport = srvport;
  } in
  set_body msg body;
  add_headers msg headers;
  msg

let to_string msg ~fstLineToString =
  let b = body msg in
  fstLineToString ^  (* {request,status} line *)
  crlf ^
  (String.concat  (* headers, crlf terminated *) ""
    (List.map (fun (h,v) -> h ^ ": " ^ v ^ crlf) (headers msg))) ^
  (sprintf "Content-Length: %d" (String.length b)) ^ crlf ^
  crlf ^
  b (* body *)

let serialize msg outchan ~fstLineToString =
  Lwt_io.write outchan (to_string msg ~fstLineToString)

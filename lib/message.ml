(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2009-2012> Anil Madhavapeddy <anil@recoil.org>
  Copyright (C) <2009> David Sheets <sheets@alum.mit.edu>
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

open Common
open Constants
open Types
open Printf
open Lwt

(* remove all bindings of 'name' from hashtbl 'tbl' *)
let rec hashtbl_remove_all tbl name =
  if not (Hashtbl.mem tbl name) then
    raise (Header_not_found name);
  Hashtbl.remove tbl name;
  if Hashtbl.mem tbl name then hashtbl_remove_all tbl name

type contents = [ 
  | `Buffer of Buffer.t
  | `String of string
  | `Inchan of int64 * Lwt_io.input_channel * unit Lwt.u
]

type message = {
  mutable m_contents : contents list;
  m_headers : (string, string) Hashtbl.t; (* TODO: remove Hashtbl, use int offsets into the header buf *)
  m_version : version;
  m_cliaddr : string;
  m_cliport : int;
  m_srvaddr : string;
  m_srvport : int;
} 

let explode_sockaddr = function
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr, port)
  | _ -> assert false (* can explode only inet address *)

let body msg = List.rev msg.m_contents
let body_size cl =
  let (+) = Int64.add in
  List.fold_left (fun a c ->
    match c with
    | `String s -> a + (Int64.of_int (String.length s))
    | `Buffer b -> a + (Int64.of_int (Buffer.length b))
    | `Inchan (i, _, _) -> a + i) Int64.zero cl

let string_of_body cl =
  (* TODO: What if the body is larger than 1GB? *)
  let buf = String.create (Int64.to_int (body_size cl)) in
  lwt _ = List.fold_left (fun pos c ->
    match c with
    | `String s ->
        lwt pos = pos in
        let len = String.length s in
        String.blit s 0 buf pos len;
        return (pos + len)
    | `Buffer b ->
        lwt pos = pos in
        let len = Buffer.length b in
        let str = Buffer.contents b in
        String.blit str 0 buf pos len;
        return (pos + len)
    | `Inchan (il, ic, finished) ->
        lwt pos = pos in
        let il = Int64.to_int il in
        lwt () = Lwt_io.read_into_exactly ic buf pos il in
        Lwt.wakeup finished ();
        return (pos + il)
    ) (return 0) cl in
  return buf

let set_body msg contents =
  msg.m_contents <- [contents]

let add_body msg contents =
  msg.m_contents <- (contents :: msg.m_contents)

let add_header msg ~name ~value =
  let name = String.lowercase name in
  Hashtbl.add msg.m_headers name value

let add_headers msg =
  List.iter (fun (name, value) ->
    add_header msg ~name ~value)

let replace_header msg ~name ~value =
  let name = String.lowercase name in
  Hashtbl.replace msg.m_headers name value

let replace_headers msg =
  List.iter (fun (name, value) ->
    replace_header msg ~name ~value)

let remove_header msg ~name =
  let name = String.lowercase name in
  hashtbl_remove_all msg.m_headers name

let has_header msg ~name =
  Hashtbl.mem msg.m_headers name

let header msg ~name =
  let name = String.lowercase name in
  let compact = String.concat ", " in
  (* TODO: Just these old headers or all of HTTP 1.0? *)
  let no_compact = ["set-cookie"] in
  if has_header msg ~name then
    let hl = List.rev (Hashtbl.find_all msg.m_headers name) in
    if List.mem name no_compact then hl else [compact hl]
  else []

let headers msg =
  let hset = Hashtbl.create 11 in
  Hashtbl.iter (fun name _ -> Hashtbl.replace hset name ()) msg.m_headers;
  Hashtbl.fold (fun name _ headers -> 
    List.rev_append (List.map (fun h -> (name, h)) (header msg ~name)) headers
  ) hset []
    
let client_addr msg = msg.m_cliaddr

let server_addr msg = msg.m_srvaddr

let client_port msg = msg.m_cliport

let server_port msg = msg.m_srvport

let version msg = msg.m_version

let init ~body ~headers ~version ~clisockaddr ~srvsockaddr =
  let ((cliaddr, cliport), (srvaddr, srvport)) =
    (explode_sockaddr clisockaddr,
     explode_sockaddr srvsockaddr) in
  let msg = {
    m_contents = body;
    m_headers = Hashtbl.create 11;
    m_version = version;
    m_cliaddr = cliaddr;
    m_cliport = cliport;
    m_srvaddr = srvaddr;
    m_srvport = srvport;
  } in
  add_headers msg headers;
  msg
      
let relay ic oc write_from_exactly =
  let bufsize = 4096 in (* blksz *)
  let buffer = String.create bufsize in
  let rec aux () =
    match_lwt Lwt_io.read_into ic buffer 0 bufsize with
    |0 -> return ()
    |len ->
       write_from_exactly oc buffer 0 len >>
       aux ()
  in aux ()

let serialize msg outchan write write_from_exactly ~fstLineToString =
  let body = body msg in
  let bodylen = body_size body in 
  let _ = write outchan (fstLineToString ^ crlf) in
  lwt () = Lwt_list.iter_s (fun (h,v) ->
    write outchan (sprintf "%s: %s\r\n" h v)) (headers msg) in
  lwt () = if bodylen != Int64.zero then
    write outchan (sprintf "Content-Length: %Ld\r\n\r\n" bodylen)
  else return () in
  Lwt_list.iter_s (
    function
    | `String s -> write outchan s
    | `Buffer b -> write outchan (Buffer.contents b)
    | `Inchan (_, ic, finished) ->
         lwt () = relay ic outchan write_from_exactly in 
         wakeup finished ();
         return ()
  ) body

let serialize_to_output_channel msg outchan ~fstLineToString =
  serialize msg outchan Lwt_io.write Lwt_io.write_from_exactly ~fstLineToString

let serialize_to_stream msg ~fstLineToString =
  let stream, push = Lwt_stream.create () in
  ignore_result (serialize msg ()
    (fun () s -> push (Some s); return ())
    (fun () buf off len -> push (Some (String.sub buf off len)); return ())
       ~fstLineToString >> (push None; return ()));
  stream

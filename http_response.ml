
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

open Http_types
open Http_constants
open Http_common
open Printf
open Lwt

let status_line_RE = Pcre.regexp "^(HTTP/\\d\\.\\d) (\\d{3}) (.*)$"

let anyize = function
  | Some addr -> addr
  | None -> Unix.ADDR_INET (Unix.inet_addr_any, -1)

type response = {
  r_msg: Http_message.message;
  mutable r_code: int;
  mutable r_reason: string option;
  
}

let init
  ?(body = "") ?(headers = []) ?(version = http_version) ?(code = 200)
  ?clisockaddr ?srvsockaddr ?status ()
  =
    (** if no address were supplied for client and/or server, use a foo address
    instead *)
  let (clisockaddr, srvsockaddr) = (anyize clisockaddr, anyize srvsockaddr) in
  let msg = Http_message.init ~body ~headers ~version:(Some version)
     ~clisockaddr ~srvsockaddr in
  let code = match status with
      | None -> code
      | Some (s: Http_types.status) -> code_of_status s in
  let reason = None in
  { r_msg = msg; r_code = code; r_reason = reason }

let real_version r = 
  match Http_message.version r.r_msg with
      | None ->
          failwith ("Http_response.fstLineToString: " ^
            "can't serialize an HTTP response with no HTTP version defined")
      | Some v -> string_of_version v

let code r = r.r_code
let set_code r c = 
   ignore (status_of_code c);  (* sanity check on c *)
   r.r_code <- c
let status r = status_of_code (code r)
let set_status r (s: Http_types.status) = r.r_code <- code_of_status s
let reason r =
   match r.r_reason with
      | None -> Http_misc.reason_phrase_of_code r.r_code
      | Some r -> r
let set_reason r rs = r.r_reason <- Some rs
let status_line r =
      String.concat " "
        [real_version r; string_of_int (code r); reason r ]
let set_status_line r s =
   try
     let subs = Pcre.extract ~rex:status_line_RE s in
     Http_message.set_version r.r_msg (version_of_string subs.(1));
     set_code r (int_of_string subs.(2));
     set_reason r subs.(3);
   with Not_found ->
      raise (Invalid_status_line s)

let is_informational r = Http_common.is_informational r.r_code
let is_success r = Http_common.is_success r.r_code
let is_redirection r = Http_common.is_redirection r.r_code
let is_client_error r = Http_common.is_client_error r.r_code
let is_server_error r = Http_common.is_server_error r.r_code
let is_error r = Http_common.is_error r.r_code

let add_basic_headers r =
  Http_message.add_header r.r_msg ~name:"Date" ~value:(Http_misc.date_822 ());
  Http_message.add_header r.r_msg ~name:"Server" ~value:server_string

let gh name r = Http_message.header r.r_msg ~name
let rh name r = Http_message.replace_header r.r_msg ~name

let content_type = gh "Content-Type"
let set_content_type = rh "Content-Type"
let content_encoding = gh "Content-Encoding"
let set_content_encoding = rh "Content-Encoding"
let date = gh "Date"
let set_date = rh "Date"
let expires = gh "Expires"
let set_expires = rh "Expires"
let server = gh "Server"
let set_server = rh "Server"

let serialize r outchan = 
  let fstLineToString =
    sprintf "%s %d %s" (real_version r) (code r) (reason r) in
  Http_message.serialize r.r_msg outchan ~fstLineToString

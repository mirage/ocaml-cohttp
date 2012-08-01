(*
 * OCaml HTTP - do it yourself (fully OCaml) HTTP daemon
 *
 * Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
 * Copyright (C) <2009-2012> Anil Madhavapeddy <anil@recoil.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation, version 2.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *)

open Printf
open Cohttp
open Common
open Lwt

type headers = (string * string) list

type tcp_error_source = Connect | Read | Write
exception Tcp_error of tcp_error_source * exn
exception Http_error of (int * headers * string)  (* code, body *)

let tcp_bufsiz = 4096 (* for TCP I/O *)

let host_of_uri uri = 
  match Uri.host uri with
  |None -> "localhost"
  |Some h -> h

let port_of_uri uri =
  match Uri.port uri with
  |None -> begin
     match Uri.scheme uri with 
     |Some "https" -> 443 (* TODO: actually support https *)
     |Some "http" | Some _ |None -> 80
  end
  |Some p -> p

(* Path+Query string *)
let path_of_uri uri = 
  match (Uri.path uri), (Uri.query uri) with
  |"", [] -> "/"
  |"", q -> sprintf "/?%s" (Uri.encoded_of_query q)
  |p, [] -> p
  |p, q -> sprintf "%s?%s" p (Uri.encoded_of_query q)

let build_sockaddr (addr, port) =
  try_lwt
    (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
    let hent = Unix.gethostbyname addr in
    return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> 
    raise_lwt (Failure ("cant resolve hostname: " ^ addr))

module Normal = struct

  let connect uri iofn =
    let open Uri in
    let address = host_of_uri uri in
    let port = port_of_uri uri in
    lwt sockaddr = build_sockaddr (address, port) in
    Lwt_io.with_connection ~buffer_size:tcp_bufsiz sockaddr iofn

end

module SSL = struct

  let sslcontext =
    Ssl.init ();
    Ssl.create_context Ssl.SSLv23 Ssl.Client_context

  let connect uri iofn =
    let address = host_of_uri uri in
    let port = port_of_uri uri in
    lwt sockaddr = build_sockaddr (address, port) in
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sockaddr in
    lwt sock = Lwt_ssl.ssl_connect fd sslcontext in
    let ic = Lwt_ssl.in_channel_of_descr sock in
    let oc = Lwt_ssl.out_channel_of_descr sock in
    try_lwt
      lwt res = iofn (ic,oc) in
      Lwt_ssl.close sock >>
      return res
    with exn ->
      Lwt_ssl.close sock >>
      fail exn
end
 
let connect uri iofn =
  match Uri.scheme uri with
  |Some "https" -> SSL.connect uri iofn
  |Some "http" -> Normal.connect uri iofn
  |Some _ | None -> fail (Failure "unknown scheme")
 
let rec read_write_r inchan outchan read_size num_read max_to_read =
  lwt s = Lwt_io.read ~count:read_size inchan in
  if s = ""  then
    return ()
  else
    lwt () = Lwt_io.write outchan s in
    let num_read_incr = num_read + (String.length s) in
    if num_read_incr < max_to_read then
      read_write_r inchan outchan read_size num_read_incr max_to_read
    else
      return ()

let read_write ?(read_size=tcp_bufsiz) inchan outchan =
  read_write_r inchan outchan read_size 0 max_int

let read_write_count ?(read_size=tcp_bufsiz) inchan outchan ~count =
  read_write_r inchan outchan read_size 0 count
  
(* the source of a request body, if any, can be either a string or an
   input channel *)
type request_body = [ 
| `None 
| `String of string 
| `InChannel of (int * Lwt_io.input_channel) 
]

let content_length_header s = 
  "Content-Length", s 

let default_content_type_h = "Content-Type", "application/x-www-form-urlencoded"

let build_req_header headers meth uri body =
  let headers = 
    match body with
      | `None -> headers
      | `String s -> 
        let content_length_h = content_length_header (string_of_int (String.length s)) in
        default_content_type_h :: content_length_h :: headers
      | `InChannel (cl,_) -> 
        let content_length_h = content_length_header (string_of_int cl) in
        default_content_type_h :: content_length_h ::headers
  in
  let headers = ("Host", host_of_uri uri) :: headers in
  let hdrcnt = List.length headers in
  let add_header ht (n, v) = (Hashtbl.replace ht n v; ht) in
  let hdrht = List.fold_left add_header (Hashtbl.create hdrcnt) headers in
  let serialize_header name value prev =
    sprintf "%s\r\n%s: %s" prev name value in
  let hdrst = Hashtbl.fold serialize_header hdrht "" in
  let path = path_of_uri uri in
  sprintf "%s %s HTTP/1.1%s\r\n\r\n" meth path hdrst

let request ?(headers=[]) outchan meth body uri =
  let req_header = build_req_header headers meth uri body in
  lwt () = Lwt_io.write outchan req_header in
  lwt () = match body with
    | `None -> return ()
    | `String s -> Lwt_io.write outchan s
    | `InChannel (content_length, inchan) -> read_write inchan outchan 
  in
  Lwt_io.flush outchan

let parse_content_range s =
  try
    let start, fini, total = Scanf.sscanf s "bytes %d-%d/%d" 
      (fun start fini total -> start, fini, total) 
    in
    Some (start, fini, total)
  with Scanf.Scan_failure _ ->
    None

(* if we see a "Content-Range" header, than we should limit the
   number of bytes we attempt to read *)
let content_length_of_content_range headers = 
  try
    (* assuming header keys were downcased in previous step *)
    let range_s = List.assoc "content-range" headers in
    match parse_content_range range_s with
      | Some (start, fini, total) ->
          (* some sanity checking before we act on these values *)
        if fini < total && start <= total && 0 <= start && 0 <= total then (
          let num_bytes_to_read = fini - start + 1 in
          Some num_bytes_to_read
        )
        else
          None
      | None -> 
        None
  with Not_found ->
    None

(* Determine content length, either by content-length header or
   via content-range if content-length is not specified *)
let content_length_of_headers headers =
  try
    let length = List.assoc "content-length" headers in
    Some (int_of_string length)
  with _ ->
    content_length_of_content_range headers

(* determine transfer encoding *)
let stream_of_body headers ic =
  let transfer_encoding =
    try Some (String.lowercase (List.assoc "transfer-encoding" headers))
    with Not_found -> None
  in
  let content_length = content_length_of_headers headers in
  match content_length, transfer_encoding with
  |_, Some "chunked" -> Transfer.(read Chunked ic)
  |Some len, Some _
  |Some len, None -> Transfer.(read (Fixed len) ic) 
  |None, None
  |None, Some _ -> Transfer.(read Unknown ic)
    
let read_response inchan response_body =
  lwt (_, status) = Parser.parse_response_fst_line inchan in
  lwt headers = Parser.parse_headers inchan in
  let headers = List.map (fun (h, v) -> (String.lowercase h, v)) headers in
  let content_length_opt = content_length_of_headers headers in
  let response_stream = stream_of_body headers inchan in
  (* a status code of 206 (Partial) will typicall accompany "Content-Range" 
     response header *)
  match response_body with
    | `String -> begin
      let buf = Buffer.create (match content_length_opt with |None -> 1024 |Some l -> l) in
      lwt () = Lwt_stream.iter (Buffer.add_string buf) response_stream in
      let resp = Buffer.contents buf in
      match code_of_status status with
      | 200 | 201 | 202 | 203 | 204 | 205 | 206 -> return (`S (headers, resp))
      | code -> fail (Http_error (code, headers, resp))
    end
    | `OutChannel outchan -> begin
      lwt () = Lwt_stream.iter_s (Lwt_io.write outchan) response_stream in
      match code_of_status status with
        | 200 | 206 -> return (`C headers)
        | code -> fail (Http_error (code, headers, ""))
    end
 
let call (headers:headers) kind (request_body:request_body) uri response_body  =
  let meth = match kind with
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `PUT -> "PUT" 
    | `DELETE -> "DELETE" 
    | `POST -> "POST" in
  connect uri
    (fun (i, o) ->
      (try_lwt
         request ~headers o meth request_body uri
       with
        | Lwt.Canceled as e -> fail e
        | exn -> fail (Tcp_error (Write, exn))
      ) >> (
        try_lwt 
          read_response i response_body
        with
          | (Http_error _) | Lwt.Canceled as e -> fail e
          | exn -> fail (Tcp_error (Read, exn))
       ))

let call_to_string (headers:headers) kind (request_body:request_body) url : (headers * string) Lwt.t =
  lwt resp = call headers kind request_body url `String in
  (* assert relation between request and response kind *)
  match resp with
    | `S hb -> return hb
    | _ -> assert false

let call_to_chan headers kind (request_body:request_body) url outchan : headers Lwt.t =
  lwt resp = call headers kind request_body url (`OutChannel outchan) in
  (* assert relation between request and response kind *)
  match resp with
    | `C h -> return h
    | _ -> assert false

let head ?(headers=[]) url = call_to_string headers `HEAD `None url 
let get ?(headers=[]) url = call_to_string headers `GET `None url 
let post ?(headers=[]) ?(body=`None) url = call_to_string headers `POST body url 
let put ?(headers=[]) ?(body=`None) url = call_to_string headers `PUT body url 
let delete ?(headers=[]) url = call_to_string headers `DELETE `None url 

let head_to_chan ?(headers=[]) url ch = call_to_chan headers `HEAD `None url ch
let get_to_chan ?(headers=[]) url ch = call_to_chan headers `GET `None url ch
let post_to_chan ?(headers=[]) ?(body=`None) url ch = call_to_chan headers `POST body url ch
let put_to_chan ?(headers=[]) ?(body=`None) url ch = call_to_chan headers `PUT body url ch
let delete_to_chan ?(headers=[]) url ch = call_to_chan headers `DELETE `None url ch


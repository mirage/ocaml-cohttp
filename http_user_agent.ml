
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

open Printf

open Http_common

exception Http_error of (int * string)  (* code, body *)

let http_scheme_RE = Pcre.regexp ~flags:[`CASELESS] "^http://"
let url_RE = Pcre.regexp "^([\\w.-]+)(:(\\d+))?(/.*)?$"

let tcp_bufsiz = 4096 (* for TCP I/O *)

let parse_url url =
  try
    let subs =
      Pcre.extract ~rex:url_RE (Pcre.replace ~rex:http_scheme_RE url)
    in
    (subs.(1),
    (if subs.(2) = "" then 80 else int_of_string subs.(3)),
    (if subs.(4) = "" then "/" else subs.(4)))
  with exc ->
    failwith
      (sprintf "Can't parse url: %s (exception: %s)"
        url (Printexc.to_string exc))

let init_socket addr port =
  let inet_addr = (Unix.gethostbyname addr).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let suck = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect suck sockaddr;
  let outchan = Unix.out_channel_of_descr suck in
  let inchan = Unix.in_channel_of_descr suck in
  (inchan, outchan)

let submit_request kind url =
  let (address, port, path) = parse_url url in
  let (inchan, outchan) = init_socket address port in
  let req_string = match kind with `GET -> "GET" | `HEAD -> "HEAD" in
  output_string outchan (sprintf "%s %s HTTP/1.0\r\n" req_string path);
  output_string outchan (sprintf "Host: %s\r\n\r\n" address);
  flush outchan;
  (inchan, outchan)

let head url =
  let (inchan, outchan) = submit_request `HEAD url in
  let (_, status) = Http_parser.parse_response_fst_line inchan in
  (match code_of_status status with
  | 200 -> ()
  | code -> raise (Http_error (code, "")));
  let buf = Http_misc.buf_of_inchan inchan in
  close_in inchan; (* close also outchan, same fd *)
  Buffer.contents buf

let get_iter ?(head_callback = fun _ _ -> ()) callback url =
  let (inchan, outchan) = submit_request `GET url in
  let buf = String.create tcp_bufsiz in
  let (_, status) = Http_parser.parse_response_fst_line inchan in
  (match code_of_status status with
  | 200 -> ()
  | code -> raise (Http_error (code, "")));
  let headers = Http_parser.parse_headers inchan in
  head_callback status headers;
  (try
    while true do
      match input inchan buf 0 tcp_bufsiz with
      | 0 -> raise End_of_file
      | bytes when bytes = tcp_bufsiz ->  (* buffer full, no need to slice it *)
          callback buf
      | bytes when bytes < tcp_bufsiz ->  (* buffer not full, slice it *)
          callback (String.sub buf 0 bytes)
      | _ -> (* ( bytes < 0 ) || ( bytes > tcp_bufsiz ) *)
          assert false
    done
  with End_of_file -> ());
  close_in inchan (* close also outchan, same fd *)

let get ?head_callback url =
  let buf = Buffer.create 10240 in
  get_iter ?head_callback (Buffer.add_string buf) url;
  Buffer.contents buf


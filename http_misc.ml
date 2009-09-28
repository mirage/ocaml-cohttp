
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

open Http_types

let date_822 () =
  Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time ())

let is_directory name =
  match Unix.lstat name with
  | { Unix.st_kind = Unix.S_DIR } -> true
  | _ -> false

let filesize fname = (Unix.stat fname).Unix.st_size

let strip_trailing_slash =
  let rex = Pcre.regexp "/$" in
  fun s -> Pcre.replace ~rex ~templ:"" s

let strip_heading_slash =
  let rex = Pcre.regexp "^/" in
  fun s -> Pcre.replace ~rex ~templ:"" s

let ls dir =
  let rec ls' entries =
    try ls' ((Unix.readdir dir)::entries) with End_of_file -> entries
  in
  ls' []

let string_explode s =
  let rec string_explode' acc = function
    | "" -> acc
    | s -> string_explode' (s.[0] :: acc) (String.sub s 1 (String.length s - 1))
  in
  List.rev (string_explode' [] s)

let string_implode = List.fold_left (fun s c -> s ^ (String.make 1 c)) ""

let reason_phrase_of_code = function
  | 100 -> "Continue"
  | 101 -> "Switching protocols"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non authoritative information"
  | 204 -> "No content"
  | 205 -> "Reset content"
  | 206 -> "Partial content"
  | 300 -> "Multiple choices"
  | 301 -> "Moved permanently"
  | 302 -> "Found"
  | 303 -> "See other"
  | 304 -> "Not modified"
  | 305 -> "Use proxy"
  | 307 -> "Temporary redirect"
  | 400 -> "Bad request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment required"
  | 403 -> "Forbidden"
  | 404 -> "Not found"
  | 405 -> "Method not allowed"
  | 406 -> "Not acceptable"
  | 407 -> "Proxy authentication required"
  | 408 -> "Request time out"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length required"
  | 412 -> "Precondition failed"
  | 413 -> "Request entity too large"
  | 414 -> "Request URI too large"
  | 415 -> "Unsupported media type"
  | 416 -> "Requested range not satisfiable"
  | 417 -> "Expectation failed"
  | 500 -> "Internal server error"
  | 501 -> "Not implemented"
  | 502 -> "Bad gateway"
  | 503 -> "Service unavailable"
  | 504 -> "Gateway time out"
  | 505 -> "HTTP version not supported"
  | invalid_code -> raise (Invalid_code invalid_code)

let build_sockaddr (addr, port) =
  try
    Unix.ADDR_INET ((Unix.gethostbyname addr).Unix.h_addr_list.(0), port)
  with Not_found -> failwith ("OCaml-HTTP, can't resolve hostname: " ^ addr)

let explode_sockaddr = function
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr, port)
  | _ -> assert false (* can explode only inet address *)

let peername_of_out_channel outchan =
  Unix.getpeername (Unix.descr_of_out_channel outchan)
let peername_of_in_channel inchan =
  Unix.getpeername (Unix.descr_of_in_channel inchan)
let sockname_of_out_channel outchan =
  Unix.getsockname (Unix.descr_of_out_channel outchan)
let sockname_of_in_channel inchan =
  Unix.getsockname (Unix.descr_of_in_channel inchan)

let buf_of_inchan ?limit ic =
  let buf = Buffer.create 10240 in
  let tmp = String.make 1024 '\000' in
  let rec buf_of_inchan' limit =
    (match limit with
    | None ->
        let bytes = input ic tmp 0 1024 in
        if bytes > 0 then begin
          Buffer.add_substring buf tmp 0 bytes;
          buf_of_inchan' None
        end
    | Some lim -> (* TODO what about using a single really_input call? *)
        let bytes = input ic tmp 0 (min lim 1024) in
        if bytes > 0 then begin
          Buffer.add_substring buf tmp 0 bytes;
          buf_of_inchan' (Some (lim - bytes))
        end)
  in
  (try buf_of_inchan' limit with End_of_file -> ());
  buf

let list_assoc_all key pairs =
  snd (List.split (List.filter (fun (k, v) -> k = key) pairs))

let warn msg  = prerr_endline (sprintf "ocaml-http WARNING: %s" msg)
let error msg = prerr_endline (sprintf "ocaml-http ERROR:   %s" msg)

let finally at_end f arg =
  let res =
    try f arg
    with exn -> at_end (); raise exn
  in
  at_end ();
  res


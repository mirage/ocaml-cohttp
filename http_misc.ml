(*pp camlp4o -I `ocamlfind query lwt.syntax` lwt-syntax-options.cma lwt-syntax.cma *)

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
open Lwt

open Http_types

let date_822 () =
  Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time ())

let is_directory name =
  match Unix.lstat name with
  | { Unix.st_kind = Unix.S_DIR } -> true
  | _ -> false

let strip_trailing_slash =
  let rex = Pcre.regexp "/$" in
  fun s -> Pcre.replace ~rex ~templ:"" s

let strip_heading_slash =
  let rex = Pcre.regexp "^/" in
  fun s -> Pcre.replace ~rex ~templ:"" s

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
  try_lwt
      (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
      let hent = Unix.gethostbyname addr in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> failwith ("ocaml-cohttp, cant resolve hostname: " ^ addr)
     
let explode_sockaddr = function
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr, port)
  | _ -> assert false (* can explode only inet address *)

let list_assoc_all key pairs =
  snd (List.split (List.filter (fun (k, v) -> k = key) pairs))

let warn msg  = prerr_endline (sprintf "ocaml-cohttp WARNING: %s" msg)
let error msg = prerr_endline (sprintf "ocaml-cohttp ERROR:   %s" msg)


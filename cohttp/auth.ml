(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Sexplib.Std
open Printf

type req = [
 | `Basic of string (* realm *)
] with sexp

type resp = [
 | `Basic of string * string (* username, password *)
 | `Other of string
]  with sexp

let resp_to_string (resp:resp) =
  match resp with
  | `Basic (user, pass) ->
    "Basic " ^ (Base64.encode (sprintf "%s:%s" user pass))
  | `Other buf -> buf

let resp_of_string (buf:string) : resp =
  try
    let b64 = Scanf.sscanf buf "Basic %s" (fun b -> b) in
    match Stringext.split ~on:':' (Base64.decode b64) ~max:2 with
    |[user;pass] -> `Basic (user,pass)
    |_ -> `Other buf
  with _ -> `Other buf

let req_to_string (ty:req) =
  match ty with
  |`Basic realm -> sprintf "Basic realm=\"%s\"" realm

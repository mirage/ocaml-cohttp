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

type t =
  | Basic of string * string (* username, password *)
  with sexp

let to_string =
  function
  |Basic (user, pass) ->
    "Basic " ^ (Base64.encode (sprintf "%s:%s" user pass))

let of_string v =
  try
    let b64 = Scanf.sscanf v "Basic %s" (fun b -> b) in
    match Re_str.bounded_split (Re_str.regexp_string ":") (Base64.decode b64) 2 with
    |[user;pass] -> Some (Basic (user,pass))
    |_ -> None
  with _ -> None

let req_to_string (ty:req) =
  match ty with
  |`Basic realm -> sprintf "Basic realm=\"%s\"" realm

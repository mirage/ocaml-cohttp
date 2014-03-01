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

open Cohttp
open Lwt
open Sexplib.Std
open Sexplib.Conv

type t = [
  | Body.t
  | `Stream of string Lwt_stream.t sexp_opaque
] with sexp

let empty = (Body.empty :> t)

let create_stream fn arg =
  let fin = ref false in
  Lwt_stream.from (fun () ->
    match !fin with
    |true -> return None
    |false -> begin
        match_lwt fn arg with
        |Transfer.Done -> 
          return None
        |Transfer.Final_chunk c ->
          fin := true;
          return (Some c);
        |Transfer.Chunk c ->
          return (Some c)
      end
  )

let to_string (body:t) =
  match body with
  | #Body.t as body -> return (Body.to_string body)
  |`Stream s ->
    let b = Buffer.create 1024 in
    Lwt_stream.iter (Buffer.add_string b) s >>= fun () ->
    return (Buffer.contents b)

let of_string s = ((Body.of_string s) :> t)

let to_stream (body:t) =
  match body with
  |`Empty -> Lwt_stream.of_list []
  |`Stream s -> s
  |`String s -> Lwt_stream.of_list [s]

let drain_body (body:t) =
  match body with
  |`Empty
  |`String _ -> return ()
  |`Stream s -> Lwt_stream.junk_while (fun _ -> true) s

let of_string_list l : t =
  `Stream (Lwt_stream.of_list l)

let of_stream s : t =
  `Stream s

let transfer_encoding (t:t) =
  match t with
  |#Body.t as t -> Body.transfer_encoding t
  |`Stream _ -> Transfer.Chunked

(* This will consume the body and return a length, and a
 * new body that should be used instead of the input *)
let length (body:t) : (int * t) Lwt.t =
  match body with
  |#Body.t as body -> return (Body.length body, body)
  |`Stream s ->
    lwt buf = to_string body in
    let len = String.length buf in
    return (len, `String buf)

let write_body ?(flush=(fun () -> return_unit)) fn (body:t) =
  match body with
  |`Empty -> return ()
  |`Stream st -> Lwt_stream.iter_s (fun b -> fn b >>= flush) st
  |`String s -> fn s

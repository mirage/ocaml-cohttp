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

type contents = [
  |`Stream of string Lwt_stream.t
  |`String of string
]

type t = contents option

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

let string_of_body (body:t) =
  match body with
  |None -> return ""
  |Some (`String s) -> return s
  |Some (`Stream s) ->
     let b = Buffer.create 1024 in
     Lwt_stream.iter (Buffer.add_string b) s >>
     return (Buffer.contents b)

let stream_of_body (body:t) =
  match body with
  |None -> Lwt_stream.of_list []
  |Some (`Stream s) -> s
  |Some (`String s) -> Lwt_stream.of_list [s]
  
let body_of_string s : t =
  Some (`String s)

let body_of_string_list l : t =
  Some (`Stream (Lwt_stream.of_list l))

let body_of_stream s : t =
  Some (`Stream s)

let get_transfer_encoding (t:t) =
  match t with
  |None -> Transfer.Fixed 0
  |Some (`Stream _) -> Transfer.Chunked
  |Some (`String s) -> Transfer.Fixed (String.length s)

(* This will consume the body and return a length, and a
 * new body that should be used instead of the input *)
let get_length (body:t) : (int * t) Lwt.t =
  match body with
  |None ->
    return (0, body)
  |Some (`String s) -> 
    return (String.length s, body)
  |Some (`Stream s) ->
    lwt buf = string_of_body body in
    let len = String.length buf in
    return (len, (Some (`String buf)))

let write_body fn (body:t) =
  match body with
  |None -> return ()
  |Some (`Stream st) -> Lwt_stream.iter_s fn st
  |Some (`String s) -> fn s

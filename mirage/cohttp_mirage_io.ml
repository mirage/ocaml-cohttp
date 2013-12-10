(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazazagnaire.org>
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

(* open Cohttp *)

open Net

type 'a t = 'a Lwt.t
let (>>=) = Lwt.bind
let (>>) m n = m >>= fun _ -> n
let return = Lwt.return

type ic = Channel.t
type oc = Channel.t

let iter fn x = Lwt_list.iter_s fn x

let read_line ic =
  match_lwt Channel.read_line ic with
  | [] -> return None
  | bufs -> return (Some (Cstruct.copyv bufs))

let read ic len =
  try_lwt
    lwt iop = Channel.read_some ~len ic in
    return (Cstruct.to_string iop)
  with End_of_file -> return ""

let read_exactly ic buf off len =
  let rec read acc left =
    match left with
    | 0   -> return (List.rev acc)
    | len ->
      lwt iop = Channel.read_some ~len ic in
      read (iop::acc) (left - (Cstruct.len iop))
  in
  lwt iov = read [] len in
  (* XXX TODO this is hyper slow! *)
  let srcbuf = Cstruct.copyv iov in
  String.blit srcbuf 0 buf off (String.length srcbuf);
  return true

let read_exactly ic len =
  let buf = String.create len in
  read_exactly ic buf 0 len >>= function
  | true -> return (Some buf)
  | false -> return None

let write oc buf =
  Channel.write_string oc buf 0 (String.length buf);
  Channel.flush oc

let write_line oc buf =
  Channel.write_line oc buf;
  Channel.flush oc

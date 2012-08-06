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

(* Hide away Lwt details in this monad, as IO.M.t, to let us plug
 * in alternate mechanisms such as Async *)

type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

module M = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return
end

open Lwt

let iter fn x = Lwt_list.iter_s fn x

let read_line ic =
  try_lwt Lwt_io.read_line ic >>= fun x -> return (Some x)
  with _ -> return None

let read count ic =
  Lwt_io.read ~count ic

(* An inefficient relay function *)
let relay ic oc =
  let bufsize = 4096 in (* blksz *)
  let buffer = String.create bufsize in
  let rec aux () =
    match_lwt Lwt_io.read_into ic buffer 0 bufsize with
    |0 -> return ()
    |len -> Lwt_io.write_from_exactly oc buffer 0 len >>= aux
  in aux ()

let ic_of_buffer buf = Lwt_io.of_bytes ~mode:Lwt_io.input buf
let oc_of_buffer buf = Lwt_io.of_bytes ~mode:Lwt_io.output buf

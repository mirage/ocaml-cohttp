(*
 * Copyright (c) 2014 Rudi Grinberg
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

type t = [
  | `Empty
  | `String of string
] with sexp

let empty = `Empty

let to_string = function
  | `Empty -> ""
  | `String s -> s

let of_string s = `String s
let of_string_list s = `String (String.concat "" s)

let transfer_encoding (t:t) =
  match t with
  | `Empty -> Transfer.Fixed 0
  | `String s -> Transfer.Fixed (String.length s)

let length = function
  | `Empty -> 0
  | `String s -> String.length s

<<<<<<< HEAD
let map t ~f =
  match t with
  | `Empty -> `Empty
  | `String s -> `String (f s)

module type S = sig
  type t
  val to_string : t -> string
  val empty : t
  val of_string : string -> t
  val of_string_list : string list -> t
  val map : t -> f:(string -> string) -> t
  val transfer_encoding : t -> Transfer.encoding
end

=======
>>>>>>> cf6ae92a6b411b7de6f1920ff37dd70541a1fab8
(* TODO: maybe add a functor here that uses IO.S *)

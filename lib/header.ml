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

module StringMap = Map.Make(String)
type t = string StringMap.t

let init () = StringMap.empty
let add h k v = StringMap.add k v h
let remove h k = StringMap.remove k h
let get h k = try [StringMap.find k h] with Not_found -> []
let map fn h = StringMap.mapi fn h
let iter fn h = ignore(map fn h)
let fold fn h acc = StringMap.fold fn h acc
let of_list l = List.fold_left (fun a (k,v) -> StringMap.add k v a) StringMap.empty l
let to_list h = StringMap.fold (fun k v acc -> (k,v)::acc) h []

module M(IO:IO.M) = struct
  open IO
  let header_sep = Re_str.regexp ": *"
  let parse ic =
    (* consume also trailing "^\r\n$" line *)
    let rec parse_headers' headers =
      read_line ic >>= function
      |Some "" | None -> return headers
      |Some line -> begin
          match Re_str.bounded_split_delim header_sep line 2 with
          | [hd;tl] ->
              let header = String.lowercase hd in
              parse_headers' (add headers header tl);
          | _ -> return headers
      end
    in parse_headers' (init ())
end

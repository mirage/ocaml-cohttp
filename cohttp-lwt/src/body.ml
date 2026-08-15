(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

module Body = Cohttp.Body
module Transfer = Cohttp.Transfer
open Lwt

type t =
  [ Body.t
  | `Stream of (string Lwt_stream.t[@sexp.opaque])
  | `Bigstring of (Cohttp.Bigstring.write[@sexp.opaque]) ]
[@@deriving sexp]

let bigstring_length b = Cohttp.Bigstring.length (Cohttp.Bigstring.buffer b)

let bigstring_to_string buf =
  Cohttp.Bigstring.sub_string
    (Cohttp.Bigstring.buffer buf)
    ~off:0 ~len:(bigstring_length buf)

let empty = (Body.empty :> t)

let create_stream fn arg =
  let fin = ref false in
  Lwt_stream.from (fun () ->
      match !fin with
      | true -> return_none
      | false -> (
          fn arg >>= function
          | Transfer.Done -> return_none
          | Final_chunk c ->
              fin := true;
              return (Some c)
          | Chunk c -> return (Some c)))

let is_empty (body : t) =
  match body with
  | #Body.t as body -> return (Body.is_empty body)
  | `Bigstring b -> return (bigstring_length b = 0)
  | `Stream s ->
      Lwt_stream.get_while (fun x -> x = "") s >>= fun _ ->
      Lwt_stream.is_empty s

let to_string (body : t) =
  match body with
  | #Body.t as body -> return (Body.to_string body)
  | `Bigstring b -> return (bigstring_to_string b)
  | `Stream s ->
      let b = Buffer.create 1024 in
      Lwt_stream.iter (Buffer.add_string b) s >>= fun () ->
      return (Buffer.contents b)

let to_string_list (body : t) =
  match body with
  | #Body.t as body -> return (Body.to_string_list body)
  | `Bigstring b -> return [ bigstring_to_string b ]
  | `Stream s -> Lwt_stream.to_list s

let of_string s = (Body.of_string s :> t)

let bigstring_of_string s =
  let len = String.length s in
  let out = Cohttp.Bigstring.create len in
  Cohttp.Bigstring.blit_from_string s ~src_off:0 out ~dst_off:0 ~len;
  out

let to_bigstring (body : t) =
  match body with
  | `Bigstring b -> return (Cohttp.Bigstring.buffer b)
  | #Body.t as b -> return (bigstring_of_string (Body.to_string b))
  | `Stream st ->
      let out = ref (Cohttp.Bigstring.create 0) and len = ref 0 in
      let add s =
        let n = String.length s in
        let cap = Cohttp.Bigstring.length !out in
        if !len + n > cap then (
          let grown = ref (max cap 65536) in
          while !len + n > !grown do
            grown := !grown * 2
          done;
          let bigger = Cohttp.Bigstring.create !grown in
          Cohttp.Bigstring.blit !out ~src_off:0 bigger ~dst_off:0 ~len:!len;
          out := bigger);
        Cohttp.Bigstring.blit_from_string s ~src_off:0 !out ~dst_off:!len ~len:n;
        len := !len + n
      in
      Lwt_stream.iter add st >>= fun () ->
      return (Cohttp.Bigstring.sub !out ~off:0 ~len:!len)

let to_stream (body : t) =
  match body with
  | `Empty -> Lwt_stream.of_list []
  | `Stream s -> s
  | `Bigstring b -> Lwt_stream.of_list [ bigstring_to_string b ]
  | `String s -> Lwt_stream.of_list [ s ]
  | `Strings sl -> Lwt_stream.of_list sl

let drain_body (body : t) =
  match body with
  | `Empty | `String _ | `Strings _ | `Bigstring _ -> return_unit
  | `Stream s -> Lwt_stream.junk_while (fun _ -> true) s

let of_string_list l = `Strings l
let of_stream s = `Stream s
let of_bigstring b = `Bigstring b

let transfer_encoding = function
  | #Body.t as t -> Body.transfer_encoding t
  | `Bigstring b -> Transfer.Fixed (Int64.of_int (bigstring_length b))
  | `Stream _ -> Transfer.Chunked

(* This will consume the body and return a length, and a
 * new body that should be used instead of the input *)
let length (body : t) : (int64 * t) Lwt.t =
  match body with
  | #Body.t as body -> return (Body.length body, body)
  | `Bigstring b as body -> return (Int64.of_int (bigstring_length b), body)
  | `Stream _ ->
      to_string body >>= fun buf ->
      let len = Int64.of_int (String.length buf) in
      return (len, `String buf)

let write_body ?write_bigstring fn = function
  | `Empty -> return_unit
  | `Stream st -> Lwt_stream.iter_s fn st
  | `String s -> fn s
  | `Strings sl -> Lwt_list.iter_s fn sl
  | `Bigstring b -> (
      match write_bigstring with
      | Some write -> write b 0 (bigstring_length b)
      | None -> fn (bigstring_to_string b))

let map f t =
  match t with
  | #Body.t as t -> (Body.map f t :> t)
  | `Stream s -> `Stream (Lwt_stream.map f s)
  | `Bigstring b -> `String (f (bigstring_to_string b))

let to_form (body : t) = to_string body >|= Uri.query_of_encoded
let of_form ?scheme f = Uri.encoded_of_query ?scheme f |> of_string

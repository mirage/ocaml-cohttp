type name = string
type value = string
type 'a header = ..

(** Defines headers common to both Request and Response. *)
type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
(*   | Date : Ptime.t header *)

(* The following module 'Cmp' is based on `Order' module defined at
   https://github.com/hannesm/gmap/blob/main/gmap.mli
   It has the copyright as below:
   (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Cmp = struct
  type (_, _) t = Lt : ('a, 'b) t | Eq : ('a, 'a) t | Gt : ('a, 'b) t
end

exception Unrecognized_header of string

let err_unrecognized_header hdr =
  let nm = Obj.Extension_constructor.of_val hdr in
  raise (Unrecognized_header (Obj.Extension_constructor.name nm))

let compare : type a b. a header -> b header -> (a, b) Cmp.t =
 fun t t' ->
  match (t, t') with
  | Content_length, Content_length -> Eq
  | Content_length, _ -> Lt
  | _, Content_length -> Gt
  | Transfer_encoding, Transfer_encoding -> Eq
  | Transfer_encoding, _ -> Lt
  | _, Transfer_encoding -> Gt
  (*
  | Date, Date -> Eq
  | Date, _ -> Lt
  | _, Date -> Gt
*)
  | hdr, _ -> err_unrecognized_header hdr

let decode : type a. a header -> string -> a lazy_t =
 fun hdr s ->
  match hdr with
  | Content_length -> lazy (int_of_string s)
  | Transfer_encoding ->
      lazy
        (String.split_on_char ',' s
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
        |> List.map (fun te ->
               match te with
               | "chunked" -> `chunked
               | "compress" -> `compress
               | "deflate" -> `deflate
               | "gzip" -> `gzip
               | v -> failwith @@ "Invalid 'Transfer-Encoding' value " ^ v))
  | _ -> err_unrecognized_header hdr

let name_value (type a) (hdr : a header) (v : a) : string * string =
  match hdr with
  | Content_length -> ("Content-Length", string_of_int v)
  | Transfer_encoding ->
      let v =
        List.map
          (function
            | `chunked -> "chunked"
            | `compress -> "compress"
            | `deflate -> "deflate"
            | `gzip -> "gzip")
          v
        |> String.concat ","
      in
      ("Transfer-Encoding", v)
  | _ ->
      let nm = Obj.Extension_constructor.of_val hdr in
      raise (Unrecognized_header (Obj.Extension_constructor.name nm))

module type HEADER = sig
  type 'a t = 'a header

  val compare : 'a t -> 'b t -> ('a, 'b) Cmp.t
  val decode : 'a t -> string -> 'a lazy_t
end

module type S = sig
  type t
  type 'a key

  val empty : t
  val add_string_val : 'a key -> string -> t -> t
  val add : 'a key -> 'a lazy_t -> t -> t
  val find : 'a key -> t -> 'a
  val find_opt : 'a key -> t -> 'a option
end

module Make (Header : HEADER) : S = struct
  type 'a key = 'a Header.t
  type h = H : 'a key -> h
  type v = V : 'a key * 'a lazy_t -> v

  module M = Map.Make (struct
    type t = h

    let compare (H a) (H b) =
      match Header.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1
  end)

  type t = v M.t

  let empty = M.empty
  let add_string_val k s t = M.add (H k) (V (k, decode k s)) t
  let add k v t = M.add (H k) (V (k, v)) t

  let find : type a. a key -> t -> a =
   fun k t ->
    match M.find (H k) t with
    | V (k', v) -> (
        match Header.compare k k' with Eq -> Lazy.force v | _ -> assert false)

  let find_opt : type a. a key -> t -> a option =
   fun k t -> match find k t with v -> Some v | exception Not_found -> None
end

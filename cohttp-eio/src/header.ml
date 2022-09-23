type 'a t = ..
type name = string
type value = string

type 'a t +=
  | Content_length : int t
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list t
  | Header : name -> value t

let compare (type a b) (a : a t) (b : b t) : (a, b) Gmap.Order.t =
  let open Gmap.Order in
  match (a, b) with
  | Content_length, Content_length -> Eq
  | Content_length, _ -> Lt
  | _, Content_length -> Gt
  | Transfer_encoding, Transfer_encoding -> Eq
  | Transfer_encoding, _ -> Lt
  | _, Transfer_encoding -> Gt
  | Header h1, Header h2 ->
      (* TODO optimize compare *)
      if Http.Header.Private.caseless_equal h1 h2 then Eq
      else
        let x1 = String.lowercase_ascii h1 and x2 = String.lowercase_ascii h2 in
        let cmp = String.compare x1 x2 in
        if cmp = -1 then Lt else Gt
  | _ -> failwith "Unrecognized header"

type name = string (* Header name, e.g. Date, Content-Length etc *)
type value = string (* Header value, eg 10, text/html, chunked etc *)
type lname = string

let canonical_name nm =
  String.split_on_char '-' nm
  |> List.map (fun s -> String.(lowercase_ascii s |> capitalize_ascii))
  |> String.concat "-"

let lname = String.lowercase_ascii

type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | H : lname -> value header

type 'a decoder = value -> 'a
type 'a encoder = 'a -> value
type 'a undecoded = 'a Lazy.t
type (_, _) eq = Eq : ('a, 'a) eq

let canonical_name nm =
  String.split_on_char '-' nm
  |> List.map (fun s -> String.(lowercase_ascii s |> capitalize_ascii))
  |> String.concat "-"

module Codec = struct
  class type t =
    object
      method header : 'a. lname -> 'a header
      method equal : 'a 'b. 'a header -> 'b header -> ('a, 'b) eq option
      method decoder : 'a. 'a header -> 'a decoder
      method encoder : 'a. 'a header -> 'a encoder
      method name : 'a. 'a header -> name
    end

  let int_decoder v = int_of_string v
  let int_encoder v = string_of_int v

  (* Transfer-Encoding decoder. *)
  let te_decoder v =
    String.split_on_char ',' v
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.map (fun te ->
           match te with
           | "chunked" -> `chunked
           | "compress" -> `compress
           | "deflate" -> `deflate
           | "gzip" -> `gzip
           | v -> failwith @@ "Invalid 'Transfer-Encoding' value " ^ v)

  (* Transfer-Encoding encoder. *)
  let te_encoder v =
    List.map
      (function
        | `chunked -> "chunked"
        | `compress -> "compress"
        | `deflate -> "deflate"
        | `gzip -> "gzip")
      v
    |> String.concat ", "

  let constructor_name hdr =
    let nm = Obj.Extension_constructor.of_val hdr in
    Obj.Extension_constructor.name nm

  let v : t =
    object
      method header : 'a. lname -> 'a header =
        function
        | "content-length" -> Obj.magic Content_length
        | "transfer-encoding" -> Obj.magic Transfer_encoding
        | h -> Obj.magic (H h)

      method equal : type a b. a header -> b header -> (a, b) eq option =
        fun a b ->
          match (a, b) with
          | Content_length, Content_length -> Some Eq
          | Transfer_encoding, Transfer_encoding -> Some Eq
          | H a, H b -> if String.equal a b then Some Eq else None
          | _ -> None

      method decoder : type a. a header -> a decoder =
        function
        | Content_length -> int_decoder
        | Transfer_encoding -> te_decoder
        | H _ -> Fun.id
        | hdr ->
            let err = "decoder undefined for header " ^ constructor_name hdr in
            raise @@ Invalid_argument err

      method encoder : type a. a header -> a encoder =
        function
        | Content_length -> int_encoder
        | Transfer_encoding -> te_encoder
        | H _ -> Fun.id
        | hdr ->
            let err = "encoder undefined for header " ^ constructor_name hdr in
            raise @@ Invalid_argument err

      method name : type a. a header -> name =
        function
        | Content_length -> "Content-Length"
        | Transfer_encoding -> "Transfer-Encoding"
        | H name -> canonical_name name
        | hdr ->
            let err = "name undefined for header " ^ constructor_name hdr in
            raise @@ Invalid_argument err
    end
end

type name = string
type value = string
type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Date : Ptime.t header

type (_, _) eq = Eq : ('a, 'a) eq

type header_ext = {
  decode : 'a. 'a header -> string -> string -> ('a, string) result;
  equal : 'a 'b. 'a header -> 'b header -> ('a, 'b) eq option;
}

exception Unrecognized_header of string
exception Duplicate_header of string

let hdr_ext : (int, header_ext) Hashtbl.t = Hashtbl.create 5
let id t = Obj.Extension_constructor.(of_val t |> id)

let extend (type a) (t : a header) header =
  let id = id t in
  match Hashtbl.find hdr_ext id with
  | _ ->
      let hdr = Obj.Extension_constructor.(of_val t |> name) in
      raise @@ Duplicate_header hdr
  | exception Not_found -> Hashtbl.replace hdr_ext id header

let equal (type a b) (a : a header) (b : b header) : (a, b) eq option =
  let ( let* ) = Option.bind in
  match (a, b) with
  | Content_length, Content_length -> Some Eq
  | Transfer_encoding, Transfer_encoding -> Some Eq
  | Date, Date -> Some Eq
  | _ ->
      let* header = Hashtbl.find_opt hdr_ext (id a) in
      header.equal a b

let http_date ptime =
  let (year, mm, dd), ((hh, min, ss), _) = Ptime.to_date_time ptime in
  let weekday = Ptime.weekday ptime in
  let weekday =
    match weekday with
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
    | `Sun -> "Sun"
  in
  let month =
    match mm with
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Aug"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _ -> failwith "Invalid HTTP datetime value"
  in
  Format.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday dd month year hh
    min ss

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
  | Date ->
      let http_date = http_date v in
      ("Date", http_date)
  | _ ->
      let nm = Obj.Extension_constructor.of_val hdr in
      raise (Unrecognized_header (Obj.Extension_constructor.name nm))

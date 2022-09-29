type 'a t = ..
type name = string
type value = string

type 'a t +=
  | Content_length : int t
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list t
  | Date : Ptime.t t
  | Header : name -> value t

exception Unrecognized_header of string

let compare (type a b) (a : a t) (b : b t) : (a, b) Gmap.Order.t =
  let open Gmap.Order in
  match (a, b) with
  | Content_length, Content_length -> Eq
  | Content_length, _ -> Lt
  | _, Content_length -> Gt
  | Transfer_encoding, Transfer_encoding -> Eq
  | Transfer_encoding, _ -> Lt
  | _, Transfer_encoding -> Gt
  | Date, Date -> Eq
  | Date, _ -> Lt
  | _, Date -> Gt
  | Header h1, Header h2 ->
      (* TODO optimize compare *)
      if Http.Header.Private.caseless_equal h1 h2 then Eq
      else
        let x1 = String.lowercase_ascii h1 and x2 = String.lowercase_ascii h2 in
        let cmp = String.compare x1 x2 in
        if cmp = -1 then Lt else Gt
  | _ ->
      let nm1 = Obj.Extension_constructor.(of_val a |> name) in
      let nm2 = Obj.Extension_constructor.(of_val b |> name) in
      raise @@ Unrecognized_header (nm1 ^ ", " ^ nm2)

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

let name_value (type a) (hdr : a t) (v : a) : string * string =
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
  | Header nm -> (nm, v)
  | _ ->
      let nm = Obj.Extension_constructor.of_val hdr in
      raise (Unrecognized_header (Obj.Extension_constructor.name nm))

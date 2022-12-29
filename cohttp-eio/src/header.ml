type name = string
type value = string
type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Date : Ptime.t header

type (_, _) eq = Eq : ('a, 'a) eq

(* (c) 2017, 2018 Hannes Mehnert, all rights reserved *)
module Order = struct
  type (_, _) t = Lt : ('a, 'b) t | Eq : ('a, 'a) t | Gt : ('a, 'b) t
end

type header_ext = {
  decode : 'a. 'a header -> string -> 'a;
  compare : 'a 'b. 'a header -> 'b header -> ('a, 'b) Order.t;
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

let err_unrecognized_header hdr =
  let nm = Obj.Extension_constructor.of_val hdr in
  raise (Unrecognized_header (Obj.Extension_constructor.name nm))

let compare : type a b. a header -> b header -> (a, b) Order.t =
 fun t t' ->
  match (t, t') with
  | Content_length, Content_length -> Eq
  | Content_length, _ -> Lt
  | _, Content_length -> Gt
  | a, b -> (
      match Hashtbl.find_opt hdr_ext (id a) with
      | Some ext -> ext.compare a b
      | None -> err_unrecognized_header a)

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
  | _ -> (
      match Hashtbl.find_opt hdr_ext (id hdr) with
      | Some ext -> lazy (ext.decode hdr s)
      | None -> err_unrecognized_header hdr)

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

module type HEADER = sig
  type 'a t = 'a header

  val compare : 'a t -> 'b t -> ('a, 'b) Order.t
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

module Make (H : HEADER) : S = struct
  type 'a key = 'a H.t
  type h = H : 'a key -> h
  type v = V : 'a key * 'a lazy_t -> v

  module M = Map.Make (struct
    type t = h

    let compare (H a) (H b) =
      match H.compare a b with Order.Lt -> -1 | Order.Eq -> 0 | Order.Gt -> 1
  end)

  type t = v M.t

  let empty = M.empty
  let add_string_val k s t = M.add (H k) (V (k, decode k s)) t
  let add k v t = M.add (H k) (V (k, v)) t

  let find : type a. a key -> t -> a =
   fun k t ->
    match M.find (H k) t with
    | V (k', v) -> (
        match H.compare k k' with Order.Eq -> Lazy.force v | _ -> assert false)

  let find_opt : type a. a key -> t -> a option =
   fun k t -> match find k t with v -> Some v | exception Not_found -> None
end

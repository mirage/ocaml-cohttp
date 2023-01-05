type 'a header = ..

(** Defines headers common to both Request and Response. *)
type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Hdr : string -> string header  (** A generic header *)

type (_, _) eq = Eq : ('a, 'a) eq

exception Unrecognized_header of string

let err_unrecognized_header hdr =
  let nm = Obj.Extension_constructor.of_val hdr in
  raise (Unrecognized_header (Obj.Extension_constructor.name nm))

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

  val equal : 'a t -> 'b t -> ('a, 'b) eq option
  val decode : 'a t -> string -> 'a Lazy.t
end

module type S = sig
  type t
  type 'a key

  val empty : t
  val add_string_val : 'a key -> string -> t -> t
  val add : 'a key -> 'a Lazy.t -> t -> t
  val find : 'a key -> t -> 'a
  val find_opt : 'a key -> t -> 'a option
end

module Make (Header : HEADER) : S = struct
  type 'a key = 'a Header.t
  type v = V : 'a key * 'a Lazy.t -> v

  module M = Map.Make (struct
    type t = int

    let compare (a : int) (b : int) = Int.compare a b
  end)

  let equal : type a b. a header -> b header -> (a, b) eq option =
   fun t t' ->
    match (t, t') with
    | Content_length, Content_length -> Some Eq
    | Transfer_encoding, Transfer_encoding -> Some Eq
    | Hdr a, Hdr b -> if String.equal a b then Some Eq else None
    | a, b -> Header.equal a b

  let decode : type a. a header -> string -> a Lazy.t =
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
    | Hdr _ -> lazy s
    | hdr -> Header.decode hdr s

  type t = v M.t

  let empty = M.empty
  let hash = Hashtbl.hash

  let add_string_val k s t =
    let key = hash k in
    M.add key (V (k, decode k s)) t

  let add k v t = M.add (hash k) (V (k, v)) t

  let find : type a. a key -> t -> a =
   fun k t ->
    match M.find (hash k) t with
    | V (k', v) -> (
        match equal k k' with
        | Some Eq -> Lazy.force v
        | None -> raise Not_found)

  let find_opt : type a. a key -> t -> a option =
   fun k t -> match find k t with v -> Some v | exception Not_found -> None
end

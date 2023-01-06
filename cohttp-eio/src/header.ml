type 'a header = ..
type lowercase_id = string
(* Represents a unique header id value.

   If you are providing this value, ensure it is in lowercase via
   {!String.lowercase_ascii} or other suitable functions. *)

(** Common headers to both Request and Response. *)
type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | Hdr : lowercase_id -> string header
        (** A generic header. See {!type:lowercase_id}. *)

type (_, _) eq = Eq : ('a, 'a) eq

exception Decoder_undefined of string
exception Id_undefined of string
exception Equal_undefined of string

module type HEADER = sig
  type 'a t = 'a header

  val t : lowercase_id -> 'a t option

  val id : 'a t -> lowercase_id option
  (** [id hdr] is [Some id] where [id] is a unique lowercased string
      representation of [hdr] *)

  val equal : 'a t -> 'b t -> ('a, 'b) eq option
  val decoder : 'a t -> (string -> 'a) option
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

module Make (Header : HEADER) : sig
  include S

  val add_key_val : key:lowercase_id -> value:string -> t -> t
end
with type 'a key = 'a Header.t = struct
  type 'a key = 'a Header.t
  type v = V : 'a key * 'a Lazy.t -> v

  module M = Map.Make (struct
    type t = int

    let compare = Int.compare
  end)

  type t = v M.t

  let constructor_name hdr =
    let nm = Obj.Extension_constructor.of_val hdr in
    Obj.Extension_constructor.name nm

  let equal : type a b. a key -> b key -> (a, b) eq option =
   fun a b ->
    match Header.equal a b with
    | Some _ as s -> s
    | None -> (
        match (a, b) with
        | Content_length, Content_length -> Some Eq
        | Transfer_encoding, Transfer_encoding -> Some Eq
        | Hdr a, Hdr b -> if String.equal a b then Some Eq else None
        | a, _ -> raise @@ Equal_undefined (constructor_name a))

  let decode : type a. a key -> string -> a Lazy.t =
   fun hdr hdr_val ->
    match Header.decoder hdr with
    | Some decode -> lazy (decode hdr_val)
    | None -> (
        match hdr with
        | Content_length -> lazy (int_of_string hdr_val)
        | Transfer_encoding ->
            lazy
              (String.split_on_char ',' hdr_val
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
              |> List.map (fun te ->
                     match te with
                     | "chunked" -> `chunked
                     | "compress" -> `compress
                     | "deflate" -> `deflate
                     | "gzip" -> `gzip
                     | v -> failwith @@ "Invalid 'Transfer-Encoding' value " ^ v)
              )
        | Hdr _ -> lazy hdr_val
        | hdr -> raise @@ Decoder_undefined (constructor_name hdr))

  let id : type a. a key -> string =
   fun hdr ->
    match Header.id hdr with
    | Some id -> id
    | None -> (
        match hdr with
        | Content_length -> "content-length"
        | Transfer_encoding -> "transfer-encoding"
        | Hdr h -> h
        | _ -> raise @@ Id_undefined (constructor_name hdr))

  let header_t : type a. string -> a key =
   fun s ->
    match Header.t s with
    | Some t -> t
    | None -> (
        match s with
        | "content-length" -> Obj.magic Content_length
        | "transfer-encoding" -> Obj.magic Transfer_encoding
        | h -> Obj.magic (Hdr h))

  let empty = M.empty
  let hash = Hashtbl.hash

  let add_string_val k s t =
    let key = hash (id k) in
    M.add key (V (k, decode k s)) t

  let add_key_val ~key ~value t =
    let k = header_t key in
    add_string_val k value t

  let add k v t = M.add (hash k) (V (k, v)) t

  let find : type a. a key -> t -> a =
   fun k t ->
    let key = hash (id k) in
    match M.find key t with
    | V (k', v) -> (
        match equal k k' with
        | Some Eq -> Lazy.force v
        | None -> raise Not_found)

  let find_opt : type a. a key -> t -> a option =
   fun k t -> match find k t with v -> Some v | exception Not_found -> None
end

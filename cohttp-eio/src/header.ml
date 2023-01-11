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

module type HEADER = sig
  type 'a t = 'a header

  val t : lowercase_id -> 'a t option

  val id : 'a t -> lowercase_id option
  (** [id hdr] is [Some id] where [id] is a unique lowercased string
      representation of [hdr] *)

  val equal : 'a t -> 'b t -> ('a, 'b) eq option
  val decoder : 'a t -> (string -> 'a) option
  val encoder : 'a t -> ('a -> string) option
end

module type S = sig
  type t
  type 'a key
  type binding = B : 'a key * 'a -> binding
  type mapper = { f : 'a. 'a key -> 'a -> 'a }

  val empty : t
  val add_string_val : 'a key -> string -> t -> t
  val add : 'a key -> 'a -> t -> t
  val find : 'a key -> t -> 'a
  val find_opt : 'a key -> t -> 'a option
  val iter : (binding -> unit) -> t -> unit
  val map : mapper -> t -> t
  val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
  val remove : 'a key -> t -> t
  val update : 'a key -> ('a option -> 'a option) -> t -> t
end

module Make (Header : HEADER) : sig
  include S

  val add_key_val : key:lowercase_id -> value:string -> t -> t
end
with type 'a key = 'a Header.t = struct
  type 'a key = 'a Header.t
  type v = V : 'a key * 'a Lazy.t -> v  (** Lazy value *)
  type binding = B : 'a key * 'a -> binding
  type mapper = { f : 'a. 'a key -> 'a -> 'a }

  module M = Map.Make (struct
    type t = string

    let compare = String.compare
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
        | _, _ -> None)

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

  let add_string_val k s t =
    let key = id k in
    M.add key (V (k, decode k s)) t

  let add_key_val ~key ~value t =
    let k = header_t key in
    M.add key (V (k, decode k value)) t

  let add k v t = M.add (id k) (V (k, lazy v)) t

  let find : type a. a key -> t -> a =
   fun k t ->
    let key = id k in
    match M.find key t with
    | V (k', v) -> (
        match equal k k' with
        | Some Eq -> Lazy.force v
        | None -> raise Not_found)

  let find_opt : type a. a key -> t -> a option =
   fun k t -> match find k t with v -> Some v | exception Not_found -> None

  let iter f t =
    M.iter
      (fun _k v -> match v with V (key, v) -> f @@ B (key, Lazy.force v))
      t

  let map mapper t =
    M.map
      (fun v ->
        match v with
        | V (k, v) ->
            let v = mapper.f k @@ Lazy.force v in
            V (k, lazy v))
      t

  let fold f t =
    M.fold
      (fun _key v acc -> match v with V (k, v) -> f (B (k, Lazy.force v)) acc)
      t

  let remove key t = M.remove (id key) t

  let update key f t =
    match f (find_opt key t) with None -> remove key t | Some v -> add key v t
end

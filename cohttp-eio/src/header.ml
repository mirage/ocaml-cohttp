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

class codec =
  let int_decoder v = int_of_string v in
  let int_encoder v = string_of_int v in

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
  in
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
  in
  let constructor_name hdr =
    let nm = Obj.Extension_constructor.of_val hdr in
    Obj.Extension_constructor.name nm
  in
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

let name (c : #codec) = c#name

type v = V : 'a header * 'a Lazy.t -> v

class virtual t =
  object
    inherit codec
    method virtual headers : v list Atomic.t
    method virtual to_list : v list
    method virtual modify : (v list -> v list) -> unit
  end

let make_n (c : #codec) values : t =
  let headers = Atomic.make values in
  let rec modify f r =
    let v_old = Atomic.get r in
    let v_new = f v_old in
    if Atomic.compare_and_set r v_old v_new then () else modify f r
  in
  object
    method header : 'a. lname -> 'a header = c#header
    method equal : type a b. a header -> b header -> (a, b) eq option = c#equal
    method decoder : type a. a header -> a decoder = c#decoder
    method encoder : type a. a header -> a encoder = c#encoder
    method name : type a. a header -> name = c#name
    method headers : v list Atomic.t = headers
    method to_list : v list = Atomic.get headers
    method modify : (v list -> v list) -> unit = fun f -> modify f headers
  end

let make code = make_n code []

let of_name_values (c : #codec) l =
  List.map
    (fun (name, value) ->
      let h = c#header (lname name) in
      let v = lazy (c#decoder h value) in
      V (h, v))
    l
  |> make_n c

let length (t : #t) = List.length t#to_list

let add_lazy (type a) (t : t) (h : a header) v =
  t#modify (fun l -> V (h, v) :: l)

let add (type a) (t : t) (h : a header) v = add_lazy t h (lazy v)

let add_value (t : t) h value =
  let v = lazy (t#decoder h value) in
  add_lazy t h v

let add_name_value (t : t) ~name ~value =
  let h = t#header name in
  let v = lazy (t#decoder h value) in
  add_lazy t h v

let encode : type a. t -> a header -> a -> string = fun t h v -> t#encoder h v
let decode : type a. a undecoded -> a = Lazy.force

let exists (t : #t) (f : < f : 'a. 'a header -> 'a undecoded -> bool >) =
  let rec aux = function
    | [] -> false
    | V (h, v) :: tl -> if f#f h v then true else aux tl
  in
  aux t#to_list

let find_opt (type a) (t : #t) (h : a header) =
  let rec aux = function
    | [] -> None
    | V (h', v) :: tl -> (
        match t#equal h h' with
        | Some Eq -> ( try Some (Lazy.force v :> a) with _ -> None)
        | None -> aux tl)
  in
  aux t#to_list

let find (type a) (t : #t) (h : a header) =
  let rec aux = function
    | [] -> raise Not_found
    | V (h', v) :: tl -> (
        match t#equal h h' with
        | Some Eq -> (Lazy.force v :> a)
        | None -> aux tl)
  in
  aux t#to_list

let find_all (type a) (t : #t) (h : a header) : a undecoded list =
  let[@tail_mod_cons] rec aux = function
    | [] -> []
    | V (h', v) :: tl -> (
        match t#equal h h' with
        | Some Eq -> (v :> a undecoded) :: aux tl
        | None -> aux tl)
  in
  aux t#to_list

let update (t : #t) (f : < f : 'a. 'a header -> 'a undecoded -> 'a option >) =
  t#modify
    (List.filter_map (fun (V (h, v)) ->
         let v = f#f h v in
         Option.map (fun v -> V (h, lazy v)) v))

let remove (type a) ?(all = false) (t : #t) (h : a header) =
  t#modify (fun headers ->
      let _, headers =
        List.fold_left
          (fun (first, acc) (V (h', _v) as orig_v) ->
            match t#equal h h' with
            | Some Eq ->
                if first || ((not first) && all) then (false, acc)
                else (first, orig_v :: acc)
            | None -> (first, orig_v :: acc))
          (true, []) headers
      in
      headers)

type binding = B : 'a header * 'a undecoded -> binding

let iter (t : #t) (f : < f : 'a. 'a header -> 'a undecoded -> unit >) =
  List.iter (fun (V (h, v)) -> f#f h v) t#to_list

let fold_left (t : #t) (f : < f : 'a. 'a header -> 'a undecoded -> 'b -> 'b >)
    acc =
  List.fold_left (fun acc (V (h, v)) -> f#f h v acc) acc t#to_list

let to_seq (t : #t) =
  List.map (fun (V (h, v)) -> B (h, v)) t#to_list |> List.to_seq

let to_name_values (t : #t) =
  List.map
    (fun (V (h, v)) ->
      let name = t#name h in
      let value = encode t h (Lazy.force v) in
      (name, value))
    t#to_list

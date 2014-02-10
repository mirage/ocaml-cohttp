open Sexplib.Std

type t = [
  | `Empty
  | `String of string
] with sexp

let empty = `Empty

let to_string = function
  | `Empty -> ""
  | `String s -> s

let of_string s = `String s
let of_string_list s = `String (String.concat "" s)

let transfer_encoding (t:t) =
  match t with
  | `Empty -> Transfer.Fixed 0
  | `String s -> Transfer.Fixed (String.length s)

let length = function
  | `Empty -> 0
  | `String s -> String.length s

module type S = sig
  type t
  val to_string : t -> string
  val empty : t
  val of_string : string -> t
  val of_string_list : string list -> t
  val transfer_encoding : t -> Transfer.encoding
  val length : t -> int
end

(* TODO: maybe add a functor here that uses IO.S *)

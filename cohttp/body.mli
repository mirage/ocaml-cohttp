type t = [
  | `Empty
  | `String of string
] with sexp

module type S = sig
  type t
  val to_string : t -> string
  val empty : t
  val of_string : string -> t
  val of_string_list : string list -> t
  val transfer_encoding : t -> Transfer.encoding
  val length : t -> int
end

include S with type t := t

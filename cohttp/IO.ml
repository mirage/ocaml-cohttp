module type S = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t
  val return : 'a -> 'a t

  type ic
  type oc

  val iter : ('a -> unit t) -> 'a list -> unit t
  val read_line : ic -> string option t
  val read : ic -> int -> string t
  val read_exactly : ic -> string -> int -> int -> bool t

  val write : oc -> string -> unit t
end

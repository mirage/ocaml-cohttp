module type S = sig
  type version = IPv4 | IPv6
  type t

  val loopback : version -> int -> t
  val any : version -> int -> t

  val make : string -> int -> t
  val to_string : t -> string
  val addr : t -> string
  val port : t -> int
end

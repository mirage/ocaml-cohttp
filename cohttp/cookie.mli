type expiration = [ `Session ]

type cookie = string * string
(** A cookie is simply a key/value pair send from the client to the server *)

module Set_cookie_hdr : sig

  type t
  (** A header which a server sends to a client to request that the client
    returns the cookie in future requests, under certain conditions. *)

  val make :
    ?expiration:expiration ->
    ?path:string ->
    ?domain:string -> ?secure:bool -> cookie -> t

  val serialize :
    ?version:[ `HTTP_1_0 | `HTTP_1_1 ] ->
    t -> string * string
  (** Return an HTTP header *)

  val extract : Header.t -> (string * t) list
  (** Return the list of cookies sent by the server *)

  val cookie : t -> cookie
  (** The name-value binding *)

  val value : t -> string
  (** The value *)

  val expiration : t -> expiration
  (** The expiration *)

  val domain : t -> string option
  (** The domain for which the cookie is valid, if any *)

  val path : t -> string option
  (** The path for which the cookie is valid, if any *)

  val secure : t -> bool
  (** Has the cookie's secure attribute been set? *)
end

module Cookie_hdr : sig

  val extract : Header.t -> cookie list
  (** Return the list of cookies sent by the client *)

  val serialize : cookie list -> string * string
  (** [serialize cookies] returns an HTTP header containing [cookies] *)
end

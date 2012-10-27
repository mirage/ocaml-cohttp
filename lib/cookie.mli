type expiration = [ `Session ]

type cookie = string * string
(** A cookie is simply a key/value pair send from the client to the server *)

module Set_cookie_hdr : sig

	type t
	(** A header which a server sends to a client to request that the client
		returns the cookie in future requests, under certain conditions. *)

	val make :
		?expiry:expiration ->
		?path:string ->
		?domain:string -> ?secure:bool -> cookie -> t

	val serialize :
		?version:[ `HTTP_1_0 | `HTTP_1_1 ] ->
		t -> string * string
	(** Return an HTTP header *)

end

module Cookie_hdr : sig

	val extract : Header.t -> cookie list
	(** Return the list of cookies sent by the client *)

end

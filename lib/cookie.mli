type expiration = [ `Session ]
type cookie

val make :
  ?expiry:expiration ->
  ?path:string ->
  ?domain:string -> ?secure:bool -> string -> string -> string * cookie
val serialize :
  ?version:[ `HTTP_1_0 | `HTTP_1_1 ] ->
  string * cookie -> string * string
val extract : Header.t -> (string * string) list


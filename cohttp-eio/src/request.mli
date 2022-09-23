type host = string * int option
type resource_path = string
type 'a Header.t += Host : host Header.t | User_agent : string Header.t

(** HTTP/1.1 headers. *)
module Headers : sig
  include Gmap.S with type 'a key = 'a Header.t

  val header_to_name_value : b -> string * string
end

type t

val make :
  ?meth:Http.Method.t ->
  ?version:Http.Version.t ->
  ?headers:Headers.t ->
  host ->
  resource_path ->
  t

val meth : t -> Http.Method.t
val headers : t -> Headers.t
val version : t -> Http.Version.t
val resource_path : t -> resource_path

type host = string * int option
type resource_path = string

type 'a Header.header +=
  | Host : host Header.header
  | User_agent : string Header.header

type t

val make :
  ?meth:Http.Method.t -> ?version:Http.Version.t -> host -> resource_path -> t

val meth : t -> Http.Method.t
val version : t -> Http.Version.t
val resource_path : t -> resource_path

type 'a t =
  | Get : Body.none t
  | Head : Body.none t
  | Delete : Body.none t
  | Options : Body.none t
  | Trace : Body.none t
  | Post : 'a t
  | Put : 'a t
  | Patch : 'a t
  | Connect : Body.none t

let to_string (type a) (m : a t) =
  match m with
  | Get -> "GET"
  | Head -> "HEAD"
  | Delete -> "DELETE"
  | Options -> "OPTIONS"
  | Trace -> "TRACE"
  | Post -> "POST"
  | Put -> "PUT"
  | Patch -> "PATCH"
  | Connect -> "CONNECT"

let of_string (type a) s : a t =
  match String.uppercase_ascii s with
  | "GET" -> Obj.magic Get
  | "HEAD" -> Obj.magic Head
  | "DELETE" -> Obj.magic Delete
  | "OPTIONS" -> Obj.magic Options
  | "TRACE" -> Obj.magic Trace
  | "POST" -> Obj.magic Post
  | "PUT" -> Obj.magic Put
  | "PATCH" -> Obj.magic Patch
  | "CONNECT" -> Obj.magic Connect
  | _ -> raise @@ Invalid_argument ("Unsupported header: " ^ s)

let pp fmt t = Format.fprintf fmt "%s" (to_string t)

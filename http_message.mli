type message
val body : message -> string
val body_buf : message -> Buffer.t
val set_body : message -> string -> unit
val set_body_buf : message -> Buffer.t -> unit
val add_body : message -> string -> unit
val add_body_buf : message -> Buffer.t -> unit
val add_header : message -> name:string -> value:string -> unit
val add_headers : message -> (string * string) list -> unit
val replace_header : message -> name:string -> value:string -> unit
val replace_headers : message -> (string * string) list -> unit
val remove_header : message -> name:string -> unit
val has_header : message -> name:string -> bool
val header : message -> name:string -> string option
val headers : message -> (string * string) list
val client_addr : message -> string
val server_addr : message -> string
val client_port : message -> int
val server_port : message -> int
val version : message -> Http_types.version option
val set_version : message -> Http_types.version -> unit
val init :
  body:string ->
  headers:(string * string) list ->
  version:Http_types.version option ->
  clisockaddr:Unix.sockaddr -> srvsockaddr:Unix.sockaddr -> message
val serialize :
  message -> Lwt_io.output_channel -> fstLineToString:string -> unit Lwt.t

type contents =
    [ `Buffer of Buffer.t
    | `String of string
    | `Inchan of int64 * Lwt_io.input_channel
    ]
type message
val body : message -> contents list
val body_size : contents list -> int64
val string_of_body : contents list -> string Lwt.t
val set_body : message -> contents -> unit
val add_body : message -> contents -> unit
val add_header : message -> name:string -> value:string -> unit
val add_headers : message -> (string * string) list -> unit
val replace_header : message -> name:string -> value:string -> unit
val replace_headers : message -> (string * string) list -> unit
val remove_header : message -> name:string -> unit
val has_header : message -> name:string -> bool
val header : message -> name:string -> string list
val headers : message -> (string * string) list
val client_addr : message -> string
val server_addr : message -> string
val client_port : message -> int
val server_port : message -> int
val version : message -> Http_types.version
val init :
  body:contents list ->
  headers:(string * string) list ->
  version:Http_types.version ->
  clisockaddr:Unix.sockaddr -> srvsockaddr:Unix.sockaddr -> message
val serialize :
  message -> Lwt_io.output_channel -> fstLineToString:string -> unit Lwt.t

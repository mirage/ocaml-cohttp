type request
val init_request :
  clisockaddr:Unix.sockaddr ->
  srvsockaddr:Unix.sockaddr ->
  unit Lwt.u -> Lwt_io.input_channel -> request Lwt.t
val meth : request -> Types.meth
val uri : request -> Uri.t
val path : request -> string
val body : request -> Message.contents list
val param :
  ?meth:[< `GET | `POST ] -> ?default:string -> request -> string -> string
val param_all : ?meth:Types.meth -> request -> string -> string list
val params : request -> (string, string) Hashtbl.t
val params_get : request -> (string * string) list
val params_post : request -> (string * string) list
val header : request -> name:string -> string list
val headers : request -> (string * string) list

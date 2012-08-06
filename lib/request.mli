type request

val parse : IO.ic -> request option IO.M.t

val meth : request -> Code.meth
val uri : request -> Uri.t
val version : request -> Code.version

val path : request -> string

val header : name:string -> request -> string list

val params_get : request -> (string * string) list
val params_post : request -> (string * string) list
val param : string -> request -> string option

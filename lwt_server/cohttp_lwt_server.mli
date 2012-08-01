open Cohttp

type conn_id
val string_of_conn_id : conn_id -> string

type daemon_spec = {
  address : string;
  callback : conn_id -> Request.request -> string Lwt_stream.t Lwt.t;
  conn_closed : conn_id -> unit;
  port : int;
  root_dir : string option;
  exn_handler : exn -> unit Lwt.t;
  timeout : int option;
  auto_close : bool;
}
val respond :
  ?body:string ->
  ?headers:(string * string) list ->
  ?version:Types.version ->
  ?status:Types.status_code -> unit -> string Lwt_stream.t Lwt.t
val respond_control :
  string ->
  ?is_valid_status:(int -> bool) ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?version:Types.version ->
  Types.status_code -> string Lwt_stream.t Lwt.t
val respond_redirect :
  location:string ->
  ?body:string ->
  ?version:Types.version ->
  ?status:Types.status_code -> unit -> string Lwt_stream.t Lwt.t
val respond_error :
  ?body:string ->
  ?version:Types.version ->
  ?status:Types.status_code -> unit -> string Lwt_stream.t Lwt.t
val respond_not_found :
  url:'a ->
  ?version:Types.version -> unit -> string Lwt_stream.t Lwt.t
val respond_forbidden :
  url:'a ->
  ?version:Types.version -> unit -> string Lwt_stream.t Lwt.t
val respond_unauthorized :
  ?version:'a -> ?realm:string -> unit -> string Lwt_stream.t Lwt.t
val respond_file :
  fname:Lwt_io.file_name ->
  ?droot:string ->
  ?version:Types.version -> ?mime_type: string -> unit -> string Lwt_stream.t Lwt.t
val respond_with :
  Response.response -> string Lwt_stream.t Lwt.t
val daemon_callback :
  daemon_spec ->
  clisockaddr:Unix.sockaddr -> srvsockaddr:Unix.sockaddr -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
val main : daemon_spec -> 'a Lwt.t
val default_spec : daemon_spec

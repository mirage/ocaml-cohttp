type daemon_spec = {
  address : string;
  auth : (string * Http_types.auth_info) option;
  callback : Http_request.request -> Lwt_io.output_channel -> unit Lwt.t;
  port : int;
  root_dir : string option;
  exn_handler : (exn -> Lwt_io.output_channel -> unit Lwt.t) option;
  timeout : int option;
  auto_close : bool;
}
val respond :
  ?body:string ->
  ?headers:(string * string) list ->
  ?version:Http_types.version ->
  ?code:Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val send_empty_response :
  string ->
  ?is_valid_status:(int -> bool) ->
  ?headers:(string * string) list ->
  ?body:string ->
  unit ->
  ?version:Http_types.version ->
  Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_redirect :
  location:string ->
  ?body:string ->
  ?version:Http_types.version ->
  ?code:Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_error :
  ?body:string ->
  ?version:Http_types.version ->
  ?code:Http_types.status_code -> Lwt_io.output_channel -> unit Lwt.t
val respond_not_found :
  url:'a ->
  ?version:Http_types.version -> Lwt_io.output_channel -> unit Lwt.t
val respond_forbidden :
  url:'a ->
  ?version:Http_types.version -> Lwt_io.output_channel -> unit Lwt.t
val respond_unauthorized :
  ?version:'a -> ?realm:string -> Lwt_io.output_channel -> unit Lwt.t
val send_file : Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t
val respond_file :
  fname:Lwt_io.file_name ->
  ?version:Http_types.version -> ?mime_type: string ->  Lwt_io.output_channel -> unit Lwt.t
val respond_with :
  Http_response.response -> Lwt_io.output_channel -> unit Lwt.t
val main : daemon_spec -> 'a Lwt.t
module Trivial :
  sig
    val heading_slash_RE : Pcre.regexp
    val trivial_callback :
      Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val callback :
      Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val main : daemon_spec -> 'a Lwt.t
  end
val default_spec : daemon_spec

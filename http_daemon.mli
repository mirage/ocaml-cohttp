open Http_types

type daemon_spec = {
  address: string;
    (** @param address adress on which daemon will be listening, can be both a
    * numeric address (e.g. "127.0.0.1") and an hostname (e.g. "localhost") *)
  auth: (string * auth_info) option;
    (** authentication requirements (currently only basic authentication is
    * supported). If set to None no authentication is required. If set to Some
    * ("realm", `Basic ("foo", "bar")), only clients authenticated with baisc
    * authentication, for realm "realm", providing username "foo" and password
    * "bar" are accepted; others are rejected with a 401 response code *)
  callback: Http_request.request -> Lwt_io.output_channel -> unit Lwt.t;
    (** function which will be called each time a correct HTTP request will be
    * received. 1st callback argument is an Http_types.request object
    * corresponding to the request received; 2nd argument is an output channel
    * corresponding to the socket connected to the client *)
  port: int;  (** TCP port on which the daemon will be listening *)
  root_dir: string option;
    (** directory to which ocaml http will chdir before starting handling
    * requests; if None, no chdir will be performed (i.e. stay in the current
    * working directory) *)
  exn_handler: (exn -> Lwt_io.output_channel -> unit Lwt.t) option;
    (** what to do when executing callback raises an exception.  If None, the
    * exception will be re-raised: the exception is ignored and the client 
    * socket closed. If Some callback, the callback will be executed before 
    * acting as per None; the callback is meant to perform some clean up 
    * actions *)
  timeout: int option;
    (** timeout in seconds after which an incoming HTTP request will be
    * terminated closing the corresponding TCP connection; None disable the
    * timeout *)
  auto_close: bool;
    (** whether ocaml-http will automatically close the connection with the
     * client after callback has completed its execution. If set to true, close
     * will be attempted no matter if the callback raises an exception or not *)
}

val main : daemon_spec -> unit Lwt.t
module Trivial :
  sig
    val heading_slash_RE : Pcre.regexp
    val trivial_callback :
      Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val callback :
      Http_request.request -> Lwt_io.output_channel -> unit Lwt.t
    val main : daemon_spec -> unit Lwt.t
  end
val default_spec : daemon_spec


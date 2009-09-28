
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

(** Main OCaml HTTP module.
    Here you can find two set of functions:
    - functions which let you start an HTTP Daemon (start* functions)
    - facility functions which let you sent responses back to clients *)

  (** send a CRLF sequence on the given output channel, this is mandatory after
  the last header was sent and before start sending the response body *)
val send_CRLF: out_channel -> unit

  (** send response status line, version is the http version used in response,
  either code or status must be given (not both, not none) which represent the
  HTTP response code, outchan is the output channel to which send status line *)
val send_status_line:
  ?version:Http_types.version -> code:Http_types.status_code ->
  out_channel ->
    unit

  (** like send_status_line but additionally will also send "Date" and "Server"
  standard headers *)
val send_basic_headers:
  ?version: Http_types.version -> code:Http_types.status_code ->
  out_channel ->
    unit

  (** send an HTTP header on outchan *)
val send_header: header: string -> value: string -> out_channel -> unit

  (** as send_header, but for a list of pairs <header, value> *)
val send_headers: headers:(string * string) list -> out_channel -> unit

(*
  (** send a file through an out_channel, file can be passed as an in_channel
  (if 'file' is given) or as a file name (if 'name' is given) *)
val send_file: ?name:string -> ?file:in_channel -> out_channel -> unit
*)
  (** send a file through an out_channel *)
val send_file: src:Http_types.file_source -> out_channel -> unit

  (** high level response function, respond on outchan sending: basic headers
  (including Content-Length computed using 'body' argument), headers probided
  via 'headers' argument, body given via 'body' argument.  Default response
  status is 200, default response HTTP version is Http_common.http_version *)
val respond:
  ?body:string -> ?headers:(string * string) list ->
  ?version:Http_types.version -> ?code:Http_types.status_code ->
  out_channel ->
    unit

  (** send a 404 (not found) HTTP response *)
val respond_not_found:
  url:string -> ?version: Http_types.version -> out_channel -> unit

  (** send a 403 (forbidden) HTTP response *)
val respond_forbidden:
  url:string -> ?version: Http_types.version -> out_channel -> unit

  (** send a "redirection" class response, optional body argument contains data
  that will be displayed in the body of the response, default response status is
  301 (moved permanently), only redirection status are accepted by this
  function, other values will raise Failure *)
val respond_redirect:
  location:string -> ?body:string ->
  ?version: Http_types.version -> ?code:Http_types.status_code ->
  out_channel ->
    unit

  (** respond with a 401 (Unauthorized) response asking for authentication
  * against given realm (default is the server name) *)
val respond_unauthorized:
  ?version: Http_types.version -> ?realm:string -> out_channel -> unit

  (** send an "error" response (i.e. 400 <= status < 600), optional body
  argument as per send_redirect, default response status is 400 (bad request),
  only error status are accepted by this function, other values will
  raise Failure *)
val respond_error:
  ?body:string ->
  ?version: Http_types.version -> ?code:Http_types.status_code ->
  out_channel ->
    unit

  (** tipical static pages http daemon behaviour, if requested url is a file,
  return it, it it is a directory return a directory listing of it *)
val respond_file:
  fname:string -> ?version: Http_types.version -> out_channel -> unit

  (** respond using a prebuilt Http_types.response object *)
val respond_with: Http_types.response -> out_channel -> unit

  (** start an HTTP daemon
  * @param spec specification of daemon behaviour
  *)
val main: Http_types.daemon_spec -> unit

  (** default daemon specification:
  * - listen on 0.0.0.0, port 80
  * - "always ok" callback  (return an empty response, response code 200)
  * - fork a child for each request
  * - do not change to a root directory (i.e. keep cwd)
  * - 300 seconds timeout
  * - ignores exceptions
  * - no authentication required
  * - do not automatically close client connections after callback *)
val default_spec: Http_types.daemon_spec

  (** currified daemon_spec constructor. Each parameter of this function
    * corresponds to one field of Http_types.daemon_spec and defaults to the
    * corresponding field of Http_daemon.default_spec *)
val daemon_spec:
  ?address:string ->
  ?auth:(string * Http_types.auth_info) option ->
  ?auto_close:bool ->
  ?callback:(Http_types.request -> out_channel -> unit) ->
  ?mode:(Http_types.daemon_mode) ->
  ?port:int ->
  ?root_dir:string option ->
  ?exn_handler:(exn -> out_channel -> unit) option ->
  ?timeout:int option ->
  unit ->
    Http_types.daemon_spec

(*
(** XXX
 * This function has been deprecated for a while. Now it has been removed! *)
val start:
  ?addr: string -> ?port: int ->
  ?timeout: int option -> ?mode: Http_types.daemon_mode -> ?root: string ->
  (string -> (string * string) list -> out_channel -> unit) ->
    unit
*)

(*
(** XXX
 * This function has been deprecated for a while. Now it has been removed! *)
val start':
  ?addr: string -> ?port: int ->
  ?timeout: int option -> ?mode: Http_types.daemon_mode -> ?root: string -> 
  (Http_types.request -> out_channel -> unit) ->
    unit
*)

  (** Object oriented interface to HTTP daemons.
  * @param addr address on which daemon will listen for connections
  * @param port port which daemon will bind
  * see {!Http_types.daemon} *)
class daemon:
  ?addr: string -> ?port: int ->
    unit ->
      Http_types.daemon

  (** Trivial static pages HTTP daemon.
  * Daemons created using this module will serve directory indexes and files
  * found starting from the working directory *)
module Trivial :
  sig
      (** callback function, exposed if you like to use it as a basis to define
      a more powerful daemon *)
    val callback : Http_types.request -> out_channel -> unit

      (** start the "trivial" HTTP daemon
      * @param spec trivial HTTP daemon specification, "callback" field is
      * ignored and set to the callback above *)
    val main : Http_types.daemon_spec -> unit
  end


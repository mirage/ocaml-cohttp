
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

(** Type definitions *)

  (** HTTP version, actually only 1.0 and 1.1 are supported. Note that
  'supported' here means only 'accepted inside a HTTP request line', no
  different behaviours are actually implemented depending on HTTP version *)
type version =
  [ `HTTP_1_0
  | `HTTP_1_1
  ]

  (** HTTP method, actually only GET and POST methods are supported *)
type meth =
  [ `GET
  | `POST
  ]

  (** Daemon behaviour wrt request handling. `Single mode use a single process
  to handle all requests, no request is served until a previous one has been
  fully served. `Fork mode fork a new process for each request, the new process
  will execute the callback function and then exit. `Thread mode create a new
  thread for each request, the new thread will execute the callback function and
  then exit, threads can communicate using standard OCaml Thread library. *)
type daemon_mode = [ `Single | `Fork | `Thread ]

  (** A TCP server is a function taking an address on which bind and listen for
  connections, an optional timeout after which abort client connections and a
  callback function which in turn takes an input and an output channel as
  arguments. After receiving this argument a TCP server sits and waits for
  connection, on each connection it apply the callback function to channels
  connected to client. *)
type tcp_server =
  sockaddr:Unix.sockaddr -> timeout:int option ->
  (in_channel -> out_channel -> unit) ->
    unit

  (** authentication information *)
type auth_info =
  [ `Basic of string * string (* username, password *)
(*   | `Digest of ...  (* TODO digest authentication *) *)
  ]

  (** @see "RFC2616" informational HTTP status *)
type informational_substatus =
  [ `Continue
  | `Switching_protocols
  ]

  (** @see "RFC2616" success HTTP status *)
type success_substatus =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  ]

  (** @see "RFC2616" redirection HTTP status *)
type redirection_substatus =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  ]

  (** @see "RFC2616" client error HTTP status *)
type client_error_substatus =
  [ `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_time_out
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Request_entity_too_large
  | `Request_URI_too_large
  | `Unsupported_media_type
  | `Requested_range_not_satisfiable
  | `Expectation_failed
  ]

  (** @see "RFC2616" server error HTTP status *)
type server_error_substatus =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_time_out
  | `HTTP_version_not_supported
  ]

type informational_status = [ `Informational of informational_substatus ]
type success_status = [ `Success of success_substatus ]
type redirection_status = [ `Redirection of redirection_substatus ]
type client_error_status = [ `Client_error of client_error_substatus ]
type server_error_status = [ `Server_error of server_error_substatus ]

type error_status =
  [ client_error_status
  | server_error_status
  ]

  (** HTTP status *)
type status =
  [ informational_status
  | success_status
  | redirection_status
  | client_error_status
  | server_error_status
  ]

type status_code = [ `Code of int | `Status of status ]

  (** File sources *)
type file_source =
  | FileSrc of string           (** filename *)
  | InChanSrc of in_channel     (** input channel *)

  (** {2 Exceptions} *)

  (** invalid header encountered *)
exception Invalid_header of string

  (** invalid header name encountered *)
exception Invalid_header_name of string

  (** invalid header value encountered *)
exception Invalid_header_value of string

  (** unsupported or invalid HTTP version encountered *)
exception Invalid_HTTP_version of string

  (** unsupported or invalid HTTP method encountered *)
exception Invalid_HTTP_method of string

  (** invalid HTTP status code integer representation encountered *)
exception Invalid_code of int

  (** invalid URL encountered *)
exception Malformed_URL of string

  (** invalid query string encountered *)
exception Malformed_query of string

  (** invalid query string part encountered, arguments are parameter name and
  parameter value *)
exception Malformed_query_part of string * string

  (** invalid request URI encountered *)
exception Malformed_request_URI of string

  (** malformed request received *)
exception Malformed_request of string

  (** malformed response received, argument is response's first line *)
exception Malformed_response of string

  (** a parameter you were looking for was not found *)
exception Param_not_found of string

  (** invalid HTTP status line encountered *)
exception Invalid_status_line of string

  (** an header you were looking for was not found *)
exception Header_not_found of string

  (** raisable by callbacks to make main daemon quit, this is the only
  * 'clean' way to make start functions return *)
exception Quit

  (** raisable by callbacks to force a 401 (unauthorized) HTTP answer.
  * This exception should be raised _before_ sending any data over given out
  * channel.
  * @param realm authentication realm (usually needed to prompt user) *)
exception Unauthorized of string

  (** {2 OO representation of HTTP messages} *)

  (** HTTP generic messages. See {! Http_message.message} *)
class type message = object

    method version: version option
    method setVersion: version -> unit

    method body: string
    method setBody: string -> unit
    method bodyBuf: Buffer.t
    method setBodyBuf: Buffer.t -> unit
    method addBody: string -> unit
    method addBodyBuf: Buffer.t -> unit

    method addHeader: name:string -> value:string -> unit
    method addHeaders: (string * string) list -> unit
    method replaceHeader: name:string -> value:string -> unit
    method replaceHeaders: (string * string) list -> unit
    method removeHeader: name:string -> unit
    method hasHeader: name:string -> bool
    method header: name:string -> string
    method headers: (string * string) list

    method clientSockaddr: Unix.sockaddr
    method clientAddr: string
    method clientPort: int

    method serverSockaddr: Unix.sockaddr
    method serverAddr: string
    method serverPort: int

    method toString: string
    method serialize: out_channel -> unit

  end

  (** HTTP requests *)
class type request = object

      (** an HTTP request is a flavour of HTTP message *)
    inherit message

      (** @return request method *)
    method meth: meth

      (** @return requested URI (including query string, fragment, ...) *)
    method uri: string

      (** @return requested path *)
    method path: string

      (** lookup a given parameter
      @param meth if given restrict the lookup area (e.g. if meth = POST than
        only parameters received via POST are searched), if not given both GET
        and POST parameter are searched in an unspecified order (actually the
        implementation prefers POST parameters but this is not granted, you've
        been warned)
      @param default if provided, this value will be returned in case no
        parameter of that name is available instead of raising Param_not_found
      @param name name of the parameter to lookup
      @return value associated to parameter name
      @raise Param_not_found if parameter name was not found *)
    method param: ?meth:meth -> ?default:string -> string -> string

      (** like param above but return a list of values associated to given
      parameter (a parameter could be defined indeed more than once: passed more
      than once in a query string or passed both insider the url (the GET way)
      and inside message body (the POST way)) *)
    method paramAll: ?meth:meth -> string -> string list

      (** @return the list of all received parameters *)
    method params: (string * string) list

      (** @return the list of all parameters received via GET *)
    method params_GET: (string * string) list

      (** @return the list of all parameter received via POST *)
    method params_POST: (string * string) list

      (** @return authorization information, if given by the client *)
    method authorization: auth_info option

  end

  (** HTTP responses *)
class type response = object

    inherit message

      (** @return response code *)
    method code: int

      (** set response code *)
    method setCode: int -> unit

      (** @return response status *)
    method status: status

      (** set response status *)
    method setStatus: status -> unit

      (** @return reason string *)
    method reason: string

      (** set reason string *)
    method setReason: string -> unit

      (** @return status line *)
    method statusLine: string

      (** set status line
      @raise Invalid_status_line if an invalid HTTP status line was passed *)
    method setStatusLine: string -> unit

      (** response is an informational one *)
    method isInformational: bool

      (** response is a success one *)
    method isSuccess: bool

      (** response is a redirection one *)
    method isRedirection: bool

      (** response is a client error one *)
    method isClientError: bool

      (** response is a server error one *)
    method isServerError: bool

      (** response is either a client error or a server error response *)
    method isError: bool

      (** add basic headers to response, see {!Http_daemon.send_basic_headers}
      *)
    method addBasicHeaders: unit

      (** facilities to access some frequently used headers *)

      (** @return Content-Type header value *)
    method contentType: string

      (** set Content-Type header value *)
    method setContentType: string -> unit

      (** @return Content-Encoding header value *)
    method contentEncoding: string

      (** set Content-Encoding header value *)
    method setContentEncoding: string -> unit

      (** @return Date header value *)
    method date: string

      (** set Date header value *)
    method setDate: string -> unit

      (** @return Expires header value *)
    method expires: string

      (** set Expires header value *)
    method setExpires: string -> unit

      (** @return Server header value *)
    method server: string

      (** set Server header value *)
    method setServer: string -> unit

  end

  (** {2 Daemon specification} *)

  (** daemon specification, describe the behaviour of an HTTP daemon.
  *
  * The default daemon specification is {!Http_daemon.default_spec}
  *)
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
  callback: request -> out_channel -> unit;
    (** function which will be called each time a correct HTTP request will be
    * received. 1st callback argument is an Http_types.request object
    * corresponding to the request received; 2nd argument is an output channel
    * corresponding to the socket connected to the client *)
  mode: daemon_mode;
    (** requests handling mode, it can have three different values:
    * - `Single -> all requests will be handled by the same process,
    * - `Fork   -> each request will be handled by a child process,
    * - `Thread -> each request will be handled by a (new) thread *)
  port: int;  (** TCP port on which the daemon will be listening *)
  root_dir: string option;
    (** directory to which ocaml http will chdir before starting handling
    * requests; if None, no chdir will be performed (i.e. stay in the current
    * working directory) *)
  exn_handler: (exn -> out_channel -> unit) option;
    (** what to do when executing callback raises an exception.  If None, the
    * exception will be re-raised: in `Fork/`Thread mode the current
    * process/thread will be terminated. in `Single mode the exception is
    * ignored and the client socket closed. If Some callback, the callback will
    * be executed before acting as per None; the callback is meant to perform
    * some clean up actions, like releasing global mutexes in `Thread mode *)
  timeout: int option;
    (** timeout in seconds after which an incoming HTTP request will be
    * terminated closing the corresponding TCP connection; None disable the
    * timeout *)
  auto_close: bool;
    (** whether ocaml-http will automatically close the connection with the
     * client after callback has completed its execution. If set to true, close
     * will be attempted no matter if the callback raises an exception or not *)
}

  (** {2 OO representation of other HTTP entities} *)

  (** an HTTP connection from a client to a server *)
class type connection =
  object
      (** @return next request object, may block if client hasn't submitted any
      request yet, may be None if client request was ill-formed *)
    method getRequest: request option

      (** respond to client sending it a response *)
    method respond_with: response -> unit

      (** close connection to client. Warning: this object can't be used any
      longer after this method has been called *)
    method close: unit
  end

  (** an HTTP daemon *)
class type daemon =
  object
      (** @return a connection to a client, may block if no client has connected
      yet *)
    method accept: connection

      (** shortcut method, blocks until a client has submit a request and
      return a pair request * connection *)
    method getRequest: request * connection
  end



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

type version = [ `HTTP_1_0 | `HTTP_1_1 ]
type meth = [ `GET | `POST ]
type daemon_mode = [ `Single | `Fork | `Thread ]

type tcp_server =
  sockaddr:Unix.sockaddr -> timeout:int option ->
  (in_channel -> out_channel -> unit) ->
    unit

type auth_info =
  [ `Basic of string * string (* username, password *)
  ]

type informational_substatus =
  [ `Continue
  | `Switching_protocols
  ]
type success_substatus =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  ]
type redirection_substatus =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  ]
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
type status =
  [ informational_status
  | success_status
  | redirection_status
  | client_error_status
  | server_error_status
  ]

type status_code = [ `Code of int | `Status of status ]

type file_source =
  | FileSrc of string
  | InChanSrc of in_channel

exception Invalid_header of string
exception Invalid_header_name of string
exception Invalid_header_value of string
exception Invalid_HTTP_version of string
exception Invalid_HTTP_method of string
exception Invalid_code of int
exception Malformed_URL of string
exception Malformed_query of string
exception Malformed_query_part of string * string
exception Malformed_request_URI of string
exception Malformed_request of string
exception Malformed_response of string
exception Param_not_found of string
exception Invalid_status_line of string
exception Header_not_found of string
exception Quit
exception Unauthorized of string

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

class type request = object
    inherit message
    method meth: meth
    method uri: string
    method path: string
    method param: ?meth:meth -> ?default:string -> string -> string
    method paramAll: ?meth:meth -> string -> string list
    method params: (string * string) list
    method params_GET: (string * string) list
    method params_POST: (string * string) list
    method authorization: auth_info option
  end

class type response = object
    inherit message
    method code: int
    method setCode: int -> unit
    method status: status
    method setStatus: status -> unit
    method reason: string
    method setReason: string -> unit
    method statusLine: string
    method setStatusLine: string -> unit
    method isInformational: bool
    method isSuccess: bool
    method isRedirection: bool
    method isClientError: bool
    method isServerError: bool
    method isError: bool
    method addBasicHeaders: unit
    method contentType: string
    method setContentType: string -> unit
    method contentEncoding: string
    method setContentEncoding: string -> unit
    method date: string
    method setDate: string -> unit
    method expires: string
    method setExpires: string -> unit
    method server: string
    method setServer: string -> unit
  end

class type connection =
  object
    method getRequest: request option
    method respond_with: response -> unit
    method close: unit
  end
class type daemon =
  object
    method accept: connection
    method getRequest: request * connection
  end

type daemon_spec = {
  address: string;
  auth: (string * auth_info) option;
  callback: request -> out_channel -> unit;
  mode: daemon_mode;
  port: int;
  root_dir: string option;
  exn_handler: (exn -> out_channel -> unit) option;
  timeout: int option;
  auto_close: bool;
}


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

type version = [ `HTTP_1_0 | `HTTP_1_1 ]
type meth = [ `GET | `POST | `HEAD | `DELETE | `PATCH | `PUT | `OPTIONS ]

type informational_status =
  [ `Continue
  | `Switching_protocols
  ]
type success_status =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  ]
type redirection_status =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  ]
type client_error_status =
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
  | `Unprocessable_entity
  ]
type server_error_status =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_time_out
  | `HTTP_version_not_supported
  ]
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

type status_code = [ `Code of int | status ]

  (** pretty print an HTTP version *)
val string_of_version: version -> string

  (** parse an HTTP version from a string
  @raise Invalid_HTTP_version if given string doesn't represent a supported HTTP
  version *)
val version_of_string: string -> version option

  (** pretty print an HTTP method *)
val string_of_method: meth -> string

  (** parse an HTTP method from a string
  @raise Invalid_HTTP_method if given string doesn't represent a supported
  method *)
val method_of_string: string -> meth option

  (** converts an integer HTTP status to the corresponding status value
  @raise Invalid_code if given integer isn't a valid HTTP status code *)
val status_of_code: int -> status_code

  (** converts an HTTP status to the corresponding integer value *)
val code_of_status: status_code -> int

  (** converts an HTTP status to a human-readable string value *)
val string_of_status: status_code -> string

  (** converts an HTTP status to the corresponding RFC reason phrase *)
val reason_phrase_of_code: int -> string

  (** @return true on "informational" status codes, false elsewhere *)
val is_informational: int -> bool

  (** @return true on "success" status codes, false elsewhere *)
val is_success: int -> bool

  (** @return true on "redirection" status codes, false elsewhere *)
val is_redirection: int -> bool

  (** @return true on "client error" status codes, false elsewhere *)
val is_client_error: int -> bool

  (** @return true on "server error" status codes, false elsewhere *)
val is_server_error: int -> bool

  (** @return true on "client error" and "server error" status code, false
  elsewhere *)
val is_error: int -> bool

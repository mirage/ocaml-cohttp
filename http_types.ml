
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
  Copyright (C) <2009> Anil Madhavapeddy <anil@recoil.org>

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
type meth = [ `GET | `POST | `HEAD | `DELETE ]

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
exception Unauthorized of string


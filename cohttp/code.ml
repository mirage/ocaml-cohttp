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
type meth = [ `GET | `POST | `HEAD | `DELETE |`PATCH |`PUT | `OPTIONS ]

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

(* Manipulate the code types *)

let string_of_version = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"

let version_of_string = function
  | "HTTP/1.0" -> Some `HTTP_1_0
  | "HTTP/1.1" -> Some `HTTP_1_1
  | _ -> None

let string_of_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `HEAD -> "HEAD"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `OPTIONS -> "OPTIONS"
  | `PUT -> "PUT"

let method_of_string = function
  | "GET" -> Some `GET
  | "POST" -> Some `POST
  | "HEAD" -> Some `HEAD
  | "DELETE" -> Some `DELETE
  | "PATCH" -> Some `PATCH
  | "PUT" -> Some `PUT
  | "OPTIONS" -> Some `OPTIONS
  | _ -> None

let status_of_code (x:int) : status_code = match x with
  | 100 -> `Continue
  | 101 -> `Switching_protocols
  | 200 -> `OK
  | 201 -> `Created
  | 202 -> `Accepted
  | 203 -> `Non_authoritative_information
  | 204 -> `No_content
  | 205 -> `Reset_content
  | 206 -> `Partial_content
  | 300 -> `Multiple_choices
  | 301 -> `Moved_permanently
  | 302 -> `Found
  | 303 -> `See_other
  | 304 -> `Not_modified
  | 305 -> `Use_proxy
  | 307 -> `Temporary_redirect
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 402 -> `Payment_required
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | 406 -> `Not_acceptable
  | 407 -> `Proxy_authentication_required
  | 408 -> `Request_time_out
  | 409 -> `Conflict
  | 410 -> `Gone
  | 411 -> `Length_required
  | 412 -> `Precondition_failed
  | 413 -> `Request_entity_too_large
  | 414 -> `Request_URI_too_large
  | 415 -> `Unsupported_media_type
  | 416 -> `Requested_range_not_satisfiable
  | 417 -> `Expectation_failed
  | 422 -> `Unprocessable_entity
  | 500 -> `Internal_server_error
  | 501 -> `Not_implemented
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 504 -> `Gateway_time_out
  | 505 -> `HTTP_version_not_supported
  | code -> `Code code

let code_of_status = function
  | `Continue -> 100
  | `Switching_protocols -> 101
  | `OK -> 200
  | `Created -> 201
  | `Accepted -> 202
  | `Non_authoritative_information -> 203
  | `No_content -> 204
  | `Reset_content -> 205
  | `Partial_content -> 206
  | `Multiple_choices -> 300
  | `Moved_permanently -> 301
  | `Found -> 302
  | `See_other -> 303
  | `Not_modified -> 304
  | `Use_proxy -> 305
  | `Temporary_redirect -> 307
  | `Bad_request -> 400
  | `Unauthorized -> 401
  | `Payment_required -> 402
  | `Forbidden -> 403
  | `Not_found -> 404
  | `Method_not_allowed -> 405
  | `Not_acceptable -> 406
  | `Proxy_authentication_required -> 407
  | `Request_time_out -> 408
  | `Conflict -> 409
  | `Gone -> 410
  | `Length_required -> 411
  | `Precondition_failed -> 412
  | `Request_entity_too_large -> 413
  | `Request_URI_too_large -> 414
  | `Unsupported_media_type -> 415
  | `Requested_range_not_satisfiable -> 416
  | `Expectation_failed -> 417
  | `Unprocessable_entity -> 422
  | `Internal_server_error -> 500
  | `Not_implemented -> 501
  | `Bad_gateway -> 502
  | `Service_unavailable -> 503
  | `Gateway_time_out -> 504
  | `HTTP_version_not_supported -> 505
  | `Code code -> code

let string_of_status = function
  | `Continue -> "100 Continue"
  | `Switching_protocols -> "101 Switching Protocols"
  | `OK -> "200 OK"
  | `Created -> "201 Created"
  | `Accepted -> "202 Accepted"
  | `Non_authoritative_information -> "203 Non-authoritative Information"
  | `No_content -> "204 No Content"
  | `Reset_content -> "205 Reset Content"
  | `Partial_content -> "206 Partial Content"
  | `Multiple_choices -> "300 Multiple Choices"
  | `Moved_permanently -> "301 Moved Permanently"
  | `Found -> "302 Found"
  | `See_other -> "303 See Other"
  | `Not_modified -> "304 Not Modified"
  | `Use_proxy -> "305 Use Proxy"
  | `Temporary_redirect -> "307 Temporary Redirect"
  | `Bad_request -> "400 Bad Request"
  | `Unauthorized -> "401 Unauthorized"
  | `Payment_required -> "402 Payment Required"
  | `Forbidden -> "403 Forbidden"
  | `Not_found -> "404 Not Found"
  | `Method_not_allowed -> "405 Method Not Allowed"
  | `Not_acceptable -> "406 Not Acceptable"
  | `Proxy_authentication_required -> "407 Proxy Authentication Required"
  | `Request_time_out -> "408 Request Time Out"
  | `Conflict -> "409 Conflict"
  | `Gone -> "410 Gone"
  | `Length_required -> "411 Length Required"
  | `Precondition_failed -> "412 Precondition Failed"
  | `Request_entity_too_large -> "413 Request Entity Too Large"
  | `Request_URI_too_large -> "414 Request URI Too Large"
  | `Unsupported_media_type -> "415 Unsupported Media Type"
  | `Requested_range_not_satisfiable -> "416 Requested Range Not Satisfiable"
  | `Expectation_failed -> "417 Expectation Failed"
  | `Unprocessable_entity -> "422 Unprocessable Entity"
  | `Internal_server_error -> "500 Internal Server Error"
  | `Not_implemented -> "501 Not Implemented"
  | `Bad_gateway -> "502 Bad Gateway"
  | `Service_unavailable -> "503 Service Unavailable"
  | `Gateway_time_out -> "504 Gateway Timeout"
  | `HTTP_version_not_supported -> "505 HTTP Version Not Supported"
  | `Code code -> string_of_int code

let reason_phrase_of_code = function
  | 100 -> "Continue"
  | 101 -> "Switching protocols"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non authoritative information"
  | 204 -> "No content"
  | 205 -> "Reset content"
  | 206 -> "Partial content"
  | 300 -> "Multiple choices"
  | 301 -> "Moved permanently"
  | 302 -> "Found"
  | 303 -> "See other"
  | 304 -> "Not modified"
  | 305 -> "Use proxy"
  | 307 -> "Temporary redirect"
  | 400 -> "Bad request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment required"
  | 403 -> "Forbidden"
  | 404 -> "Not found"
  | 405 -> "Method not allowed"
  | 406 -> "Not acceptable"
  | 407 -> "Proxy authentication required"
  | 408 -> "Request time out"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length required"
  | 412 -> "Precondition failed"
  | 413 -> "Request entity too large"
  | 414 -> "Request URI too large"
  | 415 -> "Unsupported media type"
  | 416 -> "Requested range not satisfiable"
  | 417 -> "Expectation failed"
  | 422 -> "Unprocessable entity"
  | 500 -> "Internal server error"
  | 501 -> "Not implemented"
  | 502 -> "Bad gateway"
  | 503 -> "Service unavailable"
  | 504 -> "Gateway time out"
  | 505 -> "HTTP version not supported"
  | invalid_code -> string_of_int invalid_code

let is_informational code =
  match status_of_code code with
  | #informational_status -> true
  | _ -> false

let is_success code =
  match status_of_code code with
  | #success_status -> true
  | _ -> false

let is_redirection code =
  match status_of_code code with
  | #redirection_status -> true
  | _ -> false

let is_client_error code =
  match status_of_code code with
  | #client_error_status -> true
  | _ -> false

let is_server_error code =
  match status_of_code code with
  | #server_error_status -> true
  | _ -> false

let is_error code = is_client_error code || is_server_error code

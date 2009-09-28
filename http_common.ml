
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

open Http_types;;
open Printf;;

let debug = ref false
let debug_print s =
  if !debug then
    prerr_endline (sprintf "[OCaml HTTP] DEBUG: %s" s)

let http_version = Http_constants.version
let server_string = Http_constants.server_string

let string_of_version = function
  | `HTTP_1_0 -> "HTTP/1.0"
  | `HTTP_1_1 -> "HTTP/1.1"

let version_of_string = function
  | "HTTP/1.0" -> `HTTP_1_0
  | "HTTP/1.1" -> `HTTP_1_1
  | invalid_version -> raise (Invalid_HTTP_version invalid_version)

let string_of_method = function
  | `GET -> "GET"
  | `POST -> "POST"

let method_of_string = function
  | "GET" -> `GET
  | "POST" -> `POST
  | invalid_method -> raise (Invalid_HTTP_method invalid_method)

let status_of_code = function
  | 100 -> `Informational `Continue
  | 101 -> `Informational `Switching_protocols
  | 200 -> `Success `OK
  | 201 -> `Success `Created
  | 202 -> `Success `Accepted
  | 203 -> `Success `Non_authoritative_information
  | 204 -> `Success `No_content
  | 205 -> `Success `Reset_content
  | 206 -> `Success `Partial_content
  | 300 -> `Redirection `Multiple_choices
  | 301 -> `Redirection `Moved_permanently
  | 302 -> `Redirection `Found
  | 303 -> `Redirection `See_other
  | 304 -> `Redirection `Not_modified
  | 305 -> `Redirection `Use_proxy
  | 307 -> `Redirection `Temporary_redirect
  | 400 -> `Client_error `Bad_request
  | 401 -> `Client_error `Unauthorized
  | 402 -> `Client_error `Payment_required
  | 403 -> `Client_error `Forbidden
  | 404 -> `Client_error `Not_found
  | 405 -> `Client_error `Method_not_allowed
  | 406 -> `Client_error `Not_acceptable
  | 407 -> `Client_error `Proxy_authentication_required
  | 408 -> `Client_error `Request_time_out
  | 409 -> `Client_error `Conflict
  | 410 -> `Client_error `Gone
  | 411 -> `Client_error `Length_required
  | 412 -> `Client_error `Precondition_failed
  | 413 -> `Client_error `Request_entity_too_large
  | 414 -> `Client_error `Request_URI_too_large
  | 415 -> `Client_error `Unsupported_media_type
  | 416 -> `Client_error `Requested_range_not_satisfiable
  | 417 -> `Client_error `Expectation_failed
  | 500 -> `Server_error `Internal_server_error
  | 501 -> `Server_error `Not_implemented
  | 502 -> `Server_error `Bad_gateway
  | 503 -> `Server_error `Service_unavailable
  | 504 -> `Server_error `Gateway_time_out
  | 505 -> `Server_error `HTTP_version_not_supported
  | invalid_code -> raise (Invalid_code invalid_code)

let code_of_status = function
  | `Informational `Continue -> 100
  | `Informational `Switching_protocols -> 101
  | `Success `OK -> 200
  | `Success `Created -> 201
  | `Success `Accepted -> 202
  | `Success `Non_authoritative_information -> 203
  | `Success `No_content -> 204
  | `Success `Reset_content -> 205
  | `Success `Partial_content -> 206
  | `Redirection `Multiple_choices -> 300
  | `Redirection `Moved_permanently -> 301
  | `Redirection `Found -> 302
  | `Redirection `See_other -> 303
  | `Redirection `Not_modified -> 304
  | `Redirection `Use_proxy -> 305
  | `Redirection `Temporary_redirect -> 307
  | `Client_error `Bad_request -> 400
  | `Client_error `Unauthorized -> 401
  | `Client_error `Payment_required -> 402
  | `Client_error `Forbidden -> 403
  | `Client_error `Not_found -> 404
  | `Client_error `Method_not_allowed -> 405
  | `Client_error `Not_acceptable -> 406
  | `Client_error `Proxy_authentication_required -> 407
  | `Client_error `Request_time_out -> 408
  | `Client_error `Conflict -> 409
  | `Client_error `Gone -> 410
  | `Client_error `Length_required -> 411
  | `Client_error `Precondition_failed -> 412
  | `Client_error `Request_entity_too_large -> 413
  | `Client_error `Request_URI_too_large -> 414
  | `Client_error `Unsupported_media_type -> 415
  | `Client_error `Requested_range_not_satisfiable -> 416
  | `Client_error `Expectation_failed -> 417
  | `Server_error `Internal_server_error -> 500
  | `Server_error `Not_implemented -> 501
  | `Server_error `Bad_gateway -> 502
  | `Server_error `Service_unavailable -> 503
  | `Server_error `Gateway_time_out -> 504
  | `Server_error `HTTP_version_not_supported -> 505

let is_informational code =
  match status_of_code code with
  | `Informational _ -> true
  | _ -> false

let is_success code =
  match status_of_code code with
  | `Success _ -> true
  | _ -> false

let is_redirection code =
  match status_of_code code with
  | `Redirection _ -> true
  | _ -> false

let is_client_error code =
  match status_of_code code with
  | `Client_error _ -> true
  | _ -> false

let is_server_error code =
  match status_of_code code with
  | `Server_error _ -> true
  | _ -> false

let is_error code = is_client_error code || is_server_error code


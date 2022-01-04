open! Sexplib0.Sexp_conv

type version = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ] [@@deriving sexp]

type meth =
  [ `GET
  | `POST
  | `HEAD
  | `DELETE
  | `PATCH
  | `PUT
  | `OPTIONS
  | `TRACE
  | `CONNECT
  | `Other of string ]
[@@deriving sexp]

type informational_status =
  [ `Continue | `Switching_protocols | `Processing | `Checkpoint ]
[@@deriving sexp]

type success_status =
  [ `OK
  | `Created
  | `Accepted
  | `Non_authoritative_information
  | `No_content
  | `Reset_content
  | `Partial_content
  | `Multi_status
  | `Already_reported
  | `Im_used ]
[@@deriving sexp]

type redirection_status =
  [ `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Switch_proxy
  | `Temporary_redirect
  | `Permanent_redirect ]
[@@deriving sexp]

type client_error_status =
  [ `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_authentication_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Request_entity_too_large
  | `Request_uri_too_long
  | `Unsupported_media_type
  | `Requested_range_not_satisfiable
  | `Expectation_failed
  | `I_m_a_teapot
  | `Enhance_your_calm
  | `Unprocessable_entity
  | `Locked
  | `Failed_dependency
  | `Upgrade_required
  | `Precondition_required
  | `Too_many_requests
  | `Request_header_fields_too_large
  | `No_response
  | `Retry_with
  | `Blocked_by_windows_parental_controls
  | `Wrong_exchange_server
  | `Client_closed_request ]
[@@deriving sexp]

type server_error_status =
  [ `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported
  | `Variant_also_negotiates
  | `Insufficient_storage
  | `Loop_detected
  | `Bandwidth_limit_exceeded
  | `Not_extended
  | `Network_authentication_required
  | `Network_read_timeout_error
  | `Network_connect_timeout_error ]
[@@deriving sexp]

type status =
  [ informational_status
  | success_status
  | redirection_status
  | client_error_status
  | server_error_status ]
[@@deriving sexp]

type status_code = [ `Code of int | status ] [@@deriving sexp]

let string_of_version = Http.Version.to_string
let version_of_string = Http.Version.of_string

let compare_version a b =
  String.compare (string_of_version a) (string_of_version b)

let string_of_method = Http.Method.to_string
let method_of_string = Http.Method.of_string

let compare_method a b =
  String.compare (string_of_method a) (string_of_method b)

let status_of_code = Http.Status.of_int
let code_of_status = Http.Status.to_int
let string_of_status = Http.Status.to_string
let reason_phrase_of_code = Http.Status.reason_phrase_of_code

let is_informational code =
  match status_of_code code with #informational_status -> true | _ -> false

let is_success code =
  match status_of_code code with #success_status -> true | _ -> false

let is_redirection code =
  match status_of_code code with #redirection_status -> true | _ -> false

let is_client_error code =
  match status_of_code code with #client_error_status -> true | _ -> false

let is_server_error code =
  match status_of_code code with #server_error_status -> true | _ -> false

let is_error code = is_client_error code || is_server_error code

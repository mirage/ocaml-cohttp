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
  [ `Continue  (** Client should continue with request *)
  | `Switching_protocols  (** Server is switching protocols *)
  | `Processing  (** Server has received and is processing the request *)
  | `Checkpoint  (** resume aborted PUT or POST requests *) ]
[@@deriving sexp]
(** Informational *)

type success_status =
  [ `OK  (** standard response for successful HTTP requests *)
  | `Created  (** request has been fulfilled; new resource created *)
  | `Accepted  (** request accepted, processing pending *)
  | `Non_authoritative_information
    (** request processed, information may be from another source *)
  | `No_content  (** request processed, no content returned *)
  | `Reset_content
    (** request processed, no content returned, reset document view *)
  | `Partial_content  (** partial resource return due to request header *)
  | `Multi_status  (** XML, can contain multiple separate responses *)
  | `Already_reported  (** results previously returned *)
  | `Im_used  (** request fulfilled, reponse is instance-manipulations *) ]
[@@deriving sexp]
(** Success *)

type redirection_status =
  [ `Multiple_choices  (** multiple options for the resource delivered *)
  | `Moved_permanently
    (** this and all future requests directed to the given URI *)
  | `Found  (** temporary response to request found via alternative URI *)
  | `See_other  (** permanent response to request found via alternative URI *)
  | `Not_modified  (** resource has not been modified since last requested *)
  | `Use_proxy  (** content located elsewhere, retrieve from there *)
  | `Switch_proxy  (** subsequent requests should use the specified proxy *)
  | `Temporary_redirect  (** connect again to different URI as provided *)
  | `Permanent_redirect
    (** connect again to a different URI using the same method *) ]
[@@deriving sexp]
(** Redirection *)

type client_error_status =
  [ `Bad_request  (** request cannot be fulfilled due to bad syntax *)
  | `Unauthorized  (** authentication is possible but has failed *)
  | `Payment_required  (** payment required, reserved for future use *)
  | `Forbidden  (** server refuses to respond to request *)
  | `Not_found  (** requested resource could not be found *)
  | `Method_not_allowed  (** request method not supported by that resource *)
  | `Not_acceptable
    (** content not acceptable according to the Accept headers *)
  | `Proxy_authentication_required
    (** client must first authenticate itself with the proxy *)
  | `Request_timeout  (** server timed out waiting for the request *)
  | `Conflict  (** request could not be processed because of conflict *)
  | `Gone
    (** resource is no longer available and will not be available again *)
  | `Length_required  (** request did not specify the length of its content *)
  | `Precondition_failed  (** server does not meet request preconditions *)
  | `Request_entity_too_large
    (** request is larger than the server is willing or able to process *)
  | `Request_uri_too_long
    (** URI provided was too long for the server to process *)
  | `Unsupported_media_type  (** server does not support media type *)
  | `Requested_range_not_satisfiable
    (** client has asked for unprovidable portion of the file *)
  | `Expectation_failed
    (** server cannot meet requirements of Expect request-header field *)
  | `I_m_a_teapot  (** I'm a teapot *)
  | `Enhance_your_calm  (** Twitter rate limiting *)
  | `Unprocessable_entity
    (** request unable to be followed due to semantic errors *)
  | `Locked  (** resource that is being accessed is locked *)
  | `Failed_dependency
    (** request failed due to failure of a previous request *)
  | `Upgrade_required  (** client should switch to a different protocol *)
  | `Precondition_required
    (** origin server requires the request to be conditional *)
  | `Too_many_requests
    (** user has sent too many requests in a given amount of time *)
  | `Request_header_fields_too_large
    (** server is unwilling to process the request *)
  | `No_response  (** server returns no information and closes the connection *)
  | `Retry_with  (** request should be retried after performing action *)
  | `Blocked_by_windows_parental_controls
    (** Windows Parental Controls blocking access to webpage *)
  | `Wrong_exchange_server  (** the server cannot reach the client's mailbox *)
  | `Client_closed_request
    (** connection closed by client while HTTP server is processing *) ]
[@@deriving sexp]
(** Client_error *)

type server_error_status =
  [ `Internal_server_error  (** generic error message *)
  | `Not_implemented
    (** server does not recognise method or lacks ability to fulfill *)
  | `Bad_gateway
    (** server received an invalid response from upstream server *)
  | `Service_unavailable  (** server is currently unavailable *)
  | `Gateway_timeout
    (** gateway did not receive response from upstream server *)
  | `Http_version_not_supported
    (** server does not support the HTTP protocol version *)
  | `Variant_also_negotiates
    (** content negotiation for the request results in a circular reference *)
  | `Insufficient_storage  (** server is unable to store the representation *)
  | `Loop_detected
    (** server detected an infinite loop while processing the request *)
  | `Bandwidth_limit_exceeded  (** bandwidth limit exceeded *)
  | `Not_extended  (** further extensions to the request are required *)
  | `Network_authentication_required
    (** client needs to authenticate to gain network access *)
  | `Network_read_timeout_error  (** network read timeout behind the proxy *)
  | `Network_connect_timeout_error
    (** network connect timeout behind the proxy *) ]
[@@deriving sexp]
(** Server_error *)

type status =
  [ informational_status
  | success_status
  | redirection_status
  | client_error_status
  | server_error_status ]
[@@deriving sexp]

type status_code = [ `Code of int | status ] [@@deriving sexp]

val string_of_version : version -> string
(** Convert a version to a string. *)

val version_of_string : string -> version
(** Convert a string to a version. *)

val compare_version : version -> version -> int
(** Comparison function for [version] values *)

val string_of_method : meth -> string
(** Convert a method to a string. *)

val method_of_string : string -> meth
(** Convert a string to a method. *)

val compare_method : meth -> meth -> int
(** Comparison function for [method] values *)

val status_of_code : int -> status_code
(** Generate status values from int codes. *)

val code_of_status : status_code -> int
(** Generate an int code from a status value. *)

val string_of_status : status_code -> string
(** Give a description of the given status value. *)

val reason_phrase_of_code : int -> string
(** Give a description of the given int code. *)

val is_informational : int -> bool
(** Is the given int code belong to the class of "informational" return code ? *)

val is_success : int -> bool
(** Is the given int code belong to the class of "success" return code ? *)

val is_redirection : int -> bool
(** Is the given int code belong to the class of "redirection" return code ? *)

val is_client_error : int -> bool
(** Is the given int code belong to the class of "client_error" return code ? *)

val is_server_error : int -> bool
(** Is the given int code belong to the class of "server_error" return code ? *)

val is_error : int -> bool
(** Return true for client and server error status codes. *)

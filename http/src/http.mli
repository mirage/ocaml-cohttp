module Version : sig
  type t = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ]

  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Method : sig
  type t =
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

  val compare : t -> t -> int

  val body_allowed : t -> bool
  (** [body_allowed meth] returns whether [meth] allows a payload body to be
      present per RFC7231. *)

  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Status : sig
  type informational =
    [ `Continue  (** Client should continue with request *)
    | `Switching_protocols  (** Server is switching protocols *)
    | `Processing  (** Server has received and is processing the request *)
    | `Checkpoint  (** resume aborted PUT or POST requests *) ]
  (** Informational *)

  type success =
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
  (** Success *)

  type redirection =
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
  (** Redirection *)

  type client_error =
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
    | `No_response
      (** server returns no information and closes the connection *)
    | `Retry_with  (** request should be retried after performing action *)
    | `Blocked_by_windows_parental_controls
      (** Windows Parental Controls blocking access to webpage *)
    | `Wrong_exchange_server
      (** the server cannot reach the client's mailbox *)
    | `Client_closed_request
      (** connection closed by client while HTTP server is processing *) ]
  (** Client_error *)

  type server_error =
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
  (** Server_error *)

  type standard =
    [ informational | success | redirection | client_error | server_error ]

  type t = [ `Code of int | standard ]

  val compare : t -> t -> int
  val to_string : t -> string
  val to_int : t -> int
  val of_int : int -> t
  val reason_phrase_of_code : int -> string
  val pp : Format.formatter -> t -> unit
end

module Transfer : sig
  (** Read and write the HTTP/1.1 transfer-encoding formats. Currently supported
      are [chunked] and [content-length]. *)

  (** The encoding format detected from the [transfer-encoding] and
      [content-length] headers *)
  type encoding =
    | Chunked  (** dynamic chunked encoding *)
    | Fixed of int64  (** fixed size content *)
    | Unknown  (** unknown body size, which leads to best-effort *)

  val compare_encoding : encoding -> encoding -> int

  module Private : sig
    val has_body : encoding -> [ `No | `Unknown | `Yes ]
  end
end

module Header : sig
  (** Associative list representing HTTP headers. Order of transmission is
      preserved, which implies that headers with same name are neither removed
      or concataned by default (see [clean_dup] to do that). *)
  type t
  (** The type for HTTP headers. *)

  val init : unit -> t
  (** [init ()] constructs a fresh, empty list of HTTP headers. *)

  val is_empty : t -> bool
  (** [is_empty h] tests whether HTTP headers [h] are empty or not. *)

  val of_list_rev : (string * string) list -> t

  val of_list : (string * string) list -> t
  (** [of_list l] construct a fresh headers from the content of [l] and in same
      order. [to_list] and [of_list] are defined such as
      [to_list (of_list l) = l] is true with case insensitive comparison. *)

  val to_list : t -> (string * string) list
  (** [to_list h] converts HTTP headers [h] to a list. Order and case is
      preserved.

      {e Invariant (with case insensitive comparison):}
      [to_list (of_list l) = l] *)

  val init_with : string -> string -> t
  (** [init_with k v] construct a fresh HTTP headers with a single header with
      name [k] and value [v]. *)

  val add : t -> string -> string -> t
  (** [add h k v] adds the header name [k] and its associated value [v] at the
      front of header list [h]. *)

  val add_list : t -> (string * string) list -> t
  (** [add_list h l] adds in order all header pairs contained in [l] to the
      header list [h].

      {e Invariant (with case insensitive comparison):}
      [to_list (add_list h l) = to_list h @ l] *)

  val add_multi : t -> string -> string list -> t
  (** [add_multi h k vs] add multiple header pairs with same name [h] and values
      contained in [vs] in [h]. The new headers are in the same order that in
      [vs].

      {e Invariant:} [get_multi (add_multi h k vs) k = (get_multi h k) @ vs] *)

  val add_opt : t option -> string -> string -> t
  (** [add_opt hopt k v] adds the header [(k, v)] to [h] if [hopt] is [Some h],
      or constructs a fresh header list containing this single header if [hopt]
      is [None]. *)

  val add_unless_exists : t -> string -> string -> t
  (** [add_unless_exists h k v] adds [(k, v)] to [h] unless the header name [k]
      is already present in the header. *)

  val add_opt_unless_exists : t option -> string -> string -> t
  (** [add_opt_unless_exists h k v] adds [(k, v)] to [h] if [hopt] is [Some h]
      unless the header name [k] is already present in the headers. If [h] is
      [None] then a fresh header list is constructed containing the header
      [(k, v)]. *)

  val remove : t -> string -> t
  (** [remove h k] removes every values associated to the header name [k] from
      [h]. *)

  val replace : t -> string -> string -> t
  (** [replace h k v] replaces the last added value of [k] from [h] and removed
      all other occurences of [k] if it exists. Otherwise it adds [(k, v)] to
      [h].

      {e Invariant:} [forall h, k, v. get_multi (replace h k v) = \[ v \]] *)

  val mem : t -> string -> bool
  (** [mem h k] returns [true] if the header name [k] appears in [h] and [false]
      otherwise. *)

  val compare : t -> t -> int
  (** [compare h h'] is the structural comparison of two [Header] values. *)

  val get : t -> string -> string option
  (** [get h k] returns [Some v] where [v] is the last added value associated
      with [k] in [h] if it exists and [None] otherwise *)

  val get_multi : t -> string -> string list
  (** [get_multi h k] returns a list of all values associated with [k] in [h] in
      order they appear in it. *)

  val get_multi_concat : ?list_value_only:bool -> t -> string -> string option
  (** [get_multi_concat h k] returns [Some v] if there is at least one value
      associated with [k] in [h] and [None] otherwise. [v] is the concatenation
      of all values paired with [k] in [h], separated by a comma and in order
      they appear in [h].

      The optional argument [?list_value_only] is [false] by default. If it is
      [true] and there is at least one value associated to [k], the returned
      value is the concatenated values only if [k] is a header that can have
      multiple values (like transfer-encoding or accept). Otherwise, the
      returned value is the last value paired with [k] in [h].

      {e Invariant:}
      [forall h, k not a list-value header. get_multi_concat ~list-value-only:true h k = get h k] *)

  val update : t -> string -> (string option -> string option) -> t
  (** [update h k f] returns an header list containing the same headers as [h],
      except for the header name [k]. Depending on the value of [v] where [v] is
      [f (get h k)], the header pair [(k, v)] is added, removed or updated.

      - If [v] is [None], the last occurence of [k] in [h] is removed;

      - If [v] is [Some w] then the last value paired with [k] in [h] is
        replaced by [w] if it exists. Otherwise, the pair [(k, w)] is added;

      - If [k] was already associated last in [h] to a value that is physically
        equal to [w], [h] is returned unchanged. *)

  val update_all : t -> string -> (string list -> string list) -> t
  (** [update_all h k f] returns an header list containing the same headers as
      [h], except for the header [k]. Depending on the list of values [vs] where
      [vs] is [f (get_multi h k)], the values associated to the header [k] are
      added, removed or updated.

      - If [vs] is an empty list, every occurences of the header [k] in [h] are
        removed;

      - If [vs] is a non-empty list, all values previously associated to [k] are
        removed and all values in [vs] are added with [add_multi];

      - If [k] was already associated in [h] to a list that is equal to [vs],
        [h] is returned unchanged. *)

  val iter_ord : (string -> string -> unit) -> t -> unit
  (** [iter_ord f h] applies [f] to all the headers of [h] following the header
      order. *)

  val iter : (string -> string -> unit) -> t -> unit
  (** [iter f h] applies [f] to all the headers of [h] following an unspecified
      order. This function is faster than iter_ord. *)

  val map : (string -> string -> string) -> t -> t
  val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

  val to_lines : t -> string list
  (** [to_lines h] returns header fields as a list of lines. Beware that each
      line ends with "\r\n" characters. *)

  val to_frames : t -> string list
  (** [to_frames h] returns the same as {!to_lines} but lines do not end with
      "\r\n" characters. *)

  val to_string : t -> string

  val clean_dup : t -> t
  (** [clean_dup h] cleans duplicates in [h] following
      {{:https://tools.ietf.org/html/rfc7230#section-3.2.2} RFC7230§3.2.2}; if a
      duplicated header can not have multiple values, only the last value is
      kept in place. Otherwise, the values are concatenated and place at the
      first position the header is encountered in [h].

      Already concatenated values (like [anhost.com, anotherhost.com] in the
      example below) are not affected by [clean_dup]. For example,

      {v
    transfer-encoding: gzip
    host: afirsthost.com
    connection: keep-alive
    host: anhost.com, anotherhost.com
    transfer-encoding: chunked
      v}

      becomes

      {v
    transfer-encoding: gzip, chunked
    connection: keep-alive
    host: anhost.com, anotherhost.com
      v}

      Finally, following
      {{:https://tools.ietf.org/html/rfc7230#section-3.2.2} RFC7230§3.2.2}, the
      header [Set-cookie] is treated as an exception and ignored by [clean_dup]. *)

  val get_content_range : t -> Int64.t option
  val get_connection_close : t -> bool

  val get_transfer_encoding : t -> Transfer.encoding
  (** [get_transfer_encoding h] checks the "content-length", "content-range" and
      "transfer-encoding" headers to infer the transfer encoding. Uses Unknown
      if nothing is found.*)

  val add_transfer_encoding : t -> Transfer.encoding -> t
  val connection : t -> [ `Keep_alive | `Close | `Unknown of string ] option
  val get_location : t -> string option

  val pp_hum : Format.formatter -> t -> unit
  (** Human-readable output, used by the toplevel printer *)

  module Private : sig
    val caseless_equal : string -> string -> bool
    (** [caseless_equal a b] must be equivalent to
        [String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)]. *)
  end
end

module Request : sig
  type t = {
    headers : Header.t;  (** HTTP request headers *)
    meth : Method.t;  (** HTTP request method *)
    scheme : string option;  (** URI scheme (http or https) *)
    resource : string;  (** Request path and query *)
    version : Version.t;  (** HTTP version, usually 1.1 *)
    encoding : Transfer.encoding;
        [@deprecated "this field will be removed in the future"]
  }

  val has_body : t -> [ `No | `Unknown | `Yes ]
  val headers : t -> Header.t
  val meth : t -> Method.t
  val scheme : t -> string option
  val resource : t -> string
  val version : t -> Version.t
  val encoding : t -> Transfer.encoding
  val compare : t -> t -> int

  val is_keep_alive : t -> bool
  (** Return true whether the connection should be reused *)

  val requires_content_length : t -> bool
  (** [requires_content_length t] is [true] if [t.meth] is one of
      [`POST, `PUT or `PATCH]. Otherwise it is [false].

      A [true] value indicates that a request must include a "Content-Length"
      header.

      See https://www.rfc-editor.org/rfc/rfc7230#section-3.3.2 *)

  val content_length : t -> int option
  (** [content_length t] is [Some x] if the "Content-Length" header in [t]
      exists and its value [x] is a non negative integer, [x>=0]

      It is [None] if [requires_content_length t = false] or the value encoded
      in "Content-Length" is not a valid integer value, i.e [>= 0].

      See https://www.rfc-editor.org/rfc/rfc7230#section-3.3.2 *)

  val supports_chunked_trailers : t -> bool
  (** [supports_chunked_trailers t] is [true] if [t] contains HTTP header "TE:
      trailers". Otherwise it is [false]. *)

  val add_te_trailers : t -> t
  (** [add_te_trailers t] adds HTTP headers, 'TE' and 'Connection' to indicate
      that a user-agent can handle HTTP chunked trailers headers. *)

  val make :
    ?meth:Method.t ->
    ?version:Version.t ->
    ?headers:Header.t ->
    ?scheme:string ->
    string ->
    t
  (** [make resource] is a value of {!type:t}. The default values for the
      response, if not specified, are as follows: [meth] is [`GET], [version] is
      [`HTTP_1_1], [headers] is [Header.empty] and [scheme] is [None]. The
      request encoding value is determined via the
      [Header.get_transfer_encoding] function.*)

  val pp : Format.formatter -> t -> unit
end

module Response : sig
  type t = {
    encoding : Transfer.encoding;
        [@deprecated "this field will be removed in the future"]
    headers : Header.t;  (** response HTTP headers *)
    version : Version.t;  (** (** HTTP version, usually 1.1 *) *)
    status : Status.t;  (** HTTP status code of the response *)
    flush : bool;
        [@deprecated
          "this field will be removed in the future. Provide flush in the \
           [respond_*] function instead."]
  }

  val encoding : t -> Transfer.encoding
  val headers : t -> Header.t
  val version : t -> Version.t
  val status : t -> Status.t

  val flush :
    (t -> bool
    [@deprecated
      "this field will be removed in the future. Provide flush in the \
       [respond_*] function instead."])

  val compare : t -> t -> int

  val is_keep_alive : t -> bool
  (** Return true whether the connection should be reused *)

  val requires_content_length : ?request_meth:Method.t -> t -> bool
  (** [requires_content_length ~request_meth t] is [true] if a combination of
      [t] and [request_meth] indicates that a response message must include
      "Content-Length" header. However, please note exceptions to this:

      - Response with status code of [304] may or may not include the header.
      - Response to request with method [HEAD] may or may not include the
        header.

      https://www.rfc-editor.org/rfc/rfc7230#section-3.3.2 *)

  val content_length : t -> int option
  (** [content_length t] is [Some x] if the "Content-Length" header in [t]
      exists and its value [x] is a non negative integer, [x>=0]

      It is [None] if [requires_content_length t = false] or the value encoded
      in "Content-Length" is not a valid integer value, i.e [>= 0].

      See https://www.rfc-editor.org/rfc/rfc7230#section-3.3.2 *)

  val make :
    ?version:Version.t ->
    ?status:Status.t ->
    ?flush:bool ->
    ?headers:Header.t ->
    unit ->
    t
  (** [make ()] is a value of {!type:t}. The default values for the request, if
      not specified, are: [status] is [`Ok], [version] is [`HTTP_1_1], [flush]
      is [false] and [headers] is [Header.empty]. The request encoding value is
      determined via the [Header.get_transfer_encoding] function. *)

  val pp : Format.formatter -> t -> unit
end

module Private : sig
  module Parser : sig
    (** Attempts to parse a buffer into a HTTP request. If successful, it
        returns the parsed request and an offset value that indicates the
        starting point of unconsumed content left in the buffer. *)

    type error = Partial | Msg of string

    val parse_request :
      ?pos:int -> ?len:int -> string -> (Request.t * int, error) result

    val parse_chunk_length :
      ?pos:int -> ?len:int -> string -> (int64 * int, error) result
  end
end

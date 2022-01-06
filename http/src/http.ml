module Transfer = struct
  type encoding = Chunked | Fixed of int64 | Unknown

  let compare_encoding (x : encoding) (y : encoding) = Stdlib.compare x y
end

module Header = struct
  let caseless_equal a b =
    if a == b then true
    else
      let len = String.length a in
      len = String.length b
      &&
      let stop = ref false in
      let idx = ref 0 in
      while (not !stop) && !idx < len do
        let c1 = String.unsafe_get a !idx in
        let c2 = String.unsafe_get b !idx in
        if Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then stop := true;
        incr idx
      done;
      not !stop

  type t = (string * string) list

  let pp_hum =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    let pp_kv fmt (k, v) = Format.fprintf fmt "@[%s@ =@ %S@]" k v in
    fun fmt t ->
      Format.fprintf fmt "Header@ {@ @[%a@]@ }"
        (Format.pp_print_list ~pp_sep pp_kv)
        t

  let empty = []
  let compare = Stdlib.compare
  let init () = []
  let is_empty = function [] -> true | _ -> false
  let init_with k v = [ (k, v) ]

  let mem h k =
    let rec loop = function
      | [] -> false
      | (k', _) :: h' -> if caseless_equal k k' then true else loop h'
    in
    loop h

  let add h k v : t = (k, v) :: h
  let add_list h l = List.fold_left (fun h (k, v) -> add h k v) h l
  let add_multi h k l = List.fold_left (fun h v -> add h k v) h l

  let add_opt h_opt k v =
    match h_opt with None -> init_with k v | Some h -> add h k v

  let add_unless_exists h k v = if mem h k then h else add h k v

  let add_opt_unless_exists h k v =
    match h with None -> init_with k v | Some h -> add_unless_exists h k v

  let get h k =
    let rec loop h =
      match h with
      | [] -> None
      | (k', v) :: h' -> if caseless_equal k k' then Some v else loop h'
    in
    loop h

  let get_multi (h : t) (k : string) =
    let rec loop h acc =
      match h with
      | [] -> acc
      | (k', v) :: h' ->
          if caseless_equal k k' then loop h' (v :: acc) else loop h' acc
    in
    loop h []

  let remove h k =
    let rec loop seen = function
      | [] -> if seen then [] else raise_notrace Not_found
      | (k', _) :: h when caseless_equal k k' -> loop true h
      | x :: h -> x :: loop seen h
    in
    try loop false h with Not_found -> h

  let remove_last h k =
    let rec loop seen = function
      | [] -> raise_notrace Not_found
      | (k', _) :: h when caseless_equal k k' -> h
      | x :: h -> x :: loop seen h
    in
    try loop false h with Not_found -> h

  let replace_ last h k v =
    let rec loop seen = function
      | [] -> if seen then [] else raise_notrace Not_found
      | (k'', _) :: h when caseless_equal k k'' ->
          if last then (k'', v) :: h
          else if not seen then (k, v) :: loop true h
          else loop seen h
      | x :: h -> x :: loop seen h
    in
    try loop false h with Not_found -> add h k v

  let replace = replace_ false

  let update h k f =
    let vorig = get h k in
    match (f vorig, vorig) with
    | None, None -> h
    | None, _ -> remove_last h k
    | Some s, Some s' when s == s' -> h
    | Some s, _ -> replace_ true h k s

  let update_all h k f =
    let vorig = get_multi h k in
    match (f vorig, vorig) with
    | [], [] -> h
    | [], _ -> remove h k
    | xs, xs' when xs = xs' -> h
    | xs, _ ->
        let h = remove h k in
        add_multi h k xs

  let map (f : string -> string -> string) (h : t) : t =
    List.map
      (fun (k, v) ->
        let vs' = f k v in
        (k, vs'))
      h

  let iter (f : string -> string -> unit) (h : t) : unit =
    List.iter (fun (k, v) -> f k v) h

  let fold (f : string -> string -> 'a -> 'a) (h : t) (init : 'a) : 'a =
    List.fold_left (fun acc (k, v) -> f k v acc) init h

  let of_list_rev h = h
  let of_list h = List.rev h
  let to_list h = List.rev h

  let to_lines (h : t) =
    let header_line k v = Printf.sprintf "%s: %s\r\n" k v in
    List.fold_left (fun acc (k, v) -> header_line k v :: acc) [] h

  let to_frames h =
    let to_frame k v = Printf.sprintf "%s: %s" k v in
    List.fold_left (fun acc (k, v) -> to_frame k v :: acc) [] h

  let to_string h =
    let b = Buffer.create 128 in
    to_list h
    |> List.iter (fun (k, v) ->
           Buffer.add_string b k;
           Buffer.add_string b ": ";
           Buffer.add_string b v;
           Buffer.add_string b "\r\n");
    Buffer.add_string b "\r\n";
    Buffer.contents b

  let headers_with_list_values =
    [|
      "accept";
      "accept-charset";
      "accept-encoding";
      "accept-language";
      "accept-ranges";
      "allow";
      "cache-control";
      "connection";
      "content-encoding";
      "content-language";
      "expect";
      "if-match";
      "if-none-match";
      "link";
      "pragma";
      "proxy-authenticate";
      "te";
      "trailer";
      "transfer-encoding";
      "upgrade";
      "vary";
      "via";
      "warning";
      "www-authenticate";
    |]

  let is_header_with_list_value =
    let tbl = Hashtbl.create (Array.length headers_with_list_values) in
    headers_with_list_values |> Array.iter (fun h -> Hashtbl.add tbl h ());
    fun h -> Hashtbl.mem tbl h

  let is_set_cookie k = caseless_equal k "set-cookie"

  (* set-cookie is an exception according to
     {{:https://tools.ietf.org/html/rfc7230#section-3.2.2}
      RFC7230§3.2.2} and can appear multiple times in a response message.
  *)
  let clean_dup (h : t) : t =
    let add h k v =
      if is_set_cookie k then (k, v) :: h
      else
        let to_add = ref false in
        let rec loop = function
          | [] ->
              to_add := true;
              []
          | (k', v') :: hs ->
              if caseless_equal k k' then
                if is_header_with_list_value k then (k, v' ^ "," ^ v) :: hs
                else (
                  to_add := true;
                  hs)
              else (k', v') :: loop hs
        in
        let h = loop h in
        if !to_add then (k, v) :: h else h
    in
    List.rev h |> List.fold_left (fun acc (k, v) -> add acc k v) []

  let get_multi_concat ?(list_value_only = false) h k : string option =
    if (not list_value_only) || is_header_with_list_value k then
      let vs = get_multi h k in
      match vs with [] -> None | _ -> Some (String.concat "," vs)
    else get h k

  let parse_content_range s =
    try
      let start, fini, total =
        Scanf.sscanf s "bytes %Ld-%Ld/%Ld" (fun start fini total ->
            (start, fini, total))
      in
      Some (start, fini, total)
    with Scanf.Scan_failure _ -> None

  (* If we see a "Content-Range" header, than we should limit the
     number of bytes we attempt to read *)
  let get_content_range headers =
    match get headers "content-length" with
    | Some clen -> Int64.of_string_opt clen
    | None -> (
        match get headers "content-range" with
        | Some range_s -> (
            match parse_content_range range_s with
            | Some (start, fini, total) ->
                (* some sanity checking before we act on these values *)
                if fini < total && start <= total && 0L <= start && 0L <= total
                then
                  let num_bytes_to_read = Int64.add (Int64.sub fini start) 1L in
                  Some num_bytes_to_read
                else None
            | None -> None)
        | None -> None)

  let get_connection_close headers =
    match get headers "connection" with Some "close" -> true | _ -> false

  (* Parse the transfer-encoding and content-length headers to
   * determine how to decode a body *)
  let get_transfer_encoding headers =
    (* It should actually be [get] as the interresting value is actually the last.*)
    match
      get_multi_concat ~list_value_only:true headers "transfer-encoding"
    with
    | Some "chunked" -> Transfer.Chunked
    | Some _ | None -> (
        match get_content_range headers with
        | Some len -> Transfer.Fixed len
        | None -> Transfer.Unknown)

  let add_transfer_encoding headers enc =
    let open Transfer in
    (* Only add a header if one doesnt already exist, e.g. from the app *)
    match (get_transfer_encoding headers, enc) with
    | Fixed _, _ (* App has supplied a content length, so use that *)
    | Chunked, _ ->
        headers (* TODO: this is a protocol violation *)
    | Unknown, Chunked -> add headers "transfer-encoding" "chunked"
    | Unknown, Fixed len -> add headers "content-length" (Int64.to_string len)
    | Unknown, Unknown -> headers

  let get_location headers =
    match get headers "location" with None -> None | Some u -> Some u

  let connection h =
    match get h "connection" with
    | Some v when v = "keep-alive" -> Some `Keep_alive
    | Some v when v = "close" -> Some `Close
    | Some x -> Some (`Unknown x)
    | _ -> None
end

module Status = struct
  type informational =
    [ `Continue | `Switching_protocols | `Processing | `Checkpoint ]

  type success =
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

  type redirection =
    [ `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Switch_proxy
    | `Temporary_redirect
    | `Permanent_redirect ]

  type client_error =
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

  type server_error =
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

  type standard =
    [ informational | success | redirection | client_error | server_error ]

  type t = [ `Code of int | standard ]

  let of_int : int -> t = function
    | 100 -> `Continue
    | 101 -> `Switching_protocols
    | 102 -> `Processing
    | 103 -> `Checkpoint
    | 200 -> `OK
    | 201 -> `Created
    | 202 -> `Accepted
    | 203 -> `Non_authoritative_information
    | 204 -> `No_content
    | 205 -> `Reset_content
    | 206 -> `Partial_content
    | 207 -> `Multi_status
    | 208 -> `Already_reported
    | 226 -> `Im_used
    | 300 -> `Multiple_choices
    | 301 -> `Moved_permanently
    | 302 -> `Found
    | 303 -> `See_other
    | 304 -> `Not_modified
    | 305 -> `Use_proxy
    | 306 -> `Switch_proxy
    | 307 -> `Temporary_redirect
    | 308 -> `Permanent_redirect
    | 400 -> `Bad_request
    | 401 -> `Unauthorized
    | 402 -> `Payment_required
    | 403 -> `Forbidden
    | 404 -> `Not_found
    | 405 -> `Method_not_allowed
    | 406 -> `Not_acceptable
    | 407 -> `Proxy_authentication_required
    | 408 -> `Request_timeout
    | 409 -> `Conflict
    | 410 -> `Gone
    | 411 -> `Length_required
    | 412 -> `Precondition_failed
    | 413 -> `Request_entity_too_large
    | 414 -> `Request_uri_too_long
    | 415 -> `Unsupported_media_type
    | 416 -> `Requested_range_not_satisfiable
    | 417 -> `Expectation_failed
    | 418 -> `I_m_a_teapot
    | 420 -> `Enhance_your_calm
    | 422 -> `Unprocessable_entity
    | 423 -> `Locked
    | 424 -> `Failed_dependency
    | 426 -> `Upgrade_required
    | 428 -> `Precondition_required
    | 429 -> `Too_many_requests
    | 431 -> `Request_header_fields_too_large
    | 444 -> `No_response
    | 449 -> `Retry_with
    | 450 -> `Blocked_by_windows_parental_controls
    | 451 -> `Wrong_exchange_server
    | 499 -> `Client_closed_request
    | 500 -> `Internal_server_error
    | 501 -> `Not_implemented
    | 502 -> `Bad_gateway
    | 503 -> `Service_unavailable
    | 504 -> `Gateway_timeout
    | 505 -> `Http_version_not_supported
    | 506 -> `Variant_also_negotiates
    | 507 -> `Insufficient_storage
    | 508 -> `Loop_detected
    | 509 -> `Bandwidth_limit_exceeded
    | 510 -> `Not_extended
    | 511 -> `Network_authentication_required
    | 598 -> `Network_read_timeout_error
    | 599 -> `Network_connect_timeout_error
    | cod -> `Code cod

  let to_int : t -> int = function
    | `Continue -> 100
    | `Switching_protocols -> 101
    | `Processing -> 102
    | `Checkpoint -> 103
    | `OK -> 200
    | `Created -> 201
    | `Accepted -> 202
    | `Non_authoritative_information -> 203
    | `No_content -> 204
    | `Reset_content -> 205
    | `Partial_content -> 206
    | `Multi_status -> 207
    | `Already_reported -> 208
    | `Im_used -> 226
    | `Multiple_choices -> 300
    | `Moved_permanently -> 301
    | `Found -> 302
    | `See_other -> 303
    | `Not_modified -> 304
    | `Use_proxy -> 305
    | `Switch_proxy -> 306
    | `Temporary_redirect -> 307
    | `Permanent_redirect -> 308
    | `Bad_request -> 400
    | `Unauthorized -> 401
    | `Payment_required -> 402
    | `Forbidden -> 403
    | `Not_found -> 404
    | `Method_not_allowed -> 405
    | `Not_acceptable -> 406
    | `Proxy_authentication_required -> 407
    | `Request_timeout -> 408
    | `Conflict -> 409
    | `Gone -> 410
    | `Length_required -> 411
    | `Precondition_failed -> 412
    | `Request_entity_too_large -> 413
    | `Request_uri_too_long -> 414
    | `Unsupported_media_type -> 415
    | `Requested_range_not_satisfiable -> 416
    | `Expectation_failed -> 417
    | `I_m_a_teapot -> 418
    | `Enhance_your_calm -> 420
    | `Unprocessable_entity -> 422
    | `Locked -> 423
    | `Failed_dependency -> 424
    | `Upgrade_required -> 426
    | `Precondition_required -> 428
    | `Too_many_requests -> 429
    | `Request_header_fields_too_large -> 431
    | `No_response -> 444
    | `Retry_with -> 449
    | `Blocked_by_windows_parental_controls -> 450
    | `Wrong_exchange_server -> 451
    | `Client_closed_request -> 499
    | `Internal_server_error -> 500
    | `Not_implemented -> 501
    | `Bad_gateway -> 502
    | `Service_unavailable -> 503
    | `Gateway_timeout -> 504
    | `Http_version_not_supported -> 505
    | `Variant_also_negotiates -> 506
    | `Insufficient_storage -> 507
    | `Loop_detected -> 508
    | `Bandwidth_limit_exceeded -> 509
    | `Not_extended -> 510
    | `Network_authentication_required -> 511
    | `Network_read_timeout_error -> 598
    | `Network_connect_timeout_error -> 599
    | `Code code -> code

  let reason_phrase_of_code : int -> string = function
    | 100 -> "Continue"
    | 101 -> "Switching Protocols"
    | 102 -> "Processing (WebDAV) (RFC 2518)"
    | 103 -> "Checkpoint"
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 203 -> "Non-Authoritative Information (since HTTP/1.1)"
    | 204 -> "No Content"
    | 205 -> "Reset Content"
    | 206 -> "Partial Content"
    | 207 -> "Multi-Status (WebDAV) (RFC 4918)"
    | 208 -> "Already Reported (WebDAV) (RFC 5842)"
    | 226 -> "IM Used (RFC 3229)"
    | 300 -> "Multiple Choices"
    | 301 -> "Moved Permanently"
    | 302 -> "Found"
    | 303 -> "See Other"
    | 304 -> "Not Modified"
    | 305 -> "Use Proxy (since HTTP/1.1)"
    | 306 -> "Switch Proxy"
    | 307 -> "Temporary Redirect (since HTTP/1.1)"
    | 308 -> "Permanent Redirect"
    | 400 -> "Bad Request"
    | 401 -> "Unauthorized"
    | 402 -> "Payment Required"
    | 403 -> "Forbidden"
    | 404 -> "Not Found"
    | 405 -> "Method Not Allowed"
    | 406 -> "Not Acceptable"
    | 407 -> "Proxy Authentication Required"
    | 408 -> "Request Timeout"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 411 -> "Length Required"
    | 412 -> "Precondition Failed"
    | 413 -> "Request Entity Too Large"
    | 414 -> "Request-URI Too Long"
    | 415 -> "Unsupported Media Type"
    | 416 -> "Requested Range Not Satisfiable"
    | 417 -> "Expectation Failed"
    | 418 -> "I'm a teapot (RFC 2324)"
    | 420 -> "Enhance Your Calm"
    | 422 -> "Unprocessable Entity (WebDAV) (RFC 4918)"
    | 423 -> "Locked (WebDAV) (RFC 4918)"
    | 424 -> "Failed Dependency (WebDAV) (RFC 4918)"
    | 426 -> "Upgrade Required (RFC 2817)"
    | 428 -> "Precondition Required"
    | 429 -> "Too Many Requests"
    | 431 -> "Request Header Fields Too Large"
    | 444 -> "No Response"
    | 449 -> "Retry With"
    | 450 -> "Blocked by Windows Parental Controls"
    | 451 -> "Wrong Exchange server"
    | 499 -> "Client Closed Request"
    | 500 -> "Internal Server Error"
    | 501 -> "Not Implemented"
    | 502 -> "Bad Gateway"
    | 503 -> "Service Unavailable"
    | 504 -> "Gateway Timeout"
    | 505 -> "HTTP Version Not Supported"
    | 506 -> "Variant Also Negotiates (RFC 2295)"
    | 507 -> "Insufficient Storage (WebDAV) (RFC 4918)"
    | 508 -> "Loop Detected (WebDAV) (RFC 5842)"
    | 509 -> "Bandwidth Limit Exceeded (Apache bw/limited extension)"
    | 510 -> "Not Extended (RFC 2774)"
    | 511 -> "Network Authentication Required"
    | 598 -> "Network read timeout error"
    | 599 -> "Network connect timeout error"
    | cod -> string_of_int cod

  let compare = Stdlib.compare

  let to_string : t -> string = function
    | `Code code ->
        let code = string_of_int code in
        code ^ " Status " ^ code
    | #standard as s ->
        let code = to_int s in
        string_of_int code ^ " " ^ reason_phrase_of_code code
end

module Method = struct
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

  let to_string : t -> string = function
    | `GET -> "GET"
    | `POST -> "POST"
    | `HEAD -> "HEAD"
    | `DELETE -> "DELETE"
    | `PATCH -> "PATCH"
    | `PUT -> "PUT"
    | `OPTIONS -> "OPTIONS"
    | `TRACE -> "TRACE"
    | `CONNECT -> "CONNECT"
    | `Other s -> s

  let of_string : string -> t = function
    | "GET" -> `GET
    | "POST" -> `POST
    | "HEAD" -> `HEAD
    | "DELETE" -> `DELETE
    | "PATCH" -> `PATCH
    | "PUT" -> `PUT
    | "OPTIONS" -> `OPTIONS
    | "TRACE" -> `TRACE
    | "CONNECT" -> `CONNECT
    | s -> `Other s

  let compare (a : t) (b : t) = Stdlib.compare a b
end

module Version = struct
  type t = [ `HTTP_1_0 | `HTTP_1_1 | `Other of string ]

  let to_string = function
    | `HTTP_1_0 -> "HTTP/1.0"
    | `HTTP_1_1 -> "HTTP/1.1"
    | `Other s -> s

  let of_string = function
    | "HTTP/1.0" -> `HTTP_1_0
    | "HTTP/1.1" -> `HTTP_1_1
    | s -> `Other s

  let compare (a : t) (b : t) = Stdlib.compare a b
end

module Request = struct
  type t = {
    headers : Header.t;  (** HTTP request headers *)
    meth : Method.t;  (** HTTP request method *)
    scheme : string option;  (** URI scheme (http or https) *)
    resource : string;  (** Request path and query *)
    version : Version.t;  (** HTTP version, usually 1.1 *)
    encoding : Transfer.encoding;
  }

  let headers t = t.headers
  let meth t = t.meth
  let scheme t = t.scheme
  let resource t = t.resource
  let version t = t.version
  let encoding t = t.encoding

  let compare { headers; meth; scheme; resource; version; encoding } y =
    match Header.compare headers y.headers with
    | 0 -> (
        match Method.compare meth y.meth with
        | 0 -> (
            match Option.compare String.compare scheme y.scheme with
            | 0 -> (
                match String.compare resource y.resource with
                | 0 -> (
                    match Version.compare version y.version with
                    | 0 -> Transfer.compare_encoding encoding y.encoding
                    | i -> i)
                | i -> i)
            | i -> i)
        | i -> i)
    | i -> i

  let is_keep_alive { version; headers; _ } =
    not
      (version = `HTTP_1_0
      || match Header.connection headers with Some `Close -> true | _ -> false)
end

module Response = struct
  type t = {
    encoding : Transfer.encoding;
    headers : Header.t;  (** response HTTP headers *)
    version : Version.t;  (** (** HTTP version, usually 1.1 *) *)
    status : Status.t;  (** HTTP status code of the response *)
    flush : bool;
  }

  let compare { headers; flush; version; encoding; status } y =
    match Header.compare headers y.headers with
    | 0 -> (
        match Bool.compare flush y.flush with
        | 0 -> (
            match Stdlib.compare status y.status with
            | 0 -> (
                match Version.compare version y.version with
                | 0 -> Transfer.compare_encoding encoding y.encoding
                | i -> i)
            | i -> i)
        | i -> i)
    | i -> i

  let make ?(version = `HTTP_1_1) ?(status = `OK) ?(flush = false)
      ?(encoding = Transfer.Chunked) ?(headers = Header.empty) () =
    let encoding =
      match Header.get_transfer_encoding headers with
      | Transfer.(Chunked | Fixed _) as enc -> enc
      | Unknown -> encoding
    in
    { encoding; headers; version; flush; status }

  let headers t = t.headers
  let encoding t = t.encoding
  let version t = t.version
  let status t = t.status
  let flush t = t.flush
end

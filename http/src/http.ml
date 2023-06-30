module Transfer = struct
  type encoding = Chunked | Fixed of int64 | Unknown

  let compare_encoding (x : encoding) (y : encoding) = Stdlib.compare x y

  let has_body = function
    | Fixed 0L -> `No
    | Chunked | Fixed _ -> `Yes
    | Unknown -> `Unknown

  module Private = struct
    let has_body = has_body
  end
end

module Header = struct
  external string_unsafe_get64 : string -> int -> int64 = "%caml_string_get64u"

  (* [caseless_equal a b] must be equivalent to
     [String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)]. *)
  let caseless_equal a b =
    if a == b then true
    else
      let len = String.length a in
      len = String.length b
      (* Note: at this point we konw that [a] and [b] have the same length. *)
      &&
      (* [word_loop a b i len] compares strings [a] and [b] from
         offsets [i] (included) to [len] (excluded), one word at a time.
         [i] is a world-aligned index into the strings.
      *)
      let rec word_loop a b i len =
        if i = len then true
        else
          let i' = i + 8 in
          (* If [i' > len], what remains to be compared is strictly
             less than a word long, use byte-per-byte comparison. *)
          if i' > len then byte_loop a b i len
          else if string_unsafe_get64 a i = string_unsafe_get64 b i then
            word_loop a b i' len
          else
            (* If the words at [i] differ, it may due to a case
               difference; we check the individual bytes of this
               work, and then we continue checking the other
               words. *)
            byte_loop a b i i' && word_loop a b i' len
      (* [byte_loop a b i len] compares the strings [a] and [b] from
         offsets [i] (included) to [len] (excluded), one byte at
         a time.

         This function assumes that [i < len] holds -- its only called
         by [word_loop] when this is known to hold. *)
      and byte_loop a b i len =
        let c1 = String.unsafe_get a i in
        let c2 = String.unsafe_get b i in
        Char.lowercase_ascii c1 = Char.lowercase_ascii c2
        &&
        let i' = i + 1 in
        i' = len || byte_loop a b i' len
      in
      word_loop a b 0 len

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

  let first t = match t with [] -> None | (k, v) :: _ -> Some (k, v)

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

  let move_to_front t hdr_name =
    match t with
    | (k, _) :: _ when caseless_equal k hdr_name -> t
    | _ -> (
        match get t hdr_name with
        | Some v ->
            let headers = remove t hdr_name in
            add headers hdr_name v
        | None -> t)

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
    let b = Buffer.create 128 in
    let header_line k v =
      Buffer.clear b;
      Buffer.add_string b k;
      Buffer.add_string b ": ";
      Buffer.add_string b v;
      Buffer.add_string b "\r\n";
      Buffer.contents b
    in
    List.fold_left (fun acc (k, v) -> header_line k v :: acc) [] h

  let to_frames h =
    let to_frame k v = String.concat ": " [ k; v ] in
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

  module Private = struct
    let caseless_equal = caseless_equal
    let first = first
    let move_to_front = move_to_front
  end
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

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)
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

  (* Defined for method types in RFC7231 *)
  let body_allowed = function
    | `GET | `HEAD | `CONNECT | `TRACE -> false
    | `DELETE | `POST | `PUT | `PATCH | `OPTIONS | `Other _ -> true

  let compare (a : t) (b : t) = Stdlib.compare a b
  let pp fmt t = Format.fprintf fmt "%s" (to_string t)
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
  let pp fmt t = Format.fprintf fmt "%s" (to_string t)
end

let is_keep_alive version headers =
  match Header.connection headers with
  | Some `Close -> false
  | Some `Keep_alive -> true
  | Some (`Unknown _) -> false
  | None -> Version.compare version `HTTP_1_1 = 0

let pp_field field_name pp_v fmt v =
  Format.fprintf fmt "@[<1>%s:@ %a@]" field_name pp_v v

let content_length requires_content_length headers =
  let ( let* ) o f = Option.bind o f in
  if requires_content_length then
    let* x = Header.get headers "Content-Length" in
    let* x = int_of_string_opt x in
    if x >= 0 then Some x else None
  else None

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

  let is_keep_alive { version; headers; _ } = is_keep_alive version headers

  let requires_content_length t =
    match t.meth with `POST | `PUT | `PATCH -> true | _ -> false

  let content_length t = content_length (requires_content_length t) t.headers

  let supports_chunked_trailers t =
    Header.get_multi t.headers "TE" |> List.mem "trailers"

  let add_te_trailers t =
    let headers = Header.add t.headers "TE" "trailers" in
    let headers = Header.add headers "Connection" "TE" in
    { t with headers }

  (* Defined for method types in RFC7231 *)
  let has_body req =
    if Method.body_allowed req.meth then Transfer.has_body req.encoding else `No

  let make ?(meth = `GET) ?(version = `HTTP_1_1) ?(headers = Header.empty)
      ?scheme resource =
    let encoding = Header.get_transfer_encoding headers in
    { headers; meth; scheme; resource; version; encoding }

  let pp fmt t =
    let open Format in
    pp_open_vbox fmt 0;
    pp_field "meth" Method.pp fmt t.meth;
    pp_print_cut fmt ();
    pp_field "resource" pp_print_string fmt t.resource;
    pp_print_cut fmt ();
    pp_field "version" Version.pp fmt t.version;
    pp_print_cut fmt ();
    pp_field "headers" Header.pp_hum fmt t.headers;
    pp_close_box fmt ()
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
      ?(headers = Header.empty) () =
    let encoding = Header.get_transfer_encoding headers in
    { encoding; headers; version; flush; status }

  let headers t = t.headers
  let encoding t = t.encoding
  let version t = t.version
  let status t = t.status
  let flush t = t.flush
  let is_keep_alive { version; headers; _ } = is_keep_alive version headers

  let requires_content_length ?request_meth t =
    match (Status.to_int t.status, request_meth) with
    | 204, _ -> false
    | s, _ when s >= 100 && s < 200 -> false
    | s, Some meth when s >= 200 && s < 300 && meth = `CONNECT -> false
    | _, _ -> not (Header.mem t.headers "Transfer-Encoding")

  let content_length t = content_length (requires_content_length t) t.headers

  let pp fmt t =
    let open Format in
    pp_open_vbox fmt 0;
    pp_field "version" Version.pp fmt t.version;
    pp_print_cut fmt ();
    pp_field "status" Status.pp fmt t.status;
    pp_print_cut fmt ();
    pp_field "headers" Header.pp_hum fmt t.headers;
    pp_close_box fmt ()
end

module Parser = struct
  let[@inline always] is_tchar = function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false

  module Source = struct
    type t = { buffer : string; mutable pos : int; upper_bound : int }

    let of_bytes ~pos ?len buffer =
      let buf_len = String.length buffer in
      if pos < 0 || pos > buf_len then
        invalid_arg
          (Printf.sprintf
             "Http_parser.Source.of_bigstring: Invalid offset %d. Buffer \
              length: %d"
             pos buf_len);
      let len = Option.value len ~default:(buf_len - pos) in
      if len < 0 || pos + len > buf_len then
        invalid_arg
          (Printf.sprintf
             "Http_parser.Source.of_bigstring: Invalid len %d. offset: %d, \
              buffer_length: %d, requested_length: %d"
             len pos buf_len (pos + len));
      { buffer; pos; upper_bound = pos + len }

    let[@inline always] get_unsafe t idx =
      String.unsafe_get t.buffer (t.pos + idx)

    let[@inline always] get t idx =
      if idx < 0 || t.pos + idx >= t.upper_bound then
        invalid_arg "Http_parser.Source.get: Index out of bounds";
      String.unsafe_get t.buffer (t.pos + idx)

    let[@inline always] advance_unsafe t count = t.pos <- t.pos + count

    let[@inline always] advance t count =
      if count < 0 || t.pos + count > t.upper_bound then
        invalid_arg
          (Printf.sprintf
             "Http_parser.Source.advance: Index out of bounds. Requested \
              count: %d"
             count);
      t.pos <- t.pos + count

    let[@inline always] length t = t.upper_bound - t.pos
    let[@inline always] is_empty t = t.pos = t.upper_bound

    let[@inline always] to_string t ~pos ~len =
      if
        pos < 0
        || t.pos + pos >= t.upper_bound
        || len < 0
        || t.pos + pos + len > t.upper_bound
      then
        invalid_arg
          (Format.asprintf
             "Http_parser.Source.substring: Index out of bounds., Requested \
              off: %d, len: %d"
             pos len);
      String.sub t.buffer (t.pos + pos) len

    let[@inline always] is_space = function
      | ' ' | '\012' | '\n' | '\r' | '\t' -> true
      | _ -> false

    let[@inline always] to_string_trim t ~pos ~len =
      if
        pos < 0
        || t.pos + pos >= t.upper_bound
        || len < 0
        || t.pos + pos + len > t.upper_bound
      then
        invalid_arg
          (Format.asprintf
             "Http_parser.Source.substring: Index out of bounds., Requested \
              off: %d, len: %d"
             pos len);
      let last = ref (t.pos + len - 1) in
      let pos = ref (t.pos + pos) in
      while is_space (String.unsafe_get t.buffer !pos) do
        incr pos
      done;
      while is_space (String.unsafe_get t.buffer !last) do
        decr last
      done;
      let len = !last - !pos + 1 in
      String.sub t.buffer !pos len

    let rec index_rec t ch idx len =
      if idx = len then -1
      else if String.unsafe_get t.buffer (t.pos + idx) = ch then idx
      else index_rec t ch (idx + 1) len

    let index t ch = index_rec t ch 0 (length t)

    let for_all_is_tchar t ~pos ~len =
      if
        pos < 0
        || t.pos + pos >= t.upper_bound
        || len < 0
        || t.pos + pos + len > t.upper_bound
      then
        invalid_arg
          (Format.asprintf
             "Http_parser.Source.substring: Index out of bounds. Requested \
              off: %d, len: %d"
             pos len);
      let pos = ref (t.pos + pos) in
      let len = t.pos + len in
      while !pos < len && is_tchar (String.unsafe_get t.buffer !pos) do
        incr pos
      done;
      !pos = len

    let unsafe_memcmp t pos str =
      let rec loop t pos str len =
        if pos = len then true
        else
          Char.equal (get_unsafe t pos) (String.unsafe_get str pos)
          && loop t (pos + 1) str len
      in
      loop t pos str (String.length str)
  end

  exception Msg of string
  exception Partial

  let string str source =
    let len = String.length str in
    if Source.length source < len then raise_notrace Partial
    else if Source.unsafe_memcmp source 0 str then Source.advance source len
    else raise_notrace (Msg (Printf.sprintf "Could not match: %S" str))

  let any_char source =
    if Source.is_empty source then raise_notrace Partial
    else
      let c = Source.get_unsafe source 0 in
      Source.advance_unsafe source 1;
      c

  let eol = string "\r\n"

  (* token = 1*tchar tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^"
     / "_" / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters *)

  let token source =
    let pos = Source.index source ' ' in
    if pos = -1 then raise_notrace Partial
    else
      let res = Source.to_string source ~pos:0 ~len:pos in
      Source.advance source (pos + 1);
      res

  let meth source =
    let token = token source in
    Method.of_string token

  let version_source source =
    string "HTTP/1." source;
    any_char source

  let version source =
    let ch = version_source source in
    match ch with
    | '1' -> `HTTP_1_1
    | '0' -> `HTTP_1_0
    | _ -> raise_notrace (Msg "Invalid http version")

  let header source =
    let pos = Source.index source ':' in
    if pos = -1 then raise_notrace Partial
    else if pos = 0 then raise_notrace (Msg "Invalid header: Empty header key")
    else if Source.for_all_is_tchar source ~pos:0 ~len:pos then (
      let key = Source.to_string source ~pos:0 ~len:pos in
      Source.advance_unsafe source (pos + 1);
      while
        (not (Source.is_empty source)) && Source.get_unsafe source 0 = ' '
      do
        Source.advance_unsafe source 1
      done;
      let pos = Source.index source '\r' in
      if pos = -1 then raise_notrace Partial
      else
        let v = Source.to_string_trim source ~pos:0 ~len:pos in
        Source.advance_unsafe source pos;
        (key, v))
    else raise_notrace (Msg "Invalid Header Key")

  let headers =
    let rec loop source acc =
      if (not (Source.is_empty source)) && Source.get_unsafe source 0 = '\r'
      then (
        eol source;
        Header.of_list (List.rev acc))
      else
        let v = header source in
        eol source;
        loop source (v :: acc)
    in
    fun source -> loop source []

  let chunk_length source =
    let ( lsl ) = Int64.shift_left in
    let ( lor ) = Int64.logor in
    let length = ref 0L in
    let stop = ref false in
    let state = ref `Ok in
    let count = ref 0 in
    let processing_chunk = ref true in
    let in_chunk_extension = ref false in
    while not !stop do
      if Source.is_empty source then (
        stop := true;
        state := `Partial)
      else if !count = 16 && not !in_chunk_extension then (
        stop := true;
        state := `Chunk_too_big)
      else
        let ch = Source.get source 0 in
        Source.advance source 1;
        incr count;
        match ch with
        | '0' .. '9' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code '0') in
            length := (!length lsl 4) lor curr
        | 'a' .. 'f' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'a' + 10) in
            length := (!length lsl 4) lor curr
        | 'A' .. 'F' as ch when !processing_chunk ->
            let curr = Int64.of_int (Char.code ch - Char.code 'A' + 10) in
            length := (!length lsl 4) lor curr
        | ';' when not !in_chunk_extension ->
            in_chunk_extension := true;
            processing_chunk := false
        | ('\t' | ' ') when !processing_chunk -> processing_chunk := false
        | ('\t' | ' ') when (not !in_chunk_extension) && not !processing_chunk
          ->
            ()
        | '\r' ->
            if Source.is_empty source then (
              stop := true;
              state := `Partial)
            else if Source.get source 0 = '\n' then (
              Source.advance source 1;
              stop := true)
            else (
              stop := true;
              state := `Expected_newline)
        | _ when !in_chunk_extension ->
            (* Chunk extensions aren't very common, see:
               https://tools.ietf.org/html/rfc7230#section-4.1.1 Chunk extensions aren't
               pre-defined, and they are specific to invidividual connections. In the future
               we might surface these to the user somehow, but for now we will ignore any
               extensions. TODO: Should there be any limit on the size of chunk extensions we
               parse? We might want to error if a request contains really large chunk
               extensions. *)
            ()
        | ch ->
            stop := true;
            state := `Invalid_char ch
    done;
    match !state with
    | `Ok -> !length
    | `Partial -> raise_notrace Partial
    | `Expected_newline -> raise_notrace (Msg "Expected_newline")
    | `Chunk_too_big -> raise_notrace (Msg "Chunk size is too large")
    | `Invalid_char ch ->
        raise_notrace
          (Msg (Printf.sprintf "Invalid chunk_length character %C" ch))

  let version source =
    let version = version source in
    eol source;
    version

  let[@warning "-3"] request source =
    let meth = meth source in
    let path = token source in
    let version = version source in
    let headers = headers source in
    {
      Request.headers;
      meth;
      scheme = None;
      resource = path;
      version;
      encoding = Header.get_transfer_encoding headers;
    }

  type error = Partial | Msg of string

  let run_parser ?pos ?len buf p =
    let pos = Option.value pos ~default:0 in
    let source = Source.of_bytes ~pos ?len buf in
    match p source with
    | exception Partial -> Error Partial
    | exception Msg m -> Error (Msg m)
    | v ->
        let consumed = source.pos - pos in
        Ok (v, consumed)

  let parse_request ?pos ?len buf = run_parser ?pos ?len buf request
  let parse_chunk_length ?pos ?len buf = run_parser ?pos ?len buf chunk_length
end

module Private = struct
  module Parser = Parser
end

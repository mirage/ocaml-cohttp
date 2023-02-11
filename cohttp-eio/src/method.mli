(** ['body t] is HTTP request method.

    ['body] denotes the type of request body corresponsing to the method.

    Each variant represents a specific HTTP request method.

    - {!val:Get} https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/GET
    - {!val:Head} https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/HEAD
    - {!val:Delete}
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
    - {!val:Options}
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/OPTIONS
    - {!val:Trace}
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/TRACE
    - {!val:Post} https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/POST
    - {!val:Put} https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PUT
    - {!val:Patch}
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PATCH
    - {!val:Connect}
      https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/CONNECT *)

type 'a t =
  | Get : Body.none t
  | Head : Body.none t
  | Delete : Body.none t
  | Options : Body.none t
  | Trace : Body.none t
  | Post : 'a t
  | Put : 'a t
  | Patch : 'a t
  | Connect : Body.none t

val to_string : _ t -> string
(** [to_string t] is [s] - a string representation of [t]. *)

val of_string : string -> 'a t
(** [of_string s] is [t] - a typed representation of [s]. The represenation is
    case in-sensitive.

    @raise Invalid_argument if [s] is not one of the supported HTTP methods. *)

val pp : Format.formatter -> 'a t -> unit

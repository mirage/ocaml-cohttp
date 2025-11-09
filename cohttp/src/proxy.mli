module Make : functor
  (Ipaddr : sig
     type t

     val of_string : string -> (t, [> `Msg of string ]) result

     module Prefix : sig
       type addr = t
       type t

       val of_addr : addr -> t
       val of_string : string -> (t, [> `Msg of string ]) result
       val mem : addr -> t -> bool
     end
   end)
  -> sig
  type no_proxy_patterns
  (** Patterns matching URIs that should be excluded from proxying *)

  val no_proxy_from_env_value : string option -> no_proxy_patterns
  (** [no_proxy_from_env_value no_proxy_config] are the [no_proxy_patterns]
      parsed from the [no_proxy_config]. The [no_proxy_config] follows the
      same conventions used by
      {{:https://everything.curl.dev/usingcurl/proxies/env.html#no-proxy} curl}. *)

  val check_no_proxy : Uri.t -> no_proxy_patterns -> bool
  (** [check_no_proxy uri patterns] is true when [uri] matches one of the
      [patterns].*)

end
[@@warning "-unused-functor-parameter"]

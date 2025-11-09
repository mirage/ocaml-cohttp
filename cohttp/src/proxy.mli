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
  type no_proxy_pattern
  type no_proxy = Wildcard | Patterns of no_proxy_pattern list

  val no_proxy_from_env : string -> no_proxy
  val check_no_proxy_patterns : string -> no_proxy -> bool
end
[@@warning "-unused-functor-parameter"]

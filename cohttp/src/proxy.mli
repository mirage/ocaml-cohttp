(** Utilities for configuring and reasoning about forward proxies for client
    requests *)
module Forward : sig
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
    type ('direct, 'tunnel) servers
    (** A configuration for forward proxy servers *)

    (** A forward proxying connection *)
    type ('direct, 'tunnel) t =
      | Direct of 'direct  (** A proxy providing direct forwarding *)
      | Tunnel of 'tunnel
          (** A proxy using a tunnel (i.e. for https connections)) *)

    val make_servers :
      no_proxy_patterns:string option ->
      default_proxy:Uri.t option ->
      scheme_proxies:(string * Uri.t) list ->
      direct:(Uri.t -> 'direct) ->
      tunnel:(Uri.t -> 'tunnel) ->
      ('direct, 'tunnel) servers
    (** Create a new configuration of proxy servers

        @param no_proxy_patterns
          Disable proxies for specific hosts, specified as curl's [NO_PROXY].
        @see <https://github.com/curl/curl/blob/master/docs/MANUAL.md#environment-variables>

        @param default_proxy
          The default proxy to use. Proxy for specific schemes have precedence
          over this.

        @param scheme_proxies
          A mapping of (remote) scheme's to the desired proxy URI to user for
          calls with that scheme.

        @param direct
          A function to create ['direct] connections for the given proxy uri.

        @param tunnel
          A function to create ['tunnel] connections for the given proxy uri. *)

    val get : ('direct, 'tunnel) servers -> Uri.t -> ('direct, 'tunnel) t option
    (** [get proxies uri] finds the proxy configured for the [uri], if there is
        one given [proxies].

        @param servers The configured proxy servers
        @param uri The uri to find a proxy server for *)
  end
  [@@warning "-unused-functor-parameter"]
end

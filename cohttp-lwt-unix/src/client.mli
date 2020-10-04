
(** The [Client] module implements the full UNIX HTTP client interface,
    including the UNIX-specific functions defined in {!C }. *)

include Cohttp_lwt.S.Client with type ctx = Conduit.resolvers

val custom_ctx : ?tls_config:Tls.Config.client -> unit -> ctx

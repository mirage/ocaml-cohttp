
(** The [Client] module implements the full UNIX HTTP client interface,
    including the UNIX-specific functions defined in {!C }. *)

include Cohttp_lwt.S.Client with type ctx = Conduit.resolvers

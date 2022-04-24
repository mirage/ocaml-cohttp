module Make
    (P : Mirage_clock.PCLOCK)
    (R : Resolver_mirage.S)
    (S : Conduit_mirage.S) : sig
  module Connection : Cohttp_lwt.S.Connection
  include Cohttp_lwt.S.Client with type ctx = Connection.Net.ctx

  val ctx : ?authenticator:X509.Authenticator.t -> R.t -> S.t -> ctx
  val with_authenticator : X509.Authenticator.t -> ctx -> ctx
end

module Make
    (P : Mirage_clock.PCLOCK)
    (R : Resolver_mirage.S)
    (S : Conduit_mirage.S) : sig
  type ctx = {
    resolver : R.t;
    conduit : S.t option;
    authenticator : X509.Authenticator.t option;
  }

  include Cohttp_lwt.S.Net with type ctx := ctx
end

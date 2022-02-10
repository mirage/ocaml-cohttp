module Make
    (P : Mirage_clock.PCLOCK)
    (T : Mirage_time.S)
    (R : Resolver_mirage.S)
    (S : Conduit_mirage.S) : sig
  include Cohttp_lwt.S.Client

  module Connection_cache : sig
    include Cohttp_lwt.S.Connection_cache

    val create :
      ?ctx:ctx ->
      ?keep:int64 ->
      ?retry:int ->
      ?parallel:int ->
      ?depth:int ->
      unit -> t
  end

  val ctx : ?authenticator:X509.Authenticator.t -> R.t -> S.t -> ctx
  val with_authenticator : X509.Authenticator.t -> ctx -> ctx
end

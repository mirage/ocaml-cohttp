module type S = sig
  module IO : IO.S

  module PStateIO : module type of Parameterised_monads.PStateT(IO)

  type ('a, 'b) chunk_handler = string -> ('a, 'b, unit) PStateIO.t

  type ('response, 'writer) response_handler = {
    failure : 'r. ([`Waiting_for_response], [>`Die] as 'r, unit) PStateIO.t ;
    response : 'r. 'response -> ([`Waiting_for_response], [>`Getting_body of 'writer] as 'r, unit) PStateIO.t ;
    body : string -> ([`Getting_body of 'writer|`Junking_body], [`Getting_body of 'writer|`Junking_body], unit) PStateIO.t ;
    body_end : 'r. ([`Getting_body of 'writer|`Junking_body], [>`Complete] as 'r, unit) PStateIO.t ;
  }
end

module Make(IO : IO.S) : S with module IO = IO =
struct
  module IO = IO

  module PStateIO = Parameterised_monads.PStateT(IO)

  type ('a, 'b) chunk_handler = string -> ('a, 'b, unit) PStateIO.t

  type ('response, 'writer) response_handler = {
    failure : 'r. ([`Waiting_for_response], [>`Die] as 'r, unit) PStateIO.t ;
    response : 'r. 'response -> ([`Waiting_for_response], [>`Getting_body of 'writer] as 'r, unit) PStateIO.t ;
    body : string -> ([`Getting_body of 'writer|`Junking_body], [`Getting_body of 'writer|`Junking_body], unit) PStateIO.t ;
    body_end : 'r. ([`Getting_body of 'writer|`Junking_body], [>`Complete] as 'r, unit) PStateIO.t ;
  }
end

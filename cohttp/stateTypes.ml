module type S = sig
  module IO : IO.S

  module PStateIO : module type of Parameterised_monads.PStateT(IO)

  type chunk_handler = {
    chunk : 'a. string -> ([`Working], [>`Working] as 'a, unit) PStateIO.t ;
    all_done : 'a. ([`Working], [>`Finished] as 'a, unit) PStateIO.t ;
  }
end

module Make(IO : IO.S) : S with module IO = IO =
struct
  module IO = IO

  module PStateIO = Parameterised_monads.PStateT(IO)

  type chunk_handler = {
    chunk : 'a. string -> ([`Working], [>`Working] as 'a, unit) PStateIO.t ;
    all_done : 'a. ([`Working], [>`Finished] as 'a, unit) PStateIO.t ;
  }
end

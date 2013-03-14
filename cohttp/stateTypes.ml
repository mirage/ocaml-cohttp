type (_, _) chunk_state =
    Done : ([<`Working], [>`Finished]) chunk_state
  | Chunk : string -> ([<`Working], [>`Working]) chunk_state

module type S = sig
  module IO : IO.S

  module PStateIO : module type of Parameterised_monads.PStateT(IO)

  type chunk_reader = {
    process : 'a 'b. ('a, 'b) chunk_state -> ('a, 'b, unit) PStateIO.t
  }
end

module Make(IO : IO.S) : S with module IO = IO =
struct
  module IO = IO

  module PStateIO = Parameterised_monads.PStateT(IO)

  type chunk_reader = {
    process : 'a 'b. ('a, 'b) chunk_state -> ('a, 'b, unit) PStateIO.t
  }
end

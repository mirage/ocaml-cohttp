type t =
  | Chunk of { size : int; data : Cstruct.t; extensions : extension list }
  | Last_chunk of extension list

and extension = { name : string; value : string option }

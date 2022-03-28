type t = Chunk of chunk | Last_chunk of extension list
and chunk = { size : int; data : Cstruct.t; extensions : extension list }
and extension = { name : string; value : string option }

let pp_extensions fmt =
  Fmt.(
    vbox
    @@ list ~sep:Fmt.semi
    @@ record
         [
           Fmt.field "name" (fun ext -> ext.name) Fmt.string;
           Fmt.field "value" (fun ext -> ext.value) Fmt.(option string);
         ])
    fmt

let pp fmt = function
  | Chunk chunk ->
      Fmt.(
        record
          [
            Fmt.field "size" (fun t -> t.size) Fmt.int;
            Fmt.field "data" (fun t -> Cstruct.to_string t.data) Fmt.string;
            Fmt.field "extensions" (fun t -> t.extensions) pp_extensions;
          ])
        fmt chunk
  | Last_chunk extensions -> pp_extensions fmt extensions

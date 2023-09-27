type t = Eio.Flow.source_ty Eio.Resource.t
type 't Eio.Flow.read_method += String of ('t -> string)

module String_source = struct
  type t = { s : string; mutable offset : int }

  let single_read t dst =
    if t.offset = String.length t.s then raise End_of_file;
    let len = min (Cstruct.length dst) (String.length t.s - t.offset) in
    Cstruct.blit_from_string t.s t.offset dst 0 len;
    t.offset <- t.offset + len;
    len

  let original_string t = t.s
  let read_methods = [ String original_string ]
  let create s = { s; offset = 0 }
end

let of_string =
  let ops = Eio.Flow.Pi.source (module String_source) in
  fun s -> Eio.Resource.T (String_source.create s, ops)

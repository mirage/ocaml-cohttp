type t = Eio.Flow.source
type Eio.Flow.read_method += String of string

let of_string s =
  let source = Eio.Flow.string_source s in
  object
    inherit Eio.Flow.source
    method! read_methods = String s :: source#read_methods
    method read_into dst = source#read_into dst
  end

open Sexplib.Std
type t = Uri.t
let sexp_of_t t = t |> Uri.to_string |> sexp_of_string
let t_of_sexp s = s |> string_of_sexp |> Uri.of_string

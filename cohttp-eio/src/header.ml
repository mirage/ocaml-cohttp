include Http.Header
open Easy_format

let field lbl v =
  let lbl = Atom (lbl ^ ": ", atom) in
  let v = Atom (v, atom) in
  Label ((lbl, label), v)

let easy_fmt t =
  let p =
    {
      list with
      stick_to_label = false;
      align_closing = true;
      space_after_separator = true;
      wrap_body = `Force_breaks;
    }
  in
  let t = to_list t |> List.map (fun (k, v) -> field k v) in
  List (("{", ";", "}", p), t)

let pp fmt t = Easy_format.Pretty.to_formatter fmt (easy_fmt t)

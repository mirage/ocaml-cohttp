open String
(* Not tail recursive for "performance", please choose low values for
   [max]. The idea is that max is always small because it's hard
   code *)
let split_char_bounded str ~on ~max =
  if str = "" then []
  else if max = 1 then [str]
  else
    let rec loop offset tokens =
      if tokens = max - 1
      then [sub str offset (length str - offset)]
      else
        try
          let index = index_from str offset on in
          if index = offset then
            ""::(loop (offset + 1) (tokens + 1))
          else
            let token = String.sub str offset (index - offset) in
            token::(loop (index + 1) (tokens + 1))
        with Not_found -> [sub str offset (length str - offset)]
    in loop 0 0

let split_char_unbounded str ~on =
  if str = "" then []
  else
    let rec loop acc offset =
      try begin
        let index = rindex_from str offset on in
        if index = offset then
          loop (""::acc) (index - 1)
        else
          let token = sub str (index + 1) (offset - index) in
          loop (token::acc) (index - 1)
      end
      with Not_found -> (sub str 0 (offset + 1))::acc
    in loop [] (length str - 1)

(* copying core's convention for String.split but with an optional max
   argument *)
let split ?max s ~on =
  match max with
  | None -> split_char_unbounded s ~on
  | Some max ->                 (* assert (max < 100); *)
    split_char_bounded s ~on ~max

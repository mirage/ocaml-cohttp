open String

let string_after s n = String.sub s n (String.length s - n)

let quote s =
  let len = String.length s in
  let buf = String.create (2 * len) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
      buf.[!pos] <- '\\'; buf.[!pos + 1] <- c; pos := !pos + 2
    | c ->
      buf.[!pos] <- c; pos := !pos + 1
  done;
  String.sub buf 0 !pos

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

let trim_left ?(start=0) ?stop s ~on =
  let stop = match stop with
    | None -> String.length s
    | Some x -> x
  in
  let rec loop i =
    if String.contains on s.[i]
    then loop (i + 1)
    else String.sub s i (stop - i)
  in loop start

let rindex_from_on s ~offset ~on =
  let rec loop i =
    if i < 0 then raise Not_found
    else if String.contains on s.[i] then i
    else loop (i - 1)
  in loop offset

(* Like String.sub but trim [chars] if they exist on the left *)
let trim_left_sub s ~pos ~len ~chars =
  let start_pos =
    let final = pos + len in
    let rec loop last_char i =
      if i = final then last_char
      else if String.contains chars s.[i] then loop (i + 1) (i + 1)
      else loop last_char (i + 1)
    in loop pos pos
  in
  let new_len = len - (start_pos - pos) in
  String.sub s start_pos new_len

let split_trim_right str ~on ~trim =
  if str = "" then []
  else
    let rec loop acc offset =
      try begin
        let index = rindex_from_on str ~offset ~on in
        if index = offset then
          loop (""::acc) (index - 1)
        else
          let token = trim_left_sub str ~pos:(index + 1)
                        ~len:(offset - index) ~chars:trim in
          loop (token::acc) (index - 1)
      end
      with Not_found -> (sub str 0 (offset + 1))::acc
    in loop [] (length str - 1)

let split_cookie str =
  split_trim_right str ~on:",;" ~trim:" \t"

let split_cookie_1_0 str =
  split_trim_right str ~on:";" ~trim:" \t"

exception Found_int of int

let first_char_ne s c =
  String.length s > 0 && s.[0] <> c

(* Trim spaces from the start of the strings. Returns the same string
   in case no mutation is needed *)
let trim_left s =
  if first_char_ne s ' ' then s
  else
    let len = String.length s in
    try
      for i=0 to len - 1 do
        if s.[i] <> ' ' then raise (Found_int i)
      done;
      s
    with Found_int non_space ->
      sub s non_space (len - non_space)

let split_header str =
  match str |> split ~max:2 ~on:':' with
  | x::y::[] -> [x; trim_left y]
  | x -> x

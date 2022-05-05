(* Based on https://github.com/inhabitedtype/angstrom/blob/master/lib/buffering.ml *)
type t = {
  source : Eio.Flow.source;
  mutable buf : Bigstringaf.t;
  mutable off : int;
  mutable len : int;
  mutable pos : int; (* Parser position *)
  mutable committed_bytes : int; (* Total bytes read so far *)
}

let create len source =
  assert (len > 0);
  let buf = Bigstringaf.create len in
  { source; buf; off = 0; len = 0; pos = 0; committed_bytes = 0 }

let length t = t.len
let committed_bytes t = t.committed_bytes
let pos t = t.pos
let incr_pos ?(n = 1) t = t.pos <- t.pos + n
let writable_space t = Bigstringaf.length t.buf - t.len
let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

let compress t =
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0 ~len:t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstringaf.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := 3 * (!new_len + 1) / 2
  done;
  let new_buf = Bigstringaf.create !new_len in
  Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0 ~len:t.len;
  t.buf <- new_buf;
  t.off <- 0

let adjust_buffer t to_read =
  if trailing_space t < to_read then
    if writable_space t < to_read then grow t to_read else compress t

let consume t n =
  assert (t.len >= n);
  assert (t.pos >= n);
  t.off <- t.off + n;
  t.len <- t.len - n;
  t.pos <- t.pos - n;
  t.committed_bytes <- t.committed_bytes + n

let commit t = consume t t.pos

let fill t to_read =
  adjust_buffer t to_read;
  let off = t.off + t.len in
  let len = trailing_space t in
  let cs = Cstruct.of_bigarray ~off ~len t.buf in
  let got = Eio.Flow.read t.source cs in
  t.len <- t.len + got;
  got

let unsafe_get t off = Bigstringaf.unsafe_get t.buf (t.off + off)

let sub t ~off ~len =
  let b = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes t.buf ~src_off:(t.off + off) b ~dst_off:0
    ~len;
  b

let substring t ~off ~len =
  let b = sub t ~off ~len in
  Bytes.unsafe_to_string b

let copy t ~off ~len = Bigstringaf.copy t.buf ~off:(t.off + off) ~len

(** Parser combinators *)

type 'a parser = t -> 'a

exception Parse_failure of string

let return v _ = v
let fail err _ = Stdlib.raise_notrace (Parse_failure err)
let commit rdr = commit rdr
let ( <?> ) p err rdr = try p rdr with Parse_failure _e -> fail err rdr

let ( >>= ) p f rdr =
  let a = p rdr in
  f a rdr

let ( let* ) = ( >>= )

let ( >>| ) p f rdr =
  let v = p rdr in
  f v

let ( let+ ) = ( >>| )

let ( <* ) p q rdr =
  let a = p rdr in
  let _ = q rdr in
  a

let ( *> ) p q rdr =
  let _ = p rdr in
  q rdr

let ( <|> ) p q rdr =
  let old_pos = pos rdr in
  let old_committed = committed_bytes rdr in
  try p rdr
  with Parse_failure _ as ex ->
    if old_committed < committed_bytes rdr then raise_notrace ex
    else (
      rdr.pos <- old_pos;
      q rdr)

let lift f p = p >>| f

let lift2 f p q rdr =
  let a = p rdr in
  let b = q rdr in
  f a b

let rec ensure rdr len =
  if length rdr < pos rdr + len then (
    ignore (fill rdr len);
    ensure rdr len)

(* let pos rdr = pos rdr *)

let end_of_input rdr =
  try
    ensure rdr 1;
    false
  with End_of_file -> true

let option : 'a -> 'a parser -> 'a parser = fun x p -> p <|> return x

let peek_char rdr =
  if pos rdr < length rdr then unsafe_get rdr (pos rdr)
  else (
    ensure rdr 1;
    unsafe_get rdr rdr.pos)

let peek_string n rdr =
  try
    ensure rdr n;
    substring rdr ~off:rdr.pos ~len:n
  with End_of_file -> fail "[peek_string] not enough input" rdr

let sprintf = Printf.sprintf

let char c rdr =
  let c' = peek_char rdr in
  if c = c' then incr_pos rdr
  else fail (sprintf "[char] expected %C, got %C" c c') rdr

let any_char rdr =
  ensure rdr 1;
  let c = unsafe_get rdr rdr.pos in
  incr_pos rdr;
  c

let satisfy f rdr =
  let c = peek_char rdr in
  if f c then (
    incr_pos rdr;
    c)
  else fail "[satisfy]" rdr

let string s rdr =
  let len = String.length s in
  ensure rdr len;
  let pos = pos rdr in
  let i = ref 0 in
  while
    !i < len && Char.equal (unsafe_get rdr (pos + !i)) (String.unsafe_get s !i)
  do
    incr i
  done;
  if len = !i then incr_pos ~n:len rdr else fail "[string]" rdr

let fix f =
  let rec p = lazy (f r) and r inp = (Lazy.force p) inp in
  r

let count_while rdr f =
  let i = ref 0 in
  let continue = ref true in
  while !continue do
    try
      ensure rdr (!i + 1);
      let c = unsafe_get rdr (pos rdr + !i) in
      if f c then incr i else continue := false
    with End_of_file -> continue := false
  done;
  !i

let take_while1 f rdr =
  let count = count_while rdr f in
  if count < 1 then fail "[take_while1] count is less than 1" rdr
  else
    let s = substring rdr ~off:(pos rdr) ~len:count in
    incr_pos ~n:count rdr;
    s

let take_while f rdr =
  let count = count_while rdr f in
  if count > 0 then (
    let s = substring rdr ~off:(pos rdr) ~len:count in
    incr_pos ~n:count rdr;
    s)
  else ""

let take_bigstring : int -> Bigstringaf.t parser =
 fun n rdr ->
  try
    ensure rdr n;
    let s = copy rdr ~off:(pos rdr) ~len:n in
    incr_pos ~n rdr;
    s
  with End_of_file -> fail "[take_bigstring] not enough input" rdr

let take_bytes : int -> bytes parser =
 fun n rdr ->
  try
    ensure rdr n;
    let s = sub rdr ~off:(pos rdr) ~len:n in
    incr_pos ~n rdr;
    s
  with End_of_file -> fail "[take] not enough input" rdr

let take : int -> string parser =
 fun n rdr ->
  try
    ensure rdr n;
    let s = substring rdr ~off:(pos rdr) ~len:n in
    incr_pos ~n rdr;
    s
  with End_of_file -> fail "[take] not enough input" rdr

let take_till f = take_while (fun c -> not (f c))

let rec many : 'a parser -> 'a list parser =
 fun p rdr ->
  try
    let a = p rdr in
    a :: many p rdr
  with Parse_failure _ | End_of_file -> []

let rec many_till : 'a parser -> _ parser -> 'a list parser =
 fun p t rdr ->
  try
    let _ = t rdr in
    let a = p rdr in
    a :: many_till p t rdr
  with Parse_failure _ -> []

let skip f rdr =
  ensure rdr 1;
  let c = unsafe_get rdr (pos rdr) in
  if f c then incr_pos rdr else fail "[skip]" rdr

let skip_while f rdr =
  let count = count_while rdr f in
  incr_pos ~n:count rdr

let rec skip_many p rdr =
  match p rdr with _ -> skip_many p rdr | exception Parse_failure _ -> ()

(* Builtin readers *)

let token =
  take_while1 (function
    | '0' .. '9'
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | _ -> false)

let ows = skip_while (function ' ' | '\t' -> true | _ -> false)
let crlf = string "\r\n"
let is_cr = function '\r' -> true | _ -> false
let space = char '\x20'
let p_meth = token <* space >>| Http.Method.of_string
let p_resource = take_while1 (fun c -> c != ' ') <* space

let p_version =
  string "HTTP/1." *> any_char <* crlf >>= function
  | '1' -> return `HTTP_1_1
  | '0' -> return `HTTP_1_0
  | v -> fail (Format.sprintf "Invalid HTTP version: %C" v)

let header =
  lift2
    (fun key value -> (key, value))
    (token <* char ':' <* ows)
    (take_till is_cr <* crlf)

let http_headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
      let _emp = return [] in
      let _rec = lift2 cons header headers in
      peek_char >>= function '\r' -> _emp | _ -> _rec)
  >>| Http.Header.of_list
  <* crlf

let[@warning "-3"] http_request t =
  match end_of_input t with
  | true -> Stdlib.raise_notrace End_of_file
  | false ->
      let meth = p_meth t in
      let resource = p_resource t in
      let version = p_version t in
      let headers = http_headers t in
      let encoding = Http.Header.get_transfer_encoding headers in
      commit t;
      { Http.Request.meth; resource; version; headers; scheme = None; encoding }

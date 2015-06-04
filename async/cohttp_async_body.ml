open Core.Std
open Async.Std

module B = Cohttp.Body
type t = [
  | B.t
  | `Pipe of string Pipe.Reader.t
]
with sexp_of

let empty = `Empty
let of_string s = ((B.of_string s) :> t)
let of_pipe p = `Pipe p

let to_string = function
  | #B.t as body -> return (B.to_string body)
  | `Pipe s -> Pipe.to_list s >>| String.concat

let to_string_list = function
  | #B.t as body -> return (B.to_string_list body)
  | `Pipe s -> Pipe.to_list s

let drain = function
  | #B.t -> return ()
  | `Pipe p -> Pipe.drain p

let is_empty (body:t) =
  match body with
  | #B.t as body -> return (B.is_empty body)
  | `Pipe s ->
    Pipe.values_available s
    >>| function
    |`Eof -> false
    |`Ok ->
      match Pipe.peek s with
      | Some "" -> true
      | Some _ | None -> false

let to_pipe = function
  | `Empty -> Pipe.of_list []
  | `String s -> Pipe.of_list [s]
  | `Strings sl -> Pipe.of_list sl
  | `Pipe p -> p

let disable_chunked_encoding = function
  | #B.t as body -> return (body, B.length body)
  | `Pipe s ->
    Pipe.to_list s >>| fun l ->
    let body = `Strings l in
    let len = B.length body in
    body, len

let transfer_encoding = function
  | #B.t as t -> B.transfer_encoding t
  | `Pipe _ -> Cohttp.Transfer.Chunked

let of_string_list strings = `Pipe (Pipe.of_list strings)

let write write_body body writer =
  match body with
  | `Empty -> return ()
  | `String s -> write_body writer s
  | `Strings sl -> Deferred.List.iter sl ~f:(write_body writer)
  | `Pipe p -> Pipe.iter p ~f:(write_body writer)

let map t ~f =
  match t with
  | #B.t as t -> (B.map f t :> t)
  | `Pipe p -> `Pipe (Pipe.map p ~f)

let as_pipe t ~f = `Pipe (t |> to_pipe |> f)

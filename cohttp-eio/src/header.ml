type name = string (* Header name, e.g. Date, Content-Length etc *)
type value = string (* Header value, eg 10, text/html, chunked etc *)
type lname = string

let canonical_name nm =
  String.split_on_char '-' nm
  |> List.map (fun s -> String.(lowercase_ascii s |> capitalize_ascii))
  |> String.concat "-"

let lname = String.lowercase_ascii

type 'a header = ..

type 'a header +=
  | Content_length : int header
  | Transfer_encoding : [ `chunked | `compress | `deflate | `gzip ] list header
  | H : lname -> value header

type 'a decoder = value -> 'a
type 'a encoder = 'a -> value
type 'a undecoded = 'a Lazy.t

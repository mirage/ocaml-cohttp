open Lwt
open Unix
open Sexplib.Std
open Sexplib.Conv

type version = IPv4 | IPv6 with sexp
type t = Lwt_unix.inet_addr * int

let sexp_of_t = sexp_of_pair sexp_of_opaque sexp_of_int

let loopback version ~port = match version with
  | IPv6 -> inet6_addr_loopback, port
  | IPv4 -> inet_addr_loopback, port

let any version port = match version with
  | IPv6 -> inet6_addr_any, port
  | IPv4 -> inet_addr_any, port

let make addr port =
  inet_addr_of_string addr, port

let to_string (addr, port) =
  (string_of_inet_addr addr) ^ ":" ^ (string_of_int port)

let addr (addr, _) = string_of_inet_addr addr
let port (_, port) = port

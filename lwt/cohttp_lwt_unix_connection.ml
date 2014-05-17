open Lwt
open Unix

type version = IPv4 | IPv6
type t = Lwt_unix.inet_addr * int

let loopback version port = match version with
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

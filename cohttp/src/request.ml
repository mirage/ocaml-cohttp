(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Sexplib0.Sexp_conv

type t = Http.Request.t = {
  headers : Header.t;
  meth : Code.meth;
  scheme : string option;
  resource : string;
  version : Code.version;
  encoding : Transfer.encoding;
}
[@@deriving sexp]

let compare { headers; meth; scheme; resource; version; encoding } y =
  match Header.compare headers y.headers with
  | 0 -> (
      match Code.compare_method meth y.meth with
      | 0 -> (
          match Option.compare String.compare scheme y.scheme with
          | 0 -> (
              match String.compare resource y.resource with
              | 0 -> (
                  match Code.compare_version version y.version with
                  | 0 -> Stdlib.compare encoding y.encoding
                  | i -> i)
              | i -> i)
          | i -> i)
      | i -> i)
  | i -> i

let headers t = t.headers
let meth t = t.meth
let scheme t = t.scheme
let resource t = t.resource
let version t = t.version
let encoding t = t.encoding

let make ?(meth = `GET) ?(version = `HTTP_1_1) ?(encoding = Transfer.Unknown)
    ?(headers = Header.init ()) uri =
  let headers =
    Header.add_unless_exists headers "host"
      (match Uri.scheme uri with
      | Some "httpunix" -> ""
      | _ -> (
          Uri.host_with_default ~default:"localhost" uri
          ^
          match Uri.port uri with Some p -> ":" ^ string_of_int p | None -> ""))
  in
  let headers =
    Header.add_unless_exists headers "user-agent" Header.user_agent
  in
  let headers =
    (* Add user:password auth to headers from uri
     * if headers don't already have auth *)
    match
      (Header.get_authorization headers, Uri.user uri, Uri.password uri)
    with
    | None, Some user, Some pass ->
        let auth = `Basic (user, pass) in
        Header.add_authorization headers auth
    | _, _, _ -> headers
  in
  let scheme = Uri.scheme uri in
  let resource = Uri.path_and_query uri in
  let encoding =
    match Header.get_transfer_encoding headers with
    | Transfer.Unknown -> encoding
    | encoding -> encoding
  in
  { headers; meth; scheme; resource; version; encoding }

let is_keep_alive t = Http.Request.is_keep_alive t

(* Make a client request, which involves guessing encoding and
   adding content headers if appropriate.
   @param chunked Forces chunked encoding
*)
let make_for_client ?headers ?(chunked = true) ?(body_length = Int64.zero) meth
    uri =
  let encoding =
    match chunked with
    | true -> Transfer.Chunked
    | false -> Transfer.Fixed body_length
  in
  make ~meth ~encoding ?headers uri

let pp_hum ppf r =
  Format.fprintf ppf "%s" (r |> sexp_of_t |> Sexplib0.Sexp.to_string_hum)

(* Validate path when reading URI. Implemented for compatibility with old
   implementation rather than efficiency *)
let is_valid_uri path meth =
  path = "*"
  || meth = `CONNECT
  ||
  match Uri.scheme (Uri.of_string path) with
  | Some _ -> true
  | None -> not (String.length path > 0 && path.[0] <> '/')

let uri { scheme; resource; headers; meth; _ } =
  let uri =
    match resource with
    | "*" -> (
        match Header.get headers "host" with
        | None -> Uri.of_string ""
        | Some host ->
            let host_uri = Uri.of_string ("//" ^ host) in
            Uri.(make ?host:(host host_uri) ?port:(port host_uri) ()))
    | authority when meth = `CONNECT -> Uri.of_string ("//" ^ authority)
    | path -> (
        let uri = Uri.of_string path in
        match Uri.scheme uri with
        | Some _ -> (
            Uri.(
              (* we have an absoluteURI *)
              match path uri with "" -> with_path uri "/" | _ -> uri))
        | None ->
            let empty = Uri.of_string "" in
            let empty_base = Uri.of_string "///" in
            let pqs =
              match Stringext.split ~max:2 path ~on:'?' with
              | [] -> empty_base
              | [ path ] ->
                  Uri.resolve "http" empty_base (Uri.with_path empty path)
              | path :: qs :: _ ->
                  let path_base =
                    Uri.resolve "http" empty_base (Uri.with_path empty path)
                  in
                  Uri.with_query path_base (Uri.query_of_encoded qs)
            in
            let uri =
              match Header.get headers "host" with
              | None -> Uri.(with_scheme (with_host pqs None) None)
              | Some host ->
                  let host_uri = Uri.of_string ("//" ^ host) in
                  let uri = Uri.with_host pqs (Uri.host host_uri) in
                  Uri.with_port uri (Uri.port host_uri)
            in
            uri)
  in
  (* Only set the scheme if it's not already part of the URI *)
  match Uri.scheme uri with Some _ -> uri | None -> Uri.with_scheme uri scheme

type tt = t

module Make (IO : S.IO) = struct
  type t = tt

  module IO = IO
  module Header_IO = Header_io.Make (IO)
  module Transfer_IO = Transfer_io.Make (IO)

  type reader = Transfer_IO.reader
  type writer = Transfer_IO.writer

  open IO

  let rec read ic =
    let result =
      IO.with_input_buffer ic ~f:(fun buf ~pos ~len ->
          match Http.Private.Parser.parse_request ~pos ~len buf with
          | Ok (req, consumed) -> (`Ok req, consumed)
          | Error Partial -> (`Partial, 0)
          | Error (Msg msg) -> (`Invalid msg, 0))
    in
    match result with
    | `Partial -> (
        IO.refill ic >>= function `Ok -> read ic | `Eof -> return `Eof)
    | `Ok req ->
        if is_valid_uri (Http.Request.resource req) (Http.Request.meth req) then
          return (`Ok req)
        else return (`Invalid "bad request URI")
    | `Invalid msg -> return (`Invalid msg)

  let make_body_reader req ic = Transfer_IO.make_reader req.encoding ic
  let read_body_chunk = Transfer_IO.read

  let write_header req oc =
    let fst_line =
      Printf.sprintf "%s %s %s\r\n"
        (Http.Method.to_string req.meth)
        (if req.resource = "" then "/" else req.resource)
        (Http.Version.to_string req.version)
    in
    let headers = req.headers in
    let headers =
      if Http.Method.body_allowed req.meth then
        Header.add_transfer_encoding headers req.encoding
      else headers
    in
    IO.write oc fst_line >>= fun _ -> Header_IO.write headers oc

  let make_body_writer ?flush req oc =
    Transfer_IO.make_writer ?flush req.encoding oc

  let write_body = Transfer_IO.write

  let write_footer req oc =
    match req.encoding with
    | Transfer.Chunked ->
        (* TODO Trailer header support *)
        IO.write oc "0\r\n\r\n"
    | Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write ?flush write_body req oc =
    write_header req oc >>= fun () ->
    let writer = make_body_writer ?flush req oc in
    write_body writer >>= fun () -> write_footer req oc
end

module Private = struct
  module Make = Make
end

let has_body = Http.Request.has_body

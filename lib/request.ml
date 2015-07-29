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

open Sexplib.Std

type t = {
  headers: Header.t;
  meth: Code.meth;
  uri: Uri.t;
  version: Code.version;
  encoding: Transfer.encoding;
} with fields, sexp

let make ?(meth=`GET) ?(version=`HTTP_1_1) ?encoding ?headers uri =
  let headers =
    match headers with
    | None -> Header.init ()
    | Some h -> h in
  let headers =
    (* Add user:password auth to headers from uri
     * if headers don't already have auth *)
    match Header.get_authorization headers, Uri.user uri, Uri.password uri with
    | None, Some user, Some pass ->
      let auth = `Basic (user, pass) in
      Header.add_authorization headers auth
    | _, _, _ -> headers
  in
  let encoding =
    (* Check for a content-length in the supplied headers first *)
    match Header.get_content_range headers with
    | Some clen -> Transfer.Fixed clen
    | None -> begin
        (* Otherwise look for an API-level encoding specification *)
        match encoding with
        | None -> Transfer.Fixed Int64.zero
        | Some e -> e
      end
  in
  { meth; version; headers; uri; encoding }

let is_keep_alive { version; headers; _ } =
  not (version = `HTTP_1_0 ||
       (match Header.connection headers with
        | Some `Close -> true
        | _ -> false))

(* Make a client request, which involves guessing encoding and
   adding content headers if appropriate.
   @param chunked Forces chunked encoding
*)
let make_for_client ?headers ?(chunked=true) ?(body_length=Int64.zero) meth uri =
  let encoding =
    match chunked with
    | true -> Transfer.Chunked
    | false -> Transfer.Fixed body_length
  in
  make ~meth ~encoding ?headers uri

let has_body req =
  match req.meth with
  | `GET | `HEAD | `CONNECT | `TRACE -> `No
  | `DELETE | `POST | `PUT | `PATCH | `OPTIONS | `Other _ ->
    Transfer.has_body req.encoding

let prepare req =
  let headers = Header.add_unless_exists req.headers "host"
                  (Uri.host_with_default ~default:"localhost" req.uri ^
                   match Uri.port req.uri with
                   | Some p -> ":" ^ string_of_int p
                   | None -> ""
                  ) in
  let headers =
    match has_body req with
    | `Yes | `Unknown -> Header.add_transfer_encoding headers req.encoding
    | `No -> headers in
  { req with headers }

let to_string_list req =
  let headers = Header.to_string req.headers in
  let line = Printf.sprintf "%s %s %s\r\n"
               (Code.string_of_method req.meth)
               (Uri.path_and_query req.uri)
               (Code.string_of_version req.version) in
  [ line ; headers ]

let request_line_of_string request_line =
  match Stringext.split request_line ~on:' ' with
  | [ meth_raw; path; http_ver_raw ] -> begin
      let m = Code.method_of_string meth_raw in
      match Code.version_of_string http_ver_raw with
      | `HTTP_1_1 | `HTTP_1_0 as v -> (`Ok (m, path, v))
      | `Other _ -> `Error ("Malformed request HTTP version: " ^ http_ver_raw)
    end
  | _ -> `Error ("Malformed request header: " ^ request_line)

let return_request headers meth uri version =
  let encoding = Header.get_transfer_encoding headers in
  `Ok { headers; meth; uri; version; encoding }

let of_string_list strs =
  match strs with
  | [] -> `Error "Empty"
  | fst::headers ->
    begin match request_line_of_string fst with
    | `Error reason as r -> r
    | `Ok (m, p, v) ->
      begin match Header.of_lines headers with
      | None -> `Error "Failed to parse headers"
      | Some headers ->
        begin match m, p, v with
        | (meth, "*", version) ->
          let uri = match Header.get headers "host" with
            | None -> Uri.of_string ""
            | Some host ->
              let host_uri = Uri.of_string ("//"^host) in
              let uri = Uri.(with_host (of_string "") (host host_uri)) in
              Uri.(with_port uri (port host_uri))
          in
          return_request headers meth uri version
        | (`CONNECT as meth, authority, version) ->
          let uri = Uri.of_string ("//"^authority) in
          return_request headers meth uri version
        | (meth, request_uri_s, version) ->
          let uri = Uri.of_string request_uri_s in
          match Uri.scheme uri with
          | Some _ -> (* we have an absoluteURI *)
            let uri = Uri.(
              match path uri with "" -> with_path uri "/" | _ -> uri
            ) in
            return_request headers meth uri version
          | None ->
            let len = String.length request_uri_s in
            if len > 0 && String.get request_uri_s 0 <> '/'
            then (`Error "bad request URI")
            else
              let empty = Uri.of_string "" in
              let empty_base = Uri.of_string "///" in
              let pqs = match Stringext.split ~max:2 request_uri_s ~on:'?' with
                | [] -> empty_base
                | [path] ->
                  Uri.resolve "http" empty_base (Uri.with_path empty path)
                | path::qs::_ ->
                  let path_base =
                    Uri.resolve "http" empty_base (Uri.with_path empty path)
                  in
                  Uri.with_query path_base (Uri.query_of_encoded qs)
              in
              let uri = match Header.get headers "host" with
                | None -> Uri.(with_scheme (with_host pqs None) None)
                | Some host ->
                  let host_uri = Uri.of_string ("//"^host) in
                  let uri = Uri.with_host pqs (Uri.host host_uri) in
                  Uri.with_port uri (Uri.port host_uri)
              in
              return_request headers meth uri version
        end
      end
    end

let pp_hum ppf r =
  Format.fprintf ppf "%s" (r |> sexp_of_t |> Sexplib.Sexp.to_string_hum)

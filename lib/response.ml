(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
  encoding: Transfer.encoding;
  headers: Header.t;
  version: Code.version;
  status: Code.status_code;
  flush: bool;
} with fields, sexp

let make ?(version=`HTTP_1_1) ?(status=`OK) ?(flush=false) ?(encoding=Transfer.Chunked) ?headers () =
  let headers = match headers with None -> Header.init () |Some h -> h in
  { encoding; headers; version; flush; status }

let pp_hum ppf r =
  Format.fprintf ppf "%s" (r |> sexp_of_t |> Sexplib.Sexp.to_string_hum)

let allowed_body response = (* rfc7230#section-5.7.1 *)
  match status response with
  | #Code.informational_status | `No_content | `Not_modified -> false
  | #Code.status_code -> true

let prepare rep =
  let headers =
    if allowed_body rep
    then Header.add_transfer_encoding rep.headers rep.encoding
    else rep.headers in
  { rep with headers }

let to_string_list rep =
  let first_line = Printf.sprintf "%s %s\r\n"
                     (Code.string_of_version rep.version)
                     (Code.string_of_status rep.status) in
  let headers = Header.to_string rep.headers in
  [ first_line ; headers ]


let parse_response_fst_line response_line =
  let open Code in
  match Stringext.split response_line ~on:' ' with
  | version_raw :: code_raw :: _ -> begin
      match version_of_string version_raw with
      | `HTTP_1_0
      | `HTTP_1_1 as v -> `Ok (v, (status_of_code (int_of_string code_raw)))
      | `Other _ -> `Error ("Malformed response version: " ^ version_raw)
    end
  | _ -> `Error ("Malformed response first line: " ^ response_line)

let of_lines lines =
  match lines with
  | [] -> `Error "Response is empty"
  | fst_line::headers ->
    begin match parse_response_fst_line fst_line with
    | `Error _ as r -> r
    | `Ok (version, status) ->
      match Header.of_lines headers with
      | None -> `Error "invalid headers"
      | Some headers ->
        let encoding = Header.get_transfer_encoding headers in
        let flush = false in
        `Ok { encoding; headers; version; status; flush }
    end

let has_body response =
  if allowed_body response
  then Transfer.has_body (encoding response)
  else `No

let allowed_body response = (* rfc7230#section-5.7.1 *)
  match status response with
  | #Code.informational_status | `No_content | `Not_modified -> false
  | #Code.status_code -> true

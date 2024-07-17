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

type t = Http.Response.t = {
  headers : Header.t;
  version : Code.version;
  status : Code.status_code;
}
[@@deriving sexp]

let compare { headers; version; status } y =
  match Header.compare headers y.headers with
  | 0 -> (
      match Stdlib.compare status y.status with
      | 0 -> (
          match Stdlib.compare status y.status with
          | 0 -> Code.compare_version version y.version
          | i -> i)
      | i -> i)
  | i -> i

let headers t = t.headers
let encoding t = Header.get_transfer_encoding t.headers
let version t = t.version
let status t = t.status

let make ?(version = `HTTP_1_1) ?(status = `OK) ?(encoding = Transfer.Unknown)
    ?(headers = Header.init ()) () =
  let headers =
    match encoding with
    | Unknown -> (
        match Header.get_transfer_encoding headers with
        | Unknown -> Header.add_transfer_encoding headers Chunked
        | _ -> headers)
    | _ -> Header.add_transfer_encoding headers encoding
  in
  { headers; version; status }

let pp_hum ppf r =
  Format.fprintf ppf "%s" (r |> sexp_of_t |> Sexplib0.Sexp.to_string_hum)

let allowed_body response =
  (* rfc7230#section-5.7.1 *)
  match status response with
  | #Code.informational_status | `No_content | `Not_modified -> false
  | #Code.status_code -> true

let has_body response =
  if allowed_body response then Transfer.has_body (encoding response) else `No

type tt = t

module Make (IO : S.IO) = struct
  type t = tt

  module IO = IO
  module Header_IO = Header_io.Make (IO)
  module Transfer_IO = Transfer_io.Make (IO)

  type reader = Transfer_IO.reader
  type writer = Transfer_IO.writer

  open IO

  let parse_response_fst_line ic =
    let open Code in
    read_line ic >>= function
    | Some response_line -> (
        match String.split_on_char ' ' response_line with
        | version_raw :: code_raw :: _ -> (
            match version_of_string version_raw with
            | (`HTTP_1_0 | `HTTP_1_1) as v ->
                return (`Ok (v, status_of_code (int_of_string code_raw)))
            | `Other _ ->
                return (`Invalid ("Malformed response version: " ^ version_raw))
            )
        | _ ->
            return
              (`Invalid ("Malformed response first line: " ^ response_line)))
    | None -> return `Eof

  let read ic =
    parse_response_fst_line ic >>= function
    | `Eof -> return `Eof
    | `Invalid _reason as r -> return r
    | `Ok (version, status) ->
        Header_IO.parse ic >>= fun headers ->
        return (`Ok { headers; version; status })

  let make_body_reader t ic = Transfer_IO.make_reader (encoding t) ic
  let read_body_chunk = Transfer_IO.read

  let write_header res oc =
    write oc
      (Printf.sprintf "%s %s\r\n"
         (Code.string_of_version res.version)
         (Code.string_of_status res.status))
    >>= fun () ->
    let headers =
      if allowed_body res then
        Header.add_transfer_encoding res.headers (encoding res)
      else res.headers
    in
    Header_IO.write headers oc

  let make_body_writer ~flush t oc =
    Transfer_IO.make_writer ~flush (encoding t) oc

  let write_body = Transfer_IO.write

  let write_footer t oc =
    match encoding t with
    | Transfer.Chunked ->
        (* TODO Trailer header support *)
        IO.write oc "0\r\n\r\n"
    | Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write ~flush fn req oc =
    write_header req oc >>= fun () ->
    let writer = make_body_writer ~flush req oc in
    fn writer >>= fun () -> write_footer req oc
end

module Private = struct
  module Make = Make
end

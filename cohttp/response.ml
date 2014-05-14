(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
 *)

open Sexplib.Std

type t = {
  mutable encoding: Transfer.encoding;
  mutable headers: Header.t;
  mutable version: Code.version;
  mutable status: Code.status_code;
  mutable flush: bool;
} with fields, sexp

let make ?(version=`HTTP_1_1) ?(status=`OK) ?(flush=false) ?(encoding=Transfer.Chunked) ?headers () =
  let headers = match headers with None -> Header.init () |Some h -> h in
  { encoding; headers; version; flush; status }

type tt = t
module Make(IO : S.IO) = struct
  type t = tt
  module IO = IO
  module Header_IO = Header_io.Make(IO)
  module Transfer_IO = Transfer_io.Make(IO)

  open IO

  let parse_response_fst_line ic =
    let open Code in
    read_line ic >>= function
    | Some response_line -> begin
      match Stringext.split response_line ~on:' ' with
      | version_raw :: code_raw :: _ -> begin
         match version_of_string version_raw with
         | Some v -> return (`Ok (v, (status_of_code (int_of_string code_raw))))
         | None -> return (`Invalid ("Malformed response version: " ^ version_raw))
      end
      | _ -> return (`Invalid ("Malformed response first line: " ^ response_line))
    end
    | None -> return `Eof
 
  let read ic =
    parse_response_fst_line ic >>= function
    | `Eof -> return `Eof
    | `Invalid reason as r -> return r
    | `Ok (version, status) ->
       Header_IO.parse ic >>= fun headers ->
       let encoding = Header.get_transfer_encoding headers in
       let flush = false in
       return (`Ok { encoding; headers; version; status; flush })

  let has_body {encoding} = Transfer.has_body encoding
  let read_body_chunk {encoding} ic = Transfer_IO.read encoding ic

  let write_header res oc =
    write oc (Printf.sprintf "%s %s\r\n" (Code.string_of_version res.version) 
      (Code.string_of_status res.status)) >>= fun () ->
    let headers = Header.add_transfer_encoding res.headers res.encoding in
    iter (IO.write oc) (Header.to_lines headers) >>= fun () ->
    IO.write oc "\r\n"

  let write_body {encoding} oc buf =
    Transfer_IO.write encoding oc buf

  let write_footer {encoding} oc =
    match encoding with
    |Transfer.Chunked ->
       (* TODO Trailer header support *)
       IO.write oc "0\r\n\r\n"
    |Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write fn req oc =
    write_header req oc >>= fun () ->
    fn req oc >>= fun () ->
    write_footer req oc

  let is_form req = Header.is_form req.headers
  let read_form req ic = Header_IO.parse_form req.headers ic
end

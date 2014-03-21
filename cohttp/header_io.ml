(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011-2012 Martin Jambon <martin@mjambon.com>
 * Copyright (c) 2010 Mika Illouz
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

module Make(IO : IO.S) = struct
  open IO

  module Transfer_IO = Transfer_io.Make(IO)

  let parse ic =
    (* consume also trailing "^\r\n$" line *)
    let rec parse_headers' headers =
      read_line ic >>= function
      |Some "" | None -> return headers
      |Some line -> begin
          match Stringext.split_header line with
          | [hd;tl] ->
              let header = String.lowercase hd in
              parse_headers' (Header.add headers header tl);
          | _ -> return headers
      end
    in parse_headers' (Header.init ())

  let parse_form headers ic =
    (* If the form is query-encoded, then extract those parameters also *)
    let encoding = Header.get_transfer_encoding headers in
    Transfer_IO.to_string encoding ic >>= fun body ->
    return (Uri.query_of_encoded body)
end

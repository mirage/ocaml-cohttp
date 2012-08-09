(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

module M (IO:IO.M) = struct

  module Parser = Parser.M(IO)
  module Transfer = Transfer.M(IO)
  open IO

  type response = {
    ic: ic;
    encoding: Transfer.encoding;
    headers: (string * string) list;
    version: Code.version;
    status: Code.status_code;
  }

  let parse ic =
    Parser.parse_response_fst_line ic >>= function
    |None -> return None
    |Some (version, status) ->
       Parser.parse_headers ic >>= fun headers ->
       let encoding = Transfer.parse_transfer_encoding headers in
       return (Some { ic; encoding; headers; version; status })

  let version r = r.version
  let status r = r.status
  let body r = Transfer.read r.encoding r.ic
end

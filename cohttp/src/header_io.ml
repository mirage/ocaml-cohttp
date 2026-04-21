(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

let split_header str =
  match Stringext.split ~max:2 ~on:':' str with
  | [ x; y ] -> [ x; String.trim y ]
  | x -> x

module Make (IO : S.IO) = struct
  open IO
  module Transfer_IO = Transfer_io.Make (IO)

  let add_multiline_header multiline_header headers =
    match List.rev multiline_header |> String.concat "" |> split_header with
    | [ hd; tl ] -> Some (Header.add headers hd tl)
    | _ -> None

  let parse ic =
    let malformed_header_stop_parsing = return in
    (* consume also trailing "^\r\n$" line *)
    let rec parse_headers' headers folding_lines =
      read_line ic >>= function
      | Some "" | None -> return (headers, folding_lines)
      | Some line -> (
          match folding_lines, Char.equal line.[0] ' ' || Char.equal line.[0] '\t' with
          | [], true -> malformed_header_stop_parsing (headers, [])
          | [], false -> parse_headers' headers [line]  (* we just started parsing *)
          | _, true ->
            let line_collapse_linear_whitespace = " " ^ (String.trim line) in
            parse_headers' headers (line_collapse_linear_whitespace :: folding_lines)
          | _, false ->
            (* Lines after the first are already prefixed with some number of spaces / tabs. *)
            match add_multiline_header folding_lines headers with
            | Some headers ->
              parse_headers' headers [line];
            | None -> malformed_header_stop_parsing (headers, []))
    in
    parse_headers' (Header.init ()) [] >>= fun (headers_except_last, last_header) ->
    let headers_list =
      match last_header with
       | [] -> headers_except_last
       | _ -> add_multiline_header last_header headers_except_last
                |> Option.value ~default:headers_except_last
    in
    return headers_list

  let write headers oc = IO.write oc (Header.to_string headers)
end

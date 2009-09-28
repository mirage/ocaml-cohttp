
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Printf;;

open Http_common;;
open Http_types;;
open Http_constants;;

let (bindings_sep, binding_sep, pieces_sep, header_sep) =
  (Pcre.regexp "&", Pcre.regexp "=", Pcre.regexp " ", Pcre.regexp ":")
let header_RE = Pcre.regexp "([^:]*):(.*)"

let url_decode url = Netencoding.Url.decode ~plus:true url

let split_query_params query =
  let bindings = Pcre.split ~rex:bindings_sep query in
  match bindings with
  | [] -> raise (Malformed_query query)
  | bindings ->
      List.map
        (fun binding ->
          match Pcre.split ~rex:binding_sep binding with
          | [ ""; b ] -> (* '=b' *)
              raise (Malformed_query_part (binding, query))
          | [ a; b ]  -> (* 'a=b' *) (url_decode a, url_decode b)
          | [ a ]     -> (* 'a=' || 'a' *) (url_decode a, "")
          | _ -> raise (Malformed_query_part (binding, query)))
        bindings

  (** internal, used by generic_input_line *)
exception Line_completed;;

  (** given an input channel and a separator
  @return a line read from it (like Pervasives.input_line)
  line is returned only after reading a separator string; separator string isn't
  included in the returned value
  TODO what about efficiency?, input is performed char-by-char
  *)
let generic_input_line ~sep ~ic =
  let sep_len = String.length sep in
  if sep_len < 1 then
    failwith ("Separator '" ^ sep ^ "' is too short!")
  else  (* valid separator *)
    let line = ref "" in
    let sep_pointer = ref 0 in
    try
      while true do
        if !sep_pointer >= String.length sep then (* line completed *)
          raise Line_completed
        else begin (* incomplete line: need to read more *)
          let ch = input_char ic in
          if ch = String.get sep !sep_pointer then  (* next piece of sep *)
            incr sep_pointer
          else begin  (* useful char *)
            for i = 0 to !sep_pointer - 1 do
              line := !line ^ (String.make 1 (String.get sep i))
            done;
            sep_pointer := 0;
            line := !line ^ (String.make 1 ch)
          end
        end
      done;
      assert false  (* unreacheable statement *)
    with Line_completed -> !line

let patch_empty_path = function "" -> "/" | s -> s
let debug_dump_request path params =
  debug_print
    (sprintf
      "recevied request; path: %s; params: %s"
      path
      (String.concat ", " (List.map (fun (n, v) -> n ^ "=" ^ v) params)))

let parse_request_fst_line ic =
  let request_line = generic_input_line ~sep:crlf ~ic in
  debug_print (sprintf "HTTP request line (not yet parsed): %s" request_line);
  try
    (match Pcre.split ~rex:pieces_sep request_line with
    | [ meth_raw; uri_raw ] ->  (* ancient HTTP request line *)
        (method_of_string meth_raw,                 (* method *)
        Http_parser_sanity.url_of_string uri_raw,   (* uri *)
        None)                                       (* no version given *)
    | [ meth_raw; uri_raw; http_version_raw ] ->  (* HTTP 1.{0,1} *)
          (method_of_string meth_raw,                 (* method *)
          Http_parser_sanity.url_of_string uri_raw,   (* uri *)
          Some (version_of_string http_version_raw))  (* version *)
    | _ -> raise (Malformed_request request_line))
  with Malformed_URL url -> raise (Malformed_request_URI url)

let parse_response_fst_line ic =
  let response_line = generic_input_line ~sep:crlf ~ic in
  debug_print (sprintf "HTTP response line (not yet parsed): %s" response_line);
  try
    (match Pcre.split ~rex:pieces_sep response_line with
    | version_raw :: code_raw :: _ ->
        (version_of_string version_raw,             (* method *)
        status_of_code (int_of_string code_raw))    (* status *)
    | _ -> raise (Malformed_response response_line))
  with
  | Malformed_URL _ | Invalid_code _ | Failure "int_of_string" ->
      raise (Malformed_response response_line)

let parse_path uri = patch_empty_path (String.concat "/" (Neturl.url_path uri))
let parse_query_get_params uri =
  try (* act on HTTP encoded URIs *)
    split_query_params (Neturl.url_query ~encoded:true uri)
  with Not_found -> []

let parse_headers ic =
  (* consume also trailing "^\r\n$" line *)
  let rec parse_headers' headers =
    match generic_input_line ~sep:crlf ~ic with
    | "" -> List.rev headers
    | line ->
        (let subs =
          try
            Pcre.extract ~rex:header_RE line
          with Not_found -> raise (Invalid_header line)
        in
        let header =
          try
            subs.(1)
          with Invalid_argument "Array.get" -> raise (Invalid_header line)
        in
        let value =
          try
            Http_parser_sanity.normalize_header_value subs.(2) 
          with Invalid_argument "Array.get" -> ""
        in
        Http_parser_sanity.heal_header (header, value);
        parse_headers' ((header, value) :: headers))
  in
  parse_headers' []

let parse_request ic =
  let (meth, uri, version) = parse_request_fst_line ic in
  let path = parse_path uri in
  let query_get_params = parse_query_get_params uri in
  debug_dump_request path query_get_params;
  (path, query_get_params)


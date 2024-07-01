(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

include Http.Header

let media_type_re =
  let re = Re.Emacs.re ~case:true "[ \t]*\\([^ \t;]+\\)" in
  Re.(compile (seq [ start; re ]))

let get_first_match s =
  try
    let subs = Re.exec ~pos:0 media_type_re s in
    let start, stop = Re.Group.offset subs 1 in
    Some (String.sub s start (stop - start))
  with Not_found -> None

(* Grab "foo/bar" from " foo/bar ; charset=UTF-8" *)
let get_media_type headers =
  match get headers "content-type" with
  | Some s -> get_first_match s
  | None -> None

let get_acceptable_media_ranges headers =
  Accept.media_ranges (get_multi_concat ~list_value_only:true headers "accept")

let get_acceptable_charsets headers =
  Accept.charsets
    (get_multi_concat ~list_value_only:true headers "accept-charset")

let get_acceptable_encodings headers =
  Accept.encodings
    (get_multi_concat ~list_value_only:true headers "accept-encoding")

let get_acceptable_languages headers =
  Accept.languages
    (get_multi_concat ~list_value_only:true headers "accept-language")

let add_authorization_req headers challenge =
  add headers "www-authenticate" (Auth.string_of_challenge challenge)

let add_authorization headers cred =
  add headers "authorization" (Auth.string_of_credential cred)

let get_authorization headers =
  match get headers "authorization" with
  | None -> None
  | Some v -> Some (Auth.credential_of_string v)

let is_form headers =
  get_media_type headers = Some "application/x-www-form-urlencoded"

let get_location headers =
  match get_location headers with
  | None -> None
  | Some u -> Some (Uri.of_string u)

let get_links headers =
  List.rev
    (List.fold_left
       (fun list link_s -> List.rev_append (Link.of_string link_s) list)
       [] (get_multi headers "link"))

let add_links headers links =
  add_multi headers "link" (List.map Link.to_string links)

let user_agent = Printf.sprintf "ocaml-cohttp/%s" Conf.version

let prepend_user_agent headers user_agent =
  let k = "user-agent" in
  match get headers k with
  | Some ua -> replace headers k (user_agent ^ " " ^ ua)
  | None -> add headers k user_agent

open Sexplib0.Sexp_conv

let sexp_of_t t =
  sexp_of_list (sexp_of_pair sexp_of_string sexp_of_string) (to_list t)

let t_of_sexp s =
  of_list (list_of_sexp (pair_of_sexp string_of_sexp string_of_sexp) s)

let pp_hum ppf h =
  Format.fprintf ppf "%s" (h |> sexp_of_t |> Sexplib0.Sexp.to_string_hum)

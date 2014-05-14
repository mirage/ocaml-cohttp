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

(** HTTP/1.1 response handling *)

(** This contains the metadata for a HTTP/1.1 response header, including
    the {!encoding}, {!headers}, {!version}, {!status} code and whether to
    {!flush} the connection after every body chunk (useful for server-side
    events and other long-lived connection protocols). The body is handled by
    the separate {!S} module type, as it is dependent on the IO 
    implementation. 

    The interface exposes a [fieldslib] interface which provides individual
    accessor functions for each of the records below.  It also provides [sexp]
    serializers to convert to-and-from an {!Core.Std.Sexp.t}. *)
include S.Response
module Make(IO : S.IO) : S.Http_io with type t = t and module IO = IO

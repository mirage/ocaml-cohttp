(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazazagnaire.org>
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

open Lwt

open Net

module Net_IO = struct

  module IO = Cohttp_mirage_io

  let connect_uri uri =
    fail (Failure "not implemented")

  let connect ?ssl ~host ~service () =
    fail (Failure "not implemented")

  let close_in ic = ()
  let close_out ic = ()
  let close ic oc = ignore_result (Channel.close ic)

end

(* Build all the core modules from the [Cohttp_lwt] functors *)
module Request = Cohttp_lwt.Make_request(Cohttp_mirage_io)
module Response = Cohttp_lwt.Make_response(Cohttp_mirage_io)
module Client = Cohttp_lwt.Make_client(Cohttp_mirage_io)(Request)(Response)(Net_IO)
module Server_core = Cohttp_lwt.Make_server(Cohttp_mirage_io)(Request)(Response)(Net_IO)

module type S = sig
  include Cohttp_lwt.Server with module IO = Cohttp_mirage_io
  val listen : ?timeout:float -> Net.Manager.t -> Net.Nettypes.ipv4_src -> t -> unit Lwt.t
end

(* Extend the [Server_core] module with the Mirage-specific
   functions *)
module Server = struct
  include Server_core
  let listen ?timeout mgr src spec =
    (* TODO XXX the cancel-based timeout is almost certainly broken as the
     * thread won't issue a Response *)
    let cb =
      match timeout with
      | None   -> fun dst ch -> callback spec ch ch
      |Some tm ->
        fun dst ch ->
          let tmout = OS.Time.sleep tm in
          let cb_t = callback spec ch ch in
          tmout <?> cb_t
    in
    Net.Channel.listen mgr (`TCPv4 (src, cb))
end

let listen = Server.listen

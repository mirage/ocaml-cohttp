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


module Request = Cohttp_lwt.Make_request(Cohttp_lwt_unix_io)
module Response = Cohttp_lwt.Make_response(Cohttp_lwt_unix_io)

module Client = Cohttp_lwt.Make_client
  (Cohttp_lwt_unix_io)(Request)(Response)(Cohttp_lwt_unix_net)

module Server_core = Cohttp_lwt.Make_server
  (Cohttp_lwt_unix_io)(Request)(Response)(Cohttp_lwt_unix_net)

module Server = struct
  include Server_core
  open Lwt

  let blank_uri = Uri.of_string ""

  let resolve_file ~docroot ~uri =
    (* This normalises the Uri and strips out .. characters *)
    let frag = Uri.path (Uri.resolve "" blank_uri uri) in
    Filename.concat docroot frag

  exception Isnt_a_file
  let respond_file ?headers ~fname () =
    try_lwt
      (* Check this isnt a directory first *)
      lwt () = wrap (fun () ->
       if Unix.((stat fname).st_kind <> S_REG) then raise Isnt_a_file) in
      lwt ic = Lwt_io.open_file ~buffer_size:16384 ~mode:Lwt_io.input fname in
      lwt len = Lwt_io.length ic in
      let encoding = Cohttp.Transfer.Fixed (Int64.to_int len) in
      let count = 16384 in
      let stream = Lwt_stream.from (fun () ->
        try_lwt
          Lwt_io.read ~count ic >|=
             function
             |"" -> None
             |buf -> Some buf
        with
         exn ->
           prerr_endline ("exn: " ^ (Printexc.to_string exn));
           return None
      ) in
      Lwt_stream.on_terminate stream (fun () ->
        ignore_result (Lwt_io.close ic));
      let body = Cohttp_lwt_body.body_of_stream stream in
      let res = Cohttp.Response.make ~status:`OK ~encoding ?headers () in
      return (res, body)
    with
     | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
         respond_not_found ()
     | exn ->
         let body = Printexc.to_string exn in
         respond_error ~status:`Internal_server_error ~body ()

  let create ?timeout ~address ~port spec =
    lwt sockaddr = Cohttp_lwt_unix_net.build_sockaddr address (string_of_int port) in
    Cohttp_lwt_unix_net.Tcp_server.init ~sockaddr ~timeout (callback spec)
end

module type S = sig

  include Cohttp_lwt.Server with module IO = Cohttp_lwt_unix_io

  val resolve_file :
    docroot:string -> uri:Uri.t -> string

  val respond_file :
    ?headers:Cohttp.Header.t ->
    fname:string -> unit ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val create : ?timeout:int -> address:string -> port:int -> t -> unit Lwt.t

end

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

module type C = sig
  include Cohttp_lwt.Client
    with module IO = Cohttp_lwt_unix_io
     and type ctx = Cohttp_lwt_unix_net.ctx
  val custom_ctx: ?ctx:Conduit_lwt_unix.ctx -> ?resolver:Resolver_lwt.t -> unit -> ctx

end

module Client = struct
  include
    Cohttp_lwt.Make_client
      (Cohttp_lwt_unix_io)(Cohttp_lwt_unix_net)

  let custom_ctx = Cohttp_lwt_unix_net.init
end

module Server_core = Cohttp_lwt.Make_server (Cohttp_lwt_unix_io)

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
    Lwt.catch (fun () ->
      (* Check this isnt a directory first *)
      (fname |> Lwt_unix.stat >>= fun s ->
       if Unix.(s.st_kind <> S_REG)
       then fail Isnt_a_file
       else return_unit) >>= fun () ->
      let buffer = Lwt_bytes.create 16384 in
      Lwt_io.open_file ~buffer ~mode:Lwt_io.input fname >>= fun ic ->
      Lwt_io.length ic >>= fun len ->
      let encoding = Cohttp.Transfer.Fixed len in
      let stream = Lwt_stream.from (fun () ->
        Lwt.catch (fun () ->
          Lwt_io.read ~count:16384 ic >|= function
          | "" -> None
          | buf -> Some buf)
          (fun exn ->
             Lwt_log.ign_debug ~exn ("Error resolving file " ^ fname);
             return_none)
      ) in
      Lwt_stream.on_terminate stream (fun () ->
        ignore_result (Lwt_io.close ic));
      let body = Cohttp_lwt_body.of_stream stream in
      let mime_type = Magic_mime.lookup fname in
      let headers = Cohttp.Header.add_opt_unless_exists headers "content-type" mime_type in
      let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
      return (res, body)
    ) (function
      | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
        respond_not_found ()
      | exn ->
        let body = Printexc.to_string exn in
        respond_error ~status:`Internal_server_error ~body ())

  let create ?timeout ?stop ?(ctx=Cohttp_lwt_unix_net.default_ctx) ?(mode=`TCP (`Port 8080)) spec =
    Conduit_lwt_unix.serve ?timeout ?stop ~ctx:ctx.Cohttp_lwt_unix_net.ctx ~mode
      (callback spec)
end

module type S = sig
  include Cohttp_lwt.Server with module IO = Cohttp_lwt_unix_io

  val resolve_file :
    docroot:string -> uri:Uri.t -> string

  val respond_file :
    ?headers:Cohttp.Header.t ->
    fname:string -> unit ->
    (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t

  val create :
    ?timeout:int ->
    ?stop:unit Lwt.t ->
    ?ctx:Cohttp_lwt_unix_net.ctx ->
    ?mode:Conduit_lwt_unix.server -> t -> unit Lwt.t

end

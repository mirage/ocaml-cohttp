(*{{{ Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

module Request = struct
  include Cohttp.Request
  include (Make(Cohttp_lwt_unix_io)
           : module type of Make(Cohttp_lwt_unix_io) with type t := t)
end

module Response = struct
  include Cohttp.Response
  include (Make(Cohttp_lwt_unix_io)
           : module type of Make(Cohttp_lwt_unix_io) with type t := t)
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
      let count = 16384 in
      Lwt_io.open_file
        ~buffer:(Lwt_bytes.create count)
        ~mode:Lwt_io.input fname >>= fun ic ->
      Lwt_io.length ic >>= fun len ->
      let encoding = Cohttp.Transfer.Fixed len in
      let stream = Lwt_stream.from (fun () ->
        Lwt.catch (fun () ->
          Lwt_io.read ~count ic >|= function
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
      let headers = Cohttp.Header.add_opt_unless_exists
                      headers "content-type" mime_type in
      let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
      return (res, body)
    ) (function
      | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
        respond_not_found ()
      | exn -> Lwt.fail exn)

  let create ?timeout ?stop ?on_exn ?(ctx=Cohttp_lwt_unix_net.default_ctx)
        ?(mode=`TCP (`Port 8080)) spec =
    Conduit_lwt_unix.serve ?timeout ?stop ?on_exn ~ctx:ctx.Cohttp_lwt_unix_net.ctx
      ~mode (callback spec)
end

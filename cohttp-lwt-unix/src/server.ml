
module Server_core = Cohttp_lwt.Make_server (Io)

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
      let body = Cohttp_lwt.Body.of_stream stream in
      let mime_type = Magic_mime.lookup fname in
      let headers = Cohttp.Header.add_opt_unless_exists
          headers "content-type" mime_type in
      let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
      return (res, body)
    ) (function
      | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
        respond_not_found ()
      | exn -> Lwt.fail exn)

let create ?timeout ?stop ?on_exn ?(ctx=Net.default_ctx)
    ?(mode=`TCP (`Port 8080)) spec =
  Conduit_lwt_unix.serve ?timeout ?stop ?on_exn ~ctx:ctx.Net.ctx
    ~mode (callback spec)


module Server_core = Cohttp_lwt.Make_server (Io)

include Server_core
open Lwt.Infix

let src = Logs.Src.create "cohttp.lwt.server" ~doc:"Cohttp Lwt server module"
module Log = (val Logs.src_log src : Logs.LOG)

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
       then Lwt.fail Isnt_a_file
       else Lwt.return_unit) >>= fun () ->
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
               Log.debug
                 (fun m -> m "Error resolving file %s (%s)"
                   fname
                   (Printexc.to_string exn));
               Lwt.return_none)
        ) in
      Lwt.on_success (Lwt_stream.closed stream) (fun () ->
        Lwt.ignore_result @@ Lwt.catch
          (fun () -> Lwt_io.close ic)
          (fun e ->
            Log.warn (fun f ->
              f "Closing channel failed: %s" (Printexc.to_string e));
            Lwt.return_unit
          )
      );
      let body = Cohttp_lwt.Body.of_stream stream in
      let mime_type = Magic_mime.lookup fname in
      let headers = Cohttp.Header.add_opt_unless_exists
          headers "content-type" mime_type in
      let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
      Lwt.return (res, body)
    ) (function
      | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
        respond_not_found ()
      | exn -> Lwt.fail exn)

let log_on_exn =
  function
  | Unix.Unix_error (error, func, arg) ->
     Logs.warn (fun m -> m "Client connection error %s: %s(%S)"
       (Unix.error_message error) func arg)
  | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)

let safe error_handler callback spec flow () =
  Lwt.catch
    (fun () -> let ic, oc = Conduit_lwt.io_of_flow flow in callback spec flow ic oc)
    (fun exn -> error_handler exn)

let create
  : type cfg t flow.
     ?timeout:int
  -> ?backlog:int
  -> ?stop:unit Lwt.t
  -> ?on_exn:(exn -> unit)
  -> cfg
  -> (_, flow) Conduit_lwt.protocol
  -> (cfg, t, flow) Conduit_lwt.Service.service
  -> _ -> unit Lwt.t
  = fun ?timeout ?(backlog= 128) ?(stop= fst (Lwt.wait ())) ?(on_exn=log_on_exn)
    cfg protocol service spec ->
    let error_handler exn = on_exn exn ; Lwt.return_unit in
    let cfg : cfg = match Conduit_lwt.Service.equal service Conduit_lwt.TCP.service with
      | Some (Conduit.Refl, _, _) ->
        { cfg with Conduit_lwt.TCP.capacity= backlog }
      | None -> cfg in
    let handler flow =
      let flow = Conduit_lwt.pack protocol flow in
      Lwt.finalize
        (safe error_handler callback spec flow)
        (fun () -> Conduit_lwt.close flow >>= fun _ -> Lwt.return_unit) in
    let cond, run = Conduit_lwt.serve ?timeout ~handler ~service cfg in
    Lwt.pick [ (stop >|= Lwt_condition.signal cond)
             ; run ]

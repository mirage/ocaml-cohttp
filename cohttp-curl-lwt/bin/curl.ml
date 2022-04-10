open Lwt.Syntax
open Cohttp
module Curl = Cohttp_curl_lwt
module Sexp = Sexplib0.Sexp

let src =
  Logs.Src.create "cohttp.lwt.curl" ~doc:"Cohttp Lwt curl implementation"

module Log = (val Logs.src_log src : Logs.LOG)

let client uri ofile meth' =
  Log.debug (fun d -> d "Client with URI %s" (Uri.to_string uri));
  let meth = Cohttp.Code.method_of_string meth' in
  Log.debug (fun d -> d "Client %s issued" meth');
  let reply =
    let context = Curl.Context.create () in
    let request =
      Curl.Request.create ~timeout_ms:5000 meth ~uri:(Uri.to_string uri)
        ~input:Curl.Source.empty ~output:Curl.Sink.string
    in
    Curl.submit context request
  in
  let* resp, response_body =
    Lwt.both (Curl.Response.response reply) (Curl.Response.body reply)
  in
  Format.eprintf "response:%a@.%!" Sexp.pp_hum (Response.sexp_of_t resp);
  let status = Response.status resp in
  Log.debug (fun d ->
      d "Client %s returned: %s" meth' (Code.string_of_status status));
  (match Code.is_success (Code.code_of_status status) with
  | false -> prerr_endline (Code.string_of_status status)
  | true -> ());
  let len = String.length response_body in
  Log.debug (fun d -> d "Client body length: %d" len);
  let output_body c = Lwt_io.write c response_body in
  match ofile with
  | None -> output_body Lwt_io.stdout
  | Some fname -> Lwt_io.with_file ~mode:Lwt_io.output fname output_body

let debug =
  match Sys.getenv_opt "COHTTP_CURL_DEBUG" with None -> false | Some _ -> true

let run_client level ofile uri meth =
  if debug then (
    Fmt_tty.setup_std_outputs ();
    Logs.set_level ~all:true level);
  Lwt_main.run (client uri ofile meth)

open Cmdliner

let uri =
  let loc : Uri.t Arg.conv =
    let parse s =
      try `Ok (Uri.of_string s) with Failure _ -> `Error "unable to parse URI"
    in
    (parse, fun ppf p -> Format.fprintf ppf "%s" (Uri.to_string p))
  in
  Arg.(
    required
    & pos 0 (some loc) None
    & info [] ~docv:"URI"
        ~doc:"string of the remote address (e.g. https://google.com)")

let meth =
  let doc = "Set http method" in
  Arg.(value & opt string "GET" & info [ "X"; "request" ] ~doc)

let verb = Logs_cli.level ()

let ofile =
  let doc = "Output filename to store the URI into." in
  Arg.(value & opt (some string) None & info [ "o" ] ~docv:"FILE" ~doc)

let cmd =
  let info =
    let version = Cohttp.Conf.version in
    let doc = "retrieve a remote URI contents" in
    let man =
      [
        `S "DESCRIPTION";
        `P
          "$(tname) fetches the remote $(i,URI) and prints it to standard \
           output. The output file can also be specified with the $(b,-o) \
           option, and more verbose debugging out obtained via the $(b,-v) \
           option.";
        `S "BUGS";
        `P
          "Report them via e-mail to <mirageos-devel@lists.xenproject.org>, or \
           on the issue tracker at \
           <https://github.com/mirage/ocaml-cohttp/issues>";
        `S "SEE ALSO";
        `P "$(b,curl)(1), $(b,wget)(1)";
      ]
    in
    Cmd.info "cohttp-curl" ~version ~doc ~man
  in

  let term = Term.(const run_client $ verb $ ofile $ uri $ meth) in
  Cmd.v info term

let () = exit @@ Cmd.eval cmd

(*
 * Copyright (c) 2014 Hannes Mehnert
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
open Cohttp
open Cohttp_lwt_unix
module D = Cohttp_lwt_unix_debug

let debug f = if !D.debug_active then f D.debug_print else ()

let assoc_opt k xs =
  try Some (List.assoc k xs)
  with Not_found -> None

let parse_pair s =
  match Stringext.split ~on:'=' ~max:2 s with
  | [k; v] -> (k, v)
  | _ -> invalid_arg "parse_pair"

(* TODO implement @ and < arguments *)
let part_of_argument arg =
  match Stringext.split ~on:';' arg with
  | [] -> invalid_arg "Invalid form argument"
  | namev::opts ->
    let (name, value) = parse_pair namev in
    let body = Cohttp_lwt_body.of_string value in
    let opts = opts |> List.map parse_pair in
    let filename = opts |> assoc_opt "filename" in
    let content_type = opts |> assoc_opt "type" in
    let part = Multipart.create_part ~name ?filename ?content_type () in
    (part, body)

let client uri ofile meth' multiparts =
  debug (fun d -> d "Client with URI %s\n" (Uri.to_string uri));
  let meth = Cohttp.Code.method_of_string meth' in
  debug (fun d -> d "Client %s issued\n" meth');
  (match multiparts with
   | [] -> Client.call meth uri
   | parts ->
     let body = parts |> List.map part_of_argument in
     Client.call_multipart ~body meth uri
  ) >>= fun (resp, body) ->
  let status = Response.status resp in
  debug (fun d -> d "Client GET returned: %s\n" (Code.string_of_status status));
  (* TODO follow redirects *)
  match Code.is_success (Code.code_of_status status) with
  | false ->
    prerr_endline (Code.string_of_status status);
    exit 1
  | true ->
    Cohttp_lwt_body.length body >>= fun (len, body) ->
    debug (fun d -> d "Client body length: %Ld\n" len);
    Cohttp_lwt_body.to_string body >>= fun s ->
    let output_body c =
      Lwt_stream.iter_s (Lwt_io.fprint c) (Cohttp_lwt_body.to_stream body) in
    match ofile with
    | None -> output_body Lwt_io.stdout
    | Some fname -> Lwt_io.with_file ~mode:Lwt_io.output fname output_body

let run_client verbose ofile uri meth multiparts =
  if verbose then (
    Cohttp_lwt_unix_debug.debug_active := true;
    debug (fun d -> d ">>> Debug active");
  );
  Lwt_main.run (client uri ofile meth multiparts)

open Cmdliner

let uri =
  let loc : Uri.t Arg.converter =
    let parse s =
      try `Ok (Uri.of_string s)
      with Failure _ -> `Error "unable to parse URI" in
    parse, fun ppf p -> Format.fprintf ppf "%s" (Uri.to_string p)
  in
  Arg.(required & pos 0 (some loc) None & info [] ~docv:"URI"
   ~doc:"string of the remote address (e.g. https://google.com)")

let multipart =
  let doc = "Provide http body as multipart" in
  Arg.(value & opt_all string [] (info ["F"; "form"] ~doc))

let meth =
  let doc = "Set http method" in
  Arg.(value & opt string "GET" & info ["X"; "request"] ~doc)

let verb =
  let doc = "Display additional debugging to standard error." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let ofile =
  let doc = "Output filename to store the URI into." in
  Arg.(value & opt (some string) None & info ["o"] ~docv:"FILE" ~doc)
 
let cmd =
  let doc = "retrieve a remote URI contents" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) fetches the remote $(i,URI) and prints it to standard output.
        The output file can also be specified with the $(b,-o) option, and more
        verbose debugging out obtained via the $(b,-v) option.";
    `S "BUGS";
    `P "Report them to via e-mail to <mirageos-devel@lists.xenproject.org>, or
        on the issue tracker at <https://github.com/mirage/ocaml-cohttp/issues>";
    `S "SEE ALSO";
    `P "$(b,curl)(1), $(b,wget)(1)" ]
  in
  Term.(pure run_client $ verb $ ofile $ uri $ meth $ multipart),
  Term.info "cohttp-curl" ~version:"1.0.0" ~doc ~man

let () =
  match Term.eval cmd
  with `Error _ -> exit 1 | _ -> exit 0

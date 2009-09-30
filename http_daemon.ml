(*pp camlp4o -I `ocamlfind query lwt.syntax` pa_lwt.cmo *)

(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Printf

open Http_common
open Http_types
open Http_constants
open Http_parser

open Lwt

type daemon_spec = {
  address: string;
  auth: (string * auth_info) option;
  callback: Http_request.request -> Lwt_io.output_channel -> unit Lwt.t;
  port: int;
  root_dir: string option;
  exn_handler: (exn -> Lwt_io.output_channel -> unit Lwt.t) option;
  timeout: int option;
  auto_close: bool;
}

exception Http_daemon_failure of string

  (** send raw data on outchan, flushing it afterwards *)
let send_raw ~data outchan =
  Lwt_io.write outchan data

let send_CRLF = send_raw ~data:crlf

let send_header ~header ~value =
  Http_parser_sanity.heal_header (header, value);
  send_raw ~data:(header ^ ": " ^ value ^ crlf)

let send_headers ~headers outchan =
  Lwt_util.iter_serial (fun (header, value) -> send_header ~header ~value outchan) headers

  (** internal: low level for send_status_line *)
let send_status_line' ~version code =
  let status_line =
    String.concat
      " "
      [ string_of_version version;
      string_of_int code;
      Http_misc.reason_phrase_of_code code ]
  in
  send_raw ~data:(status_line ^ crlf)

let int_of_code = function
  | `Code code -> code
  | `Status status -> code_of_status status

let send_status_line ?(version = http_version) ~(code: status_code) outchan =
  send_status_line' ~version (int_of_code code) outchan

  (* FIXME duplication of code between this and response#addBasicHeaders *)
let send_basic_headers ?(version = http_version) ~(code: status_code) outchan =
  send_status_line' ~version (int_of_code code) outchan >>= fun () ->
  send_headers
    ~headers:["Date", Http_misc.date_822 (); "Server", server_string]
    outchan

  (** internal: given a status code and an additional body return a string
  representing an HTML document that explains the meaning of given status code.
  Additional data can be added to the body via 'body' argument *)
let foo_body code body =
  let reason_phrase = Http_misc.reason_phrase_of_code code in
  sprintf
"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<HTML><HEAD>
<TITLE>%d %s</TITLE>
</HEAD><BODY>
<H1>%d - %s</H1>%s
</BODY></HTML>"
    code reason_phrase code reason_phrase body

  (** internal: send a fooish body explaining in HTML form the 'reason phrase'
  of an HTTP response; body, if given, will be appended to the body *)
let send_foo_body code body = send_raw ~data:(foo_body code body)

  (* Warning: keep default values in sync with Http_response.response class *)
let respond ?(body = "") ?(headers = []) ?version ?(code = `Code 200) (outchan:Lwt_io.output_channel) =
  send_basic_headers ?version ~code outchan >>= fun () ->
  send_headers ~headers outchan >>= fun () ->
  send_header "Content-Length" (string_of_int (String.length body)) outchan >>= fun () ->
  send_CRLF outchan >>= fun () ->
  send_raw ~data:body outchan

  (** internal: low level for respond_redirect, respond_error, ...
  This function send a status line corresponding to a given code, some basic
  headers, the additional headers (if given) and an HTML page containing the
  reason phrase; if body is given it will be included in the body of the HTML
  page *)
let send_empty_response
  func_name ?(is_valid_status = fun _ -> true) ?(headers=[]) ?(body="") () =
    fun ?version code outchan ->
      if not (is_valid_status (int_of_code code)) then
        failwith
          (sprintf "'%d' isn't a valid status code for %s"
            (int_of_code code) func_name)
      else begin  (* status code suitable for answering *)
        let headers =
          [ "Content-Type", "text/html; charset=iso-8859-1" ] @ headers
        in
        let body = (foo_body (int_of_code code) body) ^ body in
        respond ?version ~code ~headers ~body outchan
      end

let respond_redirect
  ~location ?body ?version ?(code = `Code 301) outchan
  =
  send_empty_response "Daemon.respond_redirect" ~is_valid_status:is_redirection
    ~headers:["Location", location] ?body () ?version code outchan

let respond_error ?body ?version ?(code = `Code 400) outchan =
  send_empty_response "Daemon.respond_error" ~is_valid_status:is_error
    ?body () ?version code outchan

let respond_not_found ~url ?version outchan =
  send_empty_response "Daemon.respond_not_found" () ?version (`Code 404) outchan

let respond_forbidden ~url ?version outchan =
  send_empty_response "Daemon.respond_permission_denied" () ?version
  (`Code 403) outchan

let respond_unauthorized ?version ?(realm = server_string) outchan =
  let body =
    sprintf "401 - Unauthorized - Authentication failed for realm \"%s\"" realm
  in
  respond ~headers:["WWW-Authenticate", sprintf "Basic realm=\"%s\"" realm]
    ~code:(`Code 401) ~body outchan

let send_file inchan (outchan:Lwt_io.output_channel) =
  let relay in_ch out_ch =
    let buffer = String.create 8192 in
    let rec relay_rec previous_write =
      Lwt_io.read_into in_ch buffer 0 8192 >>= fun len ->
      if len = 0 then return () else begin
      let write =
        previous_write >>= (fun () ->
          Lwt_io.write_from_exactly out_ch buffer 0 len)
      in
      relay_rec write
      end
    in
    relay_rec (return ())
    in
  relay inchan outchan

let respond_file ~fname ?(version = http_version) (outchan:Lwt_io.output_channel) =
  (** ASSUMPTION: 'fname' doesn't begin with a "/"; it's relative to the current
  document root (usually the daemon's cwd) *)
  let droot = Sys.getcwd () in  (* document root *)
  let path = droot ^ "/" ^ fname in (* full path to the desired file *)
  if not (Sys.file_exists path) then (* file not found *)
    respond_not_found ~url:fname outchan
  else begin
    try
      if Http_misc.is_directory path then begin (* file found, is a dir *)
        respond_forbidden ~url:fname ~version outchan
      end else begin  (* file found, is something else *)
        Lwt_io.with_file ~mode:Lwt_io.input fname 
          (fun inchan ->
             send_basic_headers ~version ~code:(`Code 200) outchan >>= fun () ->
             Lwt_io.file_length fname >>= fun file_size ->
             send_header ~header:"Content-Length" ~value:(Int64.to_string file_size) outchan >>= fun () ->
             send_CRLF outchan >>= fun () ->
             send_file inchan outchan
          )
      end
    with
    | Unix.Unix_error (Unix.EACCES, _, _)
    | Sys_error _ ->
        respond_forbidden ~url:fname ~version outchan
  end

let respond_with (res: Http_response.response) outchan =
  Http_response.serialize res outchan

  (** internal: this exception is raised after a malformed request has been read
  by a serving process to signal main server (or itself if mode = `Single) to
  skip to next request *)
exception Again;;

let pp_parse_exc e =
  sprintf "HTTP request parse error: %s" (Printexc.to_string e)

  (* given a Http_parser.parse_request like function, wrap it in a function that
  do the same and additionally catch parsing exception sending HTTP error
  messages back to client as needed. Returned function raises Again when it
  encounter a parse error (name 'Again' is intended for future versions that
  will support http keep alive signaling that a new request has to be parsed
  from client) *)
let rec wrap_parse_request_w_safety parse_function (inchan:Lwt_io.input_channel) outchan =
  catch (fun () -> parse_function inchan)
  (function
  | (Malformed_request req) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:("request 1st line format should be: " ^
               "'&lt;method&gt; &lt;url&gt; &lt;version&gt;'" ^
               "<br />\nwhile received request 1st line was:<br />\n" ^ req)
        outchan >>= fun () ->
      fail Again
  | (Invalid_HTTP_method meth) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 501)
        ~body:("Method '" ^ meth ^ "' isn't supported (yet)")
        outchan >>= fun () ->
      fail Again
  | (Malformed_request_URI uri) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400) ~body:("Malformed URL: '" ^ uri ^ "'")
        outchan >>= fun () ->
      fail Again
  | (Invalid_HTTP_version version) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 505)
        ~body:("HTTP version '" ^ version ^ "' isn't supported (yet)")
        outchan >>= fun () ->
      fail Again
  | (Malformed_query query) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:(sprintf "Malformed query string '%s'" query) outchan >>= fun () ->
      fail Again
  | (Malformed_query_part (binding, query)) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:(sprintf "Malformed query part '%s' in query '%s'" binding query)
        outchan >>= fun () ->
      fail Again
  | e -> fail e)
  
let chdir_to_document_root = function (* chdir to document root *)
  | Some dir -> Sys.chdir dir
  | None -> ()

  (* TODO what happens when a Quit exception is raised by a callback? Do other
  callbacks keep on living until the end or are them all killed immediatly?
  The right semantics should obviously be the first one *)

  (** - handle HTTP authentication
   *  - handle automatic closures of client connections *)
let invoke_callback (req:Http_request.request) spec (outchan:Lwt_io.output_channel) =
  catch (fun () ->
    match (spec.auth, (Http_request.authorization req)) with
    | None, _ -> spec.callback req outchan  (* no auth required *)
    | Some (realm, `Basic (spec_username, spec_password)),
      Some (`Basic (username, password))
      when (username = spec_username) && (password = spec_password) ->
        (* auth ok *)
        spec.callback req outchan
    | Some (realm, _), _ -> fail (Unauthorized realm)) (* auth failure *)
  (function
  | Unauthorized realm -> respond_unauthorized ~realm outchan
  | Again -> return ()
  | e -> fail e
  )

let main spec =
  chdir_to_document_root spec.root_dir;
  lwt sockaddr = Http_misc.build_sockaddr (spec.address, spec.port) in
  
  let daemon_callback ~clisockaddr ~srvsockaddr inchan outchan =
    let rec loop () =
      catch (fun () -> 
        debug_print "request";
        wrap_parse_request_w_safety (Http_request.init_request ~clisockaddr ~srvsockaddr) 
          inchan outchan >>= fun req ->
        debug_print "invoke_callback";
        invoke_callback req spec outchan >>= fun () ->
        debug_print "loop";
        loop ()
      ) ( function 
         | End_of_file -> debug_print "done with connction"; return ()
         | Canceled -> debug_print "cancelled"; return ()
         | e -> fail e )
    in
    debug_print "server starting";
    catch loop
    (function
     | exn ->
      debug_print (sprintf "uncaught exception: %s" (Printexc.to_string exn));
      (match spec.exn_handler with
      | Some f ->
          debug_print "executing handler";
          f exn outchan
      | None ->
          debug_print "no handler given: re-raising";
          fail exn))
  in
  Http_tcp_server.simple ~sockaddr ~timeout:spec.timeout daemon_callback

module Trivial =
  struct
    let heading_slash_RE = Pcre.regexp "^/"

    let trivial_callback req (outchan:Lwt_io.output_channel) =
      debug_print "trivial_callback";
      let path = Http_request.path req in
      if not (Pcre.pmatch ~rex:heading_slash_RE path) then
        respond_error ~code:(`Code 400) outchan
      else
        respond_file ~fname:(Http_misc.strip_heading_slash path) outchan

    let callback = trivial_callback

    let main spec = main { spec with callback = trivial_callback }
  end

open Http_constants

let default_spec = {
  address = default_addr;
  auth = default_auth;
  auto_close = default_auto_close;
  callback = default_callback;
  port = default_port;
  root_dir = default_root_dir;
  exn_handler = default_exn_handler;
  timeout = default_timeout;
}


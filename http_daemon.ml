
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

exception Http_daemon_failure of string

  (** send raw data on outchan, flushing it afterwards *)
let send_raw ~data outchan =
  output_string outchan data;
  flush outchan

let send_CRLF = send_raw ~data:crlf

let send_header ~header ~value =
  Http_parser_sanity.heal_header (header, value);
  send_raw ~data:(header ^ ": " ^ value ^ crlf)

let send_headers ~headers outchan =
  List.iter (fun (header, value) -> send_header ~header ~value outchan) headers

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
  send_status_line' ~version (int_of_code code) outchan;
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
let respond ?(body = "") ?(headers = []) ?version ?(code = `Code 200) outchan =
  send_basic_headers ?version ~code outchan;
  send_headers ~headers outchan;
  send_header "Content-Length" (string_of_int (String.length body)) outchan;
  send_CRLF outchan;
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

let send_file ~src outchan =
  let buflen = 1024 in
  let buf = String.make buflen ' ' in

  let (file, cleanup) =
    match src with
    | FileSrc fname -> (* if we open the file, we close it before returning *)
        let f = open_in fname in
        f, (fun () -> close_in f)
    | InChanSrc inchan -> inchan, ignore
  in
  try
    while true do
      let bytes = input file buf 0 buflen in
      if bytes = 0 then
        raise End_of_file
      else
        output outchan buf 0 bytes
    done;
    assert false
  with End_of_file ->
    begin
      flush outchan;
      cleanup ()
    end

  (* TODO interface is too ugly to advertise this function in .mli *)
  (** create a minimal HTML directory listing of a given directory and send it
  over an out_channel, directory is passed as a dir_handle; name is the
  directory name, used for pretty printing purposes; path is the opened dir
  path, used to test its contents with stat *)
let send_dir_listing ~dir ~name ~path outchan =
  fprintf outchan "<html>\n<head><title>%s</title></head>\n<body>\n" name;
  let (dirs, files) =
    List.partition (fun e -> Http_misc.is_directory (path ^ e)) (Http_misc.ls dir)
  in
  List.iter
    (fun d -> fprintf outchan "<a href=\"%s/\">%s/</a><br />\n" d d)
    (List.sort compare dirs);
  List.iter
    (fun f -> fprintf outchan "<a href=\"%s\">%s</a><br />\n" f f)
    (List.sort compare files);
  fprintf outchan "</body>\n</html>";
  flush outchan

let respond_file ~fname ?(version = http_version) outchan =
  (** ASSUMPTION: 'fname' doesn't begin with a "/"; it's relative to the current
  document root (usually the daemon's cwd) *)
  let droot = Sys.getcwd () in  (* document root *)
  let path = droot ^ "/" ^ fname in (* full path to the desired file *)
  if not (Sys.file_exists path) then (* file not found *)
    respond_not_found ~url:fname outchan
  else begin
    try
      if Http_misc.is_directory path then begin (* file found, is a dir *)
        let dir = Unix.opendir path in
        send_basic_headers ~version ~code:(`Code 200) outchan;
        send_header "Content-Type" "text/html" outchan;
        send_CRLF outchan;
        send_dir_listing ~dir ~name:fname ~path outchan;
        Unix.closedir dir
      end else begin  (* file found, is something else *)
        let file = open_in fname in
        send_basic_headers ~version ~code:(`Code 200) outchan;
        send_header
          ~header:"Content-Length"
          ~value:(string_of_int (Http_misc.filesize fname))
          outchan;
        send_CRLF outchan;
        send_file ~src:(InChanSrc file) outchan;
        close_in file
      end
    with
    | Unix.Unix_error (Unix.EACCES, _, _)
    | Sys_error _ ->
        respond_forbidden ~url:fname ~version outchan
  end

let respond_with (res: Http_types.response) outchan =
  res#serialize outchan;
  flush outchan

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
let rec wrap_parse_request_w_safety parse_function inchan outchan =
  (try
    parse_function inchan
  with
  | (Malformed_request req) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:("request 1st line format should be: " ^
               "'&lt;method&gt; &lt;url&gt; &lt;version&gt;'" ^
               "<br />\nwhile received request 1st line was:<br />\n" ^ req)
        outchan;
      raise Again
  | (Invalid_HTTP_method meth) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 501)
        ~body:("Method '" ^ meth ^ "' isn't supported (yet)")
        outchan;
      raise Again
  | (Malformed_request_URI uri) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400) ~body:("Malformed URL: '" ^ uri ^ "'")
        outchan;
      raise Again
  | (Invalid_HTTP_version version) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 505)
        ~body:("HTTP version '" ^ version ^ "' isn't supported (yet)")
        outchan;
      raise Again
  | (Malformed_query query) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:(sprintf "Malformed query string '%s'" query) outchan;
      raise Again
  | (Malformed_query_part (binding, query)) as e ->
      debug_print (pp_parse_exc e);
      respond_error ~code:(`Code 400)
        ~body:(sprintf "Malformed query part '%s' in query '%s'" binding query)
        outchan;
      raise Again)

  (* wrapper around Http_parser.parse_request which catch parsing exceptions and
  return error messages to client as needed
  @param inchan in_channel from which read incoming requests
  @param outchan out_channl on which respond with error messages if needed
  *)
let safe_parse_request = wrap_parse_request_w_safety parse_request

  (* as above but for OO version (Http_parser.parse_request') *)
let safe_parse_request' = wrap_parse_request_w_safety (new Http_request.request)

let chdir_to_document_root = function (* chdir to document root *)
  | Some dir -> Sys.chdir dir
  | None -> ()

let server_of_mode = function
  | `Single -> Http_tcp_server.simple
  | `Fork   -> Http_tcp_server.fork
  | `Thread -> Http_tcp_server.thread

  (* TODO what happens when a Quit exception is raised by a callback? Do other
  callbacks keep on living until the end or are them all killed immediatly?
  The right semantics should obviously be the first one *)

  (** - handle HTTP authentication
   *  - handle automatic closures of client connections *)
let invoke_callback req spec outchan =
  let callback req outchan =
    if spec.auto_close then
      Http_misc.finally
        (fun () ->
          (* XXX the pair flush + shutdown is a temporary solution since double
           * close on a socket make ocaml 3.09.2 segfault (see
           * http://caml.inria.fr/mantis/view.php?id=4059). The right thing to
           * do is probably invoke try_close outchan here *)
          flush outchan;
          try
            Unix.shutdown (Unix.descr_of_out_channel outchan) Unix.SHUTDOWN_ALL
          with Unix.Unix_error(_, "shutdown", "") -> ())
        (fun () -> spec.callback req outchan) ()
    else
      spec.callback req outchan in
  try
    (match (spec.auth, req#authorization) with
    | None, _ -> callback req outchan  (* no auth required *)
    | Some (realm, `Basic (spec_username, spec_password)),
      Some (`Basic (username, password))
      when (username = spec_username) && (password = spec_password) ->
        (* auth ok *)
        callback req outchan
    | Some (realm, _), _ -> raise (Unauthorized realm)) (* auth failure *)
  with
  | Unauthorized realm -> respond_unauthorized ~realm outchan
  | Again -> ()

let main spec =
  chdir_to_document_root spec.root_dir;
  let sockaddr = Http_misc.build_sockaddr (spec.address, spec.port) in
  let daemon_callback inchan outchan =
    let next_req () =
      try Some (safe_parse_request' inchan outchan)
      with _ -> None
    in
    let rec loop n =
      match next_req () with
      | Some req ->
          debug_print (sprintf "request #%d" n);
          invoke_callback req spec outchan;
          flush outchan;
          loop (n + 1)
      | None ->
          debug_print "server exiting";
          ()
    in
    debug_print "server starting";
    try loop 1
    with exn ->
      debug_print (sprintf "uncaught exception: %s" (Printexc.to_string exn));
      (match spec.exn_handler with
      | Some f ->
          debug_print "executing handler";
          f exn outchan
      | None ->
          debug_print "no handler given: re-raising";
          raise exn)
  in
  try
    (server_of_mode spec.mode) ~sockaddr ~timeout:spec.timeout daemon_callback 
  with Quit -> ()

module Trivial =
  struct
    let heading_slash_RE = Pcre.regexp "^/"

    let trivial_callback req outchan =
      let path = req#path in
      if not (Pcre.pmatch ~rex:heading_slash_RE path) then
        respond_error ~code:(`Code 400) outchan
      else
        respond_file ~fname:(Http_misc.strip_heading_slash path) outchan

    let callback = trivial_callback

    let main spec = main { spec with callback = trivial_callback }
  end

  (** @param inchan input channel connected to client
     @param outchan output channel connected to client
     @param sockaddr client socket address *)
class connection inchan outchan sockaddr =
  (* ASSUMPTION: inchan and outchan are channels built on top of the same
  Unix.file_descr thus closing one of them will close also the other *)
  let close' o = try o#close with Http_daemon_failure _ -> () in
  object (self)

    initializer Gc.finalise close' self

    val mutable closed = false

    method private assertNotClosed =
      if closed then
        raise (Http_daemon_failure
          "Http_daemon.connection: connection is closed")

    method getRequest =
      self#assertNotClosed;
      try
        Some (safe_parse_request' inchan outchan)
      with _ -> None

    method respond_with res =
      self#assertNotClosed;
      respond_with res outchan

    method close =
      self#assertNotClosed;
      close_in inchan;  (* this close also outchan *)
      closed <- true

  end

class daemon ?(addr = "0.0.0.0") ?(port = 80) () =
  object (self)

    val suck =
      Http_tcp_server.init_socket (Http_misc.build_sockaddr (addr, port))

    method accept =
      let (cli_suck, cli_sockaddr) = Unix.accept suck in  (* may block *)
      let (inchan, outchan) =
        (Unix.in_channel_of_descr cli_suck, Unix.out_channel_of_descr cli_suck)
      in
      new connection inchan outchan cli_sockaddr

    method getRequest =
      let conn = self#accept in
      match conn#getRequest with
      | None ->
          conn#close;
          self#getRequest
      | Some req -> (req, conn)

  end

open Http_constants

let default_spec = {
  address = default_addr;
  auth = default_auth;
  auto_close = default_auto_close;
  callback = default_callback;
  mode = default_mode;
  port = default_port;
  root_dir = default_root_dir;
  exn_handler = default_exn_handler;
  timeout = default_timeout;
}

let daemon_spec
  ?(address = default_addr) ?(auth = default_auth)
  ?(auto_close = default_auto_close)
  ?(callback = default_callback) ?(mode = default_mode) ?(port = default_port)
  ?(root_dir = default_root_dir) ?(exn_handler = default_exn_handler)
  ?(timeout = default_timeout)
  ()
=
  { default_spec with
      address = address;
      auth = auth;
      auto_close = auto_close;
      callback = callback;
      mode = mode;
      port = port;
      root_dir = root_dir;
      exn_handler = exn_handler;
      timeout = timeout;
  }


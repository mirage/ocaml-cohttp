(*
 * OCaml HTTP - do it yourself (fully OCaml) HTTP daemon
 *
 * Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
 * Copyright (C) <2009-2011> Anil Madhavapeddy <anil@recoil.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation, version 2.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *)

open Printf

open Cohttp
open Common
open Types
open Constants
open Parser

open Lwt

module Http_tcp_server = struct
  let backlog = 15

  let try_close chan =
    catch (fun () -> Lwt_io.close chan)
    (function |_ -> return ())

  let init_socket sockaddr =
    let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
    Lwt_unix.bind suck sockaddr;
    Lwt_unix.listen suck backlog;
    suck

  let process_accept ~sockaddr ~timeout callback (client,_) =
    (* client is now connected *)
    let inchan = Lwt_io.of_fd Lwt_io.input client in
    let outchan = Lwt_io.of_fd Lwt_io.output client in
 
    let clisockaddr = Unix.getpeername (Lwt_unix.unix_file_descr client) in
    let srvsockaddr = Unix.getsockname (Lwt_unix.unix_file_descr client) in
 
    let c = callback ~clisockaddr ~srvsockaddr inchan outchan in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t) >> return ()) ] in
    Lwt.pick events >> try_close outchan >> try_close inchan
  
  let simple ~sockaddr ~timeout callback =
    let suck = init_socket sockaddr in
    let rec handle_connection () =
       lwt x = Lwt_unix.accept suck in
       let _ =  process_accept ~sockaddr ~timeout callback x in
       handle_connection()
    in
    handle_connection ()

end

type conn_id = int
let string_of_conn_id = string_of_int

type daemon_spec = {
  address: string;
  callback: conn_id -> Request.request -> string Lwt_stream.t Lwt.t;
  conn_closed : conn_id -> unit;
  port: int;
  root_dir: string option;
  exn_handler: exn -> unit Lwt.t;
  timeout: int option;
  auto_close: bool;
}

exception Http_daemon_failure of string

  (** internal: given a status code and an additional body return a string
  representing an HTML document that explains the meaning of given status code.
  Additional data can be added to the body via 'body' argument *)
let control_body code body =
  let reason_phrase = Common.reason_phrase_of_code code in
  sprintf
"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<HTML><HEAD>
<TITLE>%d %s</TITLE>
</HEAD><BODY>
<H1>%d - %s</H1>%s
</BODY></HTML>"
    code reason_phrase code reason_phrase body

let respond_with response =
  return (Response.serialize_to_stream response)

  (* Warning: keep default values in sync with Http_response.response class *)
let respond ?(body = "") ?(headers = []) ?version ?(status = `Code 200) () =
  let resp = Response.init ~body:[`String body] ~headers ?version ~status () in
  respond_with resp

let respond_control
    func_name ?(is_valid_status = fun _ -> true) ?(headers=[]) ?(body="")
    ?version status =
  let code = match status with `Code c -> c | #status as s -> code_of_status s in
  if is_valid_status code then
    let headers =
      [ "Content-Type", "text/html; charset=iso-8859-1" ] @ headers
    in
    let body = (control_body code body) ^ body in
      respond ?version ~status ~headers ~body ()
  else
    failwith
      (sprintf "'%d' isn't a valid status code for %s" code func_name)
      
let respond_redirect ~location ?body ?version ?(status = `Code 301) () =
  respond_control "Daemon.respond_redirect" ~is_valid_status:is_redirection
    ~headers:["Location", location] ?body ?version status

let respond_error ?body ?version ?(status = `Code 400) () =
  respond_control "Daemon.respond_error" ~is_valid_status:is_error
    ?body ?version status
    
let respond_not_found ~url ?version () =
  respond_control "Daemon.respond_not_found" ?version (`Code 404)
    
let respond_forbidden ~url ?version () =
  respond_control "Daemon.respond_forbidden" ?version (`Code 403)
    
let respond_unauthorized ?version ?(realm = server_string) () =
  let body =
    sprintf "401 - Unauthorized - Authentication failed for realm \"%s\"" realm
  in
    respond ~headers:["WWW-Authenticate", sprintf "Basic realm=\"%s\"" realm]
      ~status:(`Code 401) ~body ()

let respond_file ~fname ?droot ?(version = default_version) 
    ?(mime_type = "application/octet-stream") () =
  (** ASSUMPTION: 'fname' doesn't begin with a "/"; it's relative to the current
      document root (usually the daemon's cwd) *)
  let droot = match droot with
    |None -> Sys.getcwd () |Some d -> d in  (* document root *)
  let path = droot ^ "/" ^ fname in (* full path to the desired file *)
    if not (Sys.file_exists path) then (* file not found *)
      respond_not_found ~url:fname ()
    else begin
      try
	if Sys.is_directory path then (* file found, is a dir *)
          respond_forbidden ~url:fname ~version ()
	else begin (* file found, is something else *)
          lwt inchan = Lwt_io.open_file ~mode:Lwt_io.input path in
          lwt file_size = Lwt_io.file_length path in
          let finished_t, finished_u = Lwt.task () in
          let resp = Response.init ~body:[`Inchan (file_size, inchan, finished_u)] ~status:(`Code 200) ~version () in
          let _ = finished_t >> Lwt_io.close inchan in
	  respond_with resp
      end
      with
	| Unix.Unix_error (Unix.EACCES, _, _)
	| Sys_error _ ->
            respond_forbidden ~url:fname ~version ()
    end
      
  (* given a Http_parser.parse_request like function, wrap it in a function that
  do the same and additionally catch parsing exception sending HTTP error
  messages back to client as needed.
  *)

let handle_parse_exn e =
  let r =
    match e with
      | Malformed_request req ->
          Some
            (`Code 400,
             ("request 1st line format should be: " ^
		"'&lt;method&gt; &lt;url&gt; &lt;version&gt;'" ^
		"<br />\nwhile received request 1st line was:<br />\n" ^ req))
      | Invalid_HTTP_method meth ->
          Some
	    (`Code 501,
             ("Method '" ^ meth ^ "' isn't supported (yet)"))
      | Malformed_request_URI uri ->
          Some
            (`Code 400,
             ("Malformed URL: '" ^ uri ^ "'"))
      | Invalid_HTTP_version version ->
          Some
            (`Code 505,
	     ("HTTP version '" ^ version ^ "' isn't supported (yet)"))
      | Malformed_query query ->
          Some
            (`Code 400,
             (sprintf "Malformed query string '%s'" query))
      | Malformed_query_part (binding, query) ->
	  Some
            (`Code 400,
             (sprintf "Malformed query part '%s' in query '%s'" binding query))
      | _ -> None in

  match r with
    | Some (status, body) ->
        prerr_endline (sprintf "HTTP request parse error: %s" (Printexc.to_string e));
        respond_error ~status ~body ()
    | None ->
        fail e

  (* TODO what happens when a Quit exception is raised by a callback? Do other
  callbacks keep on living until the end or are they all killed immediately?
  The right semantics should obviously be the first one *)

let invoke_callback conn_id (req:Request.request) spec =
  try_lwt 
    spec.callback conn_id req (* no auth required *)
  with e ->
    respond_error ~status:`Internal_server_error ~body:(Printexc.to_string e) ()

let daemon_callback spec =
  let conn_id = ref 0 in
  let daemon_callback ~clisockaddr ~srvsockaddr inchan outchan =
    let conn_id = incr conn_id; !conn_id in

    let streams, push_streams = Lwt_stream.create () in
    let write_streams =
      catch
        (fun () ->
           Lwt_stream.iter_s
             (fun stream -> stream >>= Lwt_stream.iter_s (Lwt_io.write outchan))
             streams)
        (fun _ -> Lwt.return ()) in

    let rec loop () =
      catch (fun () -> 
        let (finished_t, finished_u) = Lwt.wait () in

        let stream =
          try_bind
            (fun () -> Request.init_request ~clisockaddr ~srvsockaddr finished_u inchan)
            (fun req ->
               invoke_callback conn_id req spec)
            (fun e ->
               try_bind
                 (fun () -> handle_parse_exn e)
                 (fun s ->
                    Lwt.wakeup finished_u (); (* read another request *)
                    Lwt.return s)
                 (fun e ->
                    Lwt.wakeup_exn finished_u e;
                    Lwt.fail e)) in
        push_streams (Some stream);
        if spec.auto_close then (
          push_streams None;
          finished_t
        )
        else
          (* wait for request to finish before reading another *)
          finished_t >>= loop
      ) ( function 
         | End_of_file -> prerr_endline "done with connection"; spec.conn_closed conn_id; return ()
         | Canceled -> prerr_endline "cancelled"; spec.conn_closed conn_id; return ()
         | e -> fail e )
    in
    try_lwt
      loop () <&> write_streams
    with
      | exn ->
	  prerr_endline (sprintf "uncaught exception: %s" (Printexc.to_string exn));
          (* XXX perhaps there should be a higher-level exn handler for 500s *)
	  spec.exn_handler exn
  in
  daemon_callback
      
let build_sockaddr (addr, port) =
  try_lwt
      (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
      let hent = Unix.gethostbyname addr in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> failwith ("ocaml-cohttp, cant resolve hostname: " ^ addr)
 
let main spec =
  let () = match spec.root_dir with Some dir -> Sys.chdir dir | None -> () in
  lwt sockaddr = build_sockaddr (spec.address, spec.port) in
  Http_tcp_server.simple ~sockaddr ~timeout:spec.timeout (daemon_callback spec)

module Trivial =
  struct
    let trivial_callback _ req =
      let path = Request.path req in
      if String.length path > 0 && path.[0] = '/' then
        respond_file ~fname:(Misc.strip_heading_slash path) ()
      else
        respond_error ~status:(`Code 400) ()

    let callback = trivial_callback

    let main spec = main { spec with callback = trivial_callback }
  end

let default_callback _ _ = let (s, _) = Lwt_stream.create () in Lwt.return s
let default_exn_handler exn =
  fail exn

let default_conn_closed conn_id = ()

let default_spec = {
  address = "0.0.0.0";
  auto_close = false;
  callback = default_callback;
  conn_closed = default_conn_closed;
  port = 80;
  root_dir = None;
  exn_handler = default_exn_handler;
  timeout = Some 300;
}


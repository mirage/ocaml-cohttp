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

module IO = struct

  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let return = Lwt.return

  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel

  let iter fn x = Lwt_list.iter_s fn x

  let read_line ic = Lwt_io.read_line_opt ic
  let read ic count = 
   try_lwt Lwt_io.read ~count ic
   with End_of_file -> return ""

  let read_exactly ic buf off len =
    try_lwt Lwt_io.read_into_exactly ic buf off len >> return true
    with End_of_file -> return false

  let write oc buf = Lwt_io.write oc buf
  let write_line oc buf = Lwt_io.write_line oc buf
end

module Body  = Transfer.M(IO)
module Parser = Parser.M(IO)
module Request = Request.M(IO)
module Response = Response.M(IO)

open Lwt

let build_sockaddr (addr, port) =
  try_lwt
    (* should this be lwt hent = Lwt_lib.gethostbyname addr ? *)
    let hent = Unix.gethostbyname addr in
    return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), port))
  with _ -> 
    raise_lwt (Failure ("cant resolve hostname: " ^ addr))

let host_of_uri uri = 
  match Uri.host uri with
  |None -> "localhost"
  |Some h -> h

let port_of_uri uri =
  match Uri.port uri with
  |None -> begin
     match Uri.scheme uri with 
     |Some "https" -> 443 (* TODO: actually support https *)
     |Some "http" | Some _ |None -> 80
  end
  |Some p -> p

module Normal = struct

  let connect uri iofn =
    let open Uri in
    let address = host_of_uri uri in
    let port = port_of_uri uri in
    lwt sockaddr = build_sockaddr (address, port) in
    Lwt_io.with_connection sockaddr iofn

end

module SSL = struct

  let sslcontext =
    Ssl.init ();
    Ssl.create_context Ssl.SSLv23 Ssl.Client_context

  let connect uri iofn =
    let address = host_of_uri uri in
    let port = port_of_uri uri in
    lwt sockaddr = build_sockaddr (address, port) in
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sockaddr in
    lwt sock = Lwt_ssl.ssl_connect fd sslcontext in
    let ic = Lwt_ssl.in_channel_of_descr sock in
    let oc = Lwt_ssl.out_channel_of_descr sock in
    try_lwt
      lwt res = iofn (ic,oc) in
      Lwt_ssl.close sock >>
      return res
    with exn ->
      Lwt_ssl.close sock >>
      fail exn
end

let connect uri iofn =
  match Uri.scheme uri with
  |Some "https" -> SSL.connect uri iofn
  |Some "http" -> Normal.connect uri iofn
  |Some _ | None -> fail (Failure "unknown scheme")
 
let rec read_write_r inchan outchan num_read max_to_read =
  lwt s = Lwt_io.read inchan in
  if s = ""  then
    return ()
  else
    lwt () = Lwt_io.write outchan s in
    let num_read_incr = num_read + (String.length s) in
    if num_read_incr < max_to_read then
      read_write_r inchan outchan num_read_incr max_to_read
    else
      return ()

let read_write inchan outchan =
  read_write_r inchan outchan 0 max_int

module Client = struct

  type response = (Response.response * string Lwt_stream.t) option

  let call ?headers ?body meth uri =
    let headers =
      match headers with
      |None -> Header.init ()
      |Some h -> h in
    let encoding = 
      match body with 
      |None -> Transfer.Fixed 0L 
      |Some _ -> Transfer.Chunked in
    let req = Request.make ~meth ~encoding headers uri in
    let mvar = Lwt_mvar.create_empty () in
    let _ = connect uri
      (fun (ic, oc) ->
        lwt () = Request.write
          (match body with
           |None -> (fun _ _ -> return ())
           |Some bs ->
             (fun req oc ->
               Lwt_stream.iter_s (fun b -> Request.write_body b req oc) bs)
          ) req oc in
        match_lwt Response.read ic with
        |None ->
          Lwt_mvar.put mvar None
        |Some res ->
          let body, push_body = Lwt_stream.create () in
          let rec push_t () =
            match_lwt Response.read_body res ic with
            |Transfer.Done -> push_body None; return ()
            |Transfer.Final_chunk buf -> push_body (Some buf); push_body None; return ()
            |Transfer.Chunk buf -> push_body (Some buf); push_t ()
          in
          Lwt_mvar.put mvar (Some (res, body)) >>= push_t
      ) in
    Lwt_mvar.take mvar

   let head ?headers uri = call ?headers `HEAD uri 
   let get ?headers uri = call ?headers `GET uri 
   let delete ?headers uri = call ?headers `DELETE uri 
   let post ?headers ?body uri = call ?headers ?body `POST uri 
   let put ?headers ?body uri = call ?headers ?body `POST uri 

   let post_form ?headers ~params uri =
     let headers =
       match headers with
       |None -> Header.of_list ["content-type","application/x-www-form-urlencoded"]
       |Some h -> Header.add h "content-type" "application/x-www-form-urlencoded"
     in
     let body = Lwt_stream.of_list [(Uri.encoded_of_query (Header.to_list params))] in
     post ~headers ~body uri
end

module Server = struct

  let try_close chan =
    catch (fun () -> Lwt_io.close chan)
    (function |_ -> return ())

  let init_socket sockaddr =
    let suck = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
    Lwt_unix.bind suck sockaddr;
    Lwt_unix.listen suck 15;
    suck

  let process_accept ~sockaddr ~timeout callback (client,_) =
    (* client is now connected *)
    let inchan = Lwt_io.of_fd Lwt_io.input client in
    let outchan = Lwt_io.of_fd Lwt_io.output client in
 
    let c = callback inchan outchan in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t) >> return ()) ] in
    Lwt.pick events >> try_close outchan >> try_close inchan
  
  let tcp_server ~sockaddr ~timeout callback =
    let suck = init_socket sockaddr in
    let rec handle_connection () =
       lwt x = Lwt_unix.accept suck in
       let _ =  process_accept ~sockaddr ~timeout callback x in
       handle_connection()
    in
    handle_connection ()

  type conn_id = int
  type response = Response.response * string Lwt_stream.t
  let string_of_conn_id = string_of_int

  type config = {
    address: string;
    callback: conn_id -> Request.request -> 
      (Response.response * string Lwt_stream.t) Lwt.t;
    conn_closed : conn_id -> unit;
    port: int;
    root_dir: string option;
    timeout: int option;
  }

  let respond_string ?headers ~status ~body () =
    let headers = match headers with |None -> Header.init () |Some h -> h in
    let res = Response.make ~status 
      ~encoding:(Transfer.Fixed (Int64.of_int (String.length body))) headers in
    let body = Lwt_stream.of_list [body] in
    return (res,body)

  let respond_error ~status ~body () =
    respond_string ~status ~body:("Error: "^body) ()

  let invoke_callback conn_id (req:Request.request) spec =
    try_lwt 
      spec.callback conn_id req
    with e ->
      respond_error ~status:`Internal_server_error ~body:(Printexc.to_string e) ()

  let daemon_callback spec =
    let conn_id = ref 0 in
    let daemon_callback ic oc =
      let conn_id = incr conn_id; !conn_id in
      let responses, response_push = Lwt_stream.create () in
      let response_t =
         lwt () = Lwt_stream.iter_s (fun (res, body) ->
             Response.write 
               (fun res oc -> 
                  Lwt_stream.iter_s (fun s ->
                    Response.write_body s res oc) body
               ) res oc
         ) responses in
         try_close oc
      in
      let rec read_request () =
        match_lwt Request.read ic with
        |None -> 
          response_push None;
          spec.conn_closed conn_id;
          return ()
        |Some req -> begin
          lwt (res,body) = invoke_callback conn_id req spec in
          response_push (Some (res,body));
          match Request.header req "connection" with
          |["close"] -> 
             response_push None;
             return ()
          |_ -> 
             read_request ()
        end
      in
      response_t <&> (read_request ())
    in daemon_callback
 
  let main spec =
    let () = match spec.root_dir with Some dir -> Sys.chdir dir | None -> () in
    lwt sockaddr = build_sockaddr (spec.address, spec.port) in
    tcp_server ~sockaddr ~timeout:spec.timeout (daemon_callback spec)
end

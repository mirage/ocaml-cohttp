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

let ic_of_buffer buf = Lwt_io.of_bytes ~mode:Lwt_io.input buf
let oc_of_buffer buf = Lwt_io.of_bytes ~mode:Lwt_io.output buf

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

end

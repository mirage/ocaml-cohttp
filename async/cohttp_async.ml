(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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

open Core.Std
open Async.Std

module IO = struct
  let check_debug norm_fn debug_fn =
    try
      (* XXX why does Async remove getenv? *)
      ignore(Core.Std.Sys.getenv_exn "COHTTP_DEBUG");
      debug_fn
    with Failure _ ->
      norm_fn

  type 'a t = 'a Deferred.t
  let (>>=) = Deferred.(>>=)
  let (>>) m n = m >>= fun _ -> n
  let return = Deferred.return

  type ic = Reader.t
  type oc = Writer.t

  let iter fn x =
    Deferred.List.iter x ~f:fn 

  let read_line =
    check_debug
      (fun ic ->
         Reader.read_line ic >>=
         function
         |`Ok s -> return (Some s)
         |`Eof -> return None
      )
      (fun ic ->
         Reader.read_line ic >>=
         function
         |`Ok s -> Printf.eprintf "<<< %s\n" s; return (Some s)
         |`Eof -> Printf.eprintf "<<<EOF\n"; return None
      )

  let read = 
    let buf = String.create 4096 in
    fun ic len ->
      Reader.read ic ~len buf >>=
      function
      |`Ok len' -> return (String.sub buf 0 len')
      |`Eof -> return ""

  let read_exactly ic len =
    let buf = String.create len in
    Reader.really_read ic ~pos:0 ~len buf >>=
    function
    |`Ok -> return (Some buf)
    |`Eof _ -> return None

  let write =
    check_debug
      (fun oc buf -> Writer.write oc buf; return ())
      (fun oc buf -> Printf.eprintf "\n%4d >>> %s" (Pid.to_int (Unix.getpid ())) buf; Writer.write oc buf; return ())

  let write_line oc buf =
    Writer.write oc buf;
    Writer.write oc "\r\n";
    return ()
end

module Net = struct
  let connect ?interrupt uri =
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    |None -> raise (Failure "Net.connect") (* TODO proper exception *)
    |Some port -> Tcp.connect ?interrupt (Tcp.to_host_and_port host port)
end

module ResIO = Cohttp.Response.Make(IO)
module ReqIO = Cohttp.Request.Make(IO)
open Cohttp

let pipe_of_body read_chunk ic oc =
  let rd,wr = Pipe.create () in
  let rec aux () =
    let open Cohttp.Transfer in
    read_chunk ic
    >>= function
      | Chunk buf ->
        Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
        >>= (function
            | `Closed ->
              Writer.close oc
              >>= fun () ->
              Reader.close ic
            |`Ok _ -> 
              aux ()
          )
      | Final_chunk buf ->
        Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
        >>= (function
            | `Closed ->
              Writer.close oc
              >>= fun () ->
              Reader.close ic
            |`Ok _ -> 
              Pipe.close wr;
              return ()
          )
      | Done ->
        Pipe.close wr;
        Writer.close oc
        >>= fun () ->
        Reader.close ic
  in don't_wait_for (aux ());
  rd

module Client = struct

  let call ?interrupt ?headers ?(chunked=false) ?body meth uri =
    (* Convert the body Pipe to a list of chunks. *)
    (match body with
     | None -> return []
     | Some body -> Pipe.to_list body 
    ) >>= fun body_bufs ->
    (* Figure out an appropriate transfer encoding *)
    let req =
      match body_bufs,chunked with
      | [],true     (* Dont used chunked encoding with an empty body *)
      | _,false ->  (* If we dont want chunked, calculate a content length *)
        let body_length = List.fold ~init:0 ~f:(fun a b -> String.length b + a) body_bufs in
        Request.make_for_client ?headers ~chunked:false ~body_length meth uri 
      | _,true ->   (* Use chunked encoding if there is a body *)
        Request.make_for_client ?headers ~chunked meth uri
    in
    (* Connect to the remote side *)
    Net.connect ?interrupt uri
    >>= fun (_,ic,oc) ->
    (* Write request down the wire *)
    ReqIO.write_header req oc
    >>= fun () ->
    Deferred.List.iter ~f:(fun b -> ReqIO.write_body req oc b) body_bufs
    >>= fun () ->
    ReqIO.write_footer req oc
    >>= fun () ->
    (* Read response *)
    ResIO.read ic
    >>= fun res ->
    let res = Option.value_exn ~message:"Error reading HTTP response" res in
    (* Build a response pipe for the body *)
    let rd = pipe_of_body (ResIO.read_body_chunk res) ic oc in
    return (res, rd)

  let get ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `GET uri
end

module Server = struct
  type ('address, 'listening_on) t = {
    server: ('address, 'listening_on) Tcp.Server.t;
  }

  let close t = Tcp.Server.close t.server
  let close_finished t = Tcp.Server.close_finished t.server
  let is_closed t = Tcp.Server.is_closed t.server

  (* Turn an incoming TCP request into an HTTP request and
     dispatch it to [handle_request] *)
  let handle_client handle_request sock rd wr =
    ReqIO.read rd
    >>= fun req ->
    Option.value_exn ~message:"Error reading HTTP request" req
    |> fun req ->
    (* Create pipe for response body if it exists *)
    let body =
      match ReqIO.has_body req with
      | false -> None
      | true ->
        (* Create a Pipe for the body *)
        let read_chunk = ReqIO.read_body_chunk req in
        Some (pipe_of_body read_chunk rd wr)
    in
    handle_request ?body sock req
    >>= fun response_fn ->
    response_fn wr

  let respond ?body ?headers status wr =
    let headers = Header.add_opt headers "connection" "close" in
    match body with
    | None ->
      let res = ResIO.make ~status ~encoding:(Transfer.Fixed 0) ~headers () in
      ResIO.write_header res wr
      >>= fun () ->
      ResIO.write_footer res wr
      >>= fun () ->
      Writer.close wr
    | Some body ->
      let res = ResIO.make ~status ~encoding:Transfer.Chunked ~headers () in
      ResIO.write_header res wr
      >>= fun () ->
      Pipe.iter body ~f:(ResIO.write_body res wr)
      >>= fun () ->
      ResIO.write_footer res wr
      >>= fun () ->
      Writer.close wr


  let create ?max_connections ?max_pending_connections ?buffer_age_limit ?on_handler_error where_to_listen handle_request =
    Tcp.Server.create ?max_connections ?max_pending_connections ?buffer_age_limit ?on_handler_error where_to_listen (handle_client handle_request)

end

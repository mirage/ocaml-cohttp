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

  let read_line ic =
    Reader.read_line ic >>=
    function
    |`Ok s -> return (Some s)
    |`Eof -> return None

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

module Client = struct

  let call ?interrupt ?headers ?(chunked=false) ?body meth uri =
    (match body with
      | None -> return []
      | Some body -> Pipe.to_list body 
    ) >>= fun body_bufs ->
    let req =
      match body_bufs,chunked with
      | [],true     (* Dont used chunked encoding with an empty body *)
      | _,false ->  (* If we dont want chunked, calculate a content length *)
        let body_length = List.fold ~init:0 ~f:(fun a b -> String.length b + a) body_bufs in
        Request.make_for_client ?headers ~chunked:false ~body_length meth uri 
      | _,true ->   (* Use chunked encoding if there is a body *)
        Request.make_for_client ?headers ~chunked meth uri
    in
    Net.connect ?interrupt uri
    >>= fun (_,ic,oc) ->
    (* Write request down the wire *)
    ReqIO.write_header req oc
    >>= fun () ->
    Deferred.List.iter ~f:(fun b -> ReqIO.write_body req oc b) body_bufs
    >>= fun () ->
    ReqIO.write_footer req oc
    >>= fun () ->
    ResIO.read ic
    >>= function
      | None -> raise (Failure "Error reading HTTP response")
      | Some res ->
        (* Build a response pipe for the body *)
        let rd,wr = Pipe.create () in
        don't_wait_for (
          let rec aux () =
            let open Cohttp.Transfer in
            ResIO.read_body_chunk res ic
            >>= function
              | Done ->
                Pipe.close wr;
                Writer.close oc
                >>= fun () ->
                Reader.close ic
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
          in aux ()
        );
        return (res, rd)
end

module Server = struct

end

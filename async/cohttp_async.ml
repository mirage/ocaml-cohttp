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

module IO = Cohttp_async_io

module Net = struct
  let connect_uri ?interrupt uri =
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    | None -> raise (Failure "Net.connect") (* TODO proper exception *)
    | Some port -> begin
        let mode = 
          match Uri.scheme uri with
          | Some "https" -> `SSL
          | _ -> `TCP in
        Async_conduit.Client.connect ?interrupt ~mode ~host ~port ()
      end
end

module Request = struct
  include Cohttp.Request
  include Cohttp.Request.Make(IO)
end

module Response = struct
  include Cohttp.Response
  include Cohttp.Response.Make(IO)
end

let pipe_of_body read_chunk ic oc =
  let open Cohttp.Transfer in
  let (rd, wr) = Pipe.create () in
  let finished =
    Deferred.repeat_until_finished ()
      (fun () ->
         read_chunk ic
         >>= function
         | Chunk buf ->
           begin
             Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
             >>| function
             | `Closed -> `Finished ()
             | `Ok _ -> `Repeat ()
           end
         | Final_chunk buf ->
           Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
           >>| fun _ -> `Finished ()
         | Done -> return (`Finished ())
      ) in
  don't_wait_for (
    finished >>= fun () ->
    return (Pipe.close wr)
  );
  rd

module Body = struct
  module B = Cohttp.Body
  type t = [
    | B.t
    | `Pipe of string Pipe.Reader.t
  ]
  with sexp_of

  let empty = `Empty
  let of_string s = ((B.of_string s) :> t)
  let of_pipe p = `Pipe p

  let to_string (body:t) =
    match body with
    | #B.t as body -> return (B.to_string body)
    | `Pipe s -> s |> Pipe.to_list >>| String.concat

  let to_pipe = function
    | `Empty -> Pipe.of_list []
    | `String s -> Pipe.of_list [s]
    | `Pipe p -> p

  let transfer_encoding (t:t) =
    match t with
    | #B.t as t -> B.transfer_encoding t
    | `Pipe _ -> Cohttp.Transfer.Chunked

  let of_string_list strings = `Pipe (Pipe.of_list strings)

  let write body response wr =
    match body with
    | `Empty -> return ()
    | `String s -> Response.write_body response wr s
    | `Pipe p ->
      Pipe.iter p ~f:(fun buf ->
          Response.write_body response wr buf
          >>= fun () ->
          match Response.flush response with
          | true -> Writer.flushed wr
          | false -> return ())
end

module Client = struct

  type full_response = (Response.t * Body.t) Deferred.t

  type 'response request =
    ?interrupt:unit Deferred.t
    -> ?headers:Cohttp.Header.t
    -> Uri.t
    -> 'response

  type 'response with_body = ?chunked:bool -> ?body:Body.t -> 'response

  let call ?(chunked=false) ?(body=`Empty) ?interrupt ?headers meth uri =
    (* Convert the body Pipe to a list of chunks. *)
    (match body with
     | `Empty -> return []
     | `String s -> return [s]
     | `Pipe body -> Pipe.to_list body
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
    Net.connect_uri ?interrupt uri
    >>= fun (ic,oc) ->
    (* Write request down the wire *)
    Request.write_header req oc
    >>= fun () ->
    Deferred.List.iter ~f:(fun b -> Request.write_body req oc b) body_bufs
    >>= fun () ->
    Request.write_footer req oc
    >>= fun () ->
    Response.read ic
    >>= function
    | `Eof -> raise (Failure "Connection closed by remote host")
    | `Invalid reason -> raise (Failure reason)
    | `Ok res ->
      (* Build a response pipe for the body *)
      let rd = pipe_of_body (Response.read_body_chunk res) ic oc in
      return (res, `Pipe rd)

  let get ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `GET uri

  let head ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `HEAD uri
    >>= begin fun (res, body) ->
      (match body with
       | `Pipe p -> Pipe.close_read p;
       | _ -> ());
      return res
    end

  let post ?(chunked=false) ?body  ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked ?body `POST uri

  let put ?(chunked=false) ?body ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked ?body `PUT uri

  let patch ?(chunked=false) ?body  ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked ?body `PATCH uri

  let delete ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `DELETE uri
end

module Server = struct

  type ('address, 'listening_on) t = {
    server: ('address, 'listening_on) Tcp.Server.t sexp_opaque;
  } with sexp_of

  type response = Response.t * Body.t with sexp_of

  let close t = Tcp.Server.close t.server
  let close_finished t = Tcp.Server.close_finished t.server
  let is_closed t = Tcp.Server.is_closed t.server

  let read_body req rd wr =
    match Request.has_body req with
    | false -> `Empty
    | true -> (* Create a Pipe for the body *)
      let read_chunk = Request.read_body_chunk req in
      `Pipe (pipe_of_body read_chunk rd wr)

  let handle_client handle_request sock rd wr =
    let requests_pipe =
      Reader.read_all rd (fun rd ->
          Request.read rd
          >>| function
          | `Eof | `Invalid _ -> `Eof
          | `Ok req ->
            let body = read_body req rd wr in
            if not (Request.is_keep_alive req)
            then don't_wait_for (Reader.close rd);
            `Ok (req, body)
        ) in
    Pipe.iter requests_pipe ~f:(fun (req, body) ->
        handle_request ~body sock req >>= fun (res, body) ->
        let keep_alive = Request.is_keep_alive req in
        let res =
          let headers = Cohttp.Header.add
              (Cohttp.Response.headers res)
              "connection"
              (if keep_alive then "keep-alive" else "close") in
          { res with Response.headers } in
        Response.write_header res wr >>= fun () ->
        Body.write body res wr >>= fun () ->
        Response.write_footer res wr
      )
    >>= fun () -> Writer.close wr
    >>= fun () -> Reader.close rd

  let respond ?(flush=false) ?(headers=Cohttp.Header.init ())
      ?(body=`Empty) status : response Deferred.t =
    let encoding =
      let open Cohttp.Transfer in
      match body with
      | `Empty -> Fixed 0
      | `String s -> Fixed (String.length s)
      | `Pipe p -> Chunked in
    let resp = Response.make ~status ~flush ~encoding ~headers () in
    return (resp, body)

  let respond_with_pipe ?flush ?headers ?(code=`OK) body =
    respond ?flush ?headers ~body:(`Pipe body) code

  let respond_with_string ?flush ?headers ?(code=`OK) body =
    respond ?flush ?headers ~body:(`String body) code

  let respond_with_redirect ?headers uri =
    let headers = Cohttp.Header.add_opt headers
        "location" (Uri.to_string uri) in
    respond ~flush:false ~headers `Found

  let resolve_local_file ~docroot ~uri =
    (* This normalises the Uri and strips out .. characters *)
    Uri.path (Uri.resolve "" (Uri.of_string "") uri)
    |> Filename.concat docroot

  let error_body_default =
    "<html><body><h1>404 Not Found</h1></body></html>"

  let respond_with_file ?flush ?headers ?(error_body=error_body_default) filename =
    Monitor.try_with ~run:`Now
      (fun () ->
         Reader.open_file filename
         >>= fun rd ->
         let body = `Pipe (Reader.pipe rd) in
         respond ?flush ?headers ~body `OK
      )
    >>= function
    |Ok res -> return res
    |Error exn -> respond_with_string ~code:`Not_found error_body

  let create ?max_connections ?max_pending_connections
      ?buffer_age_limit ?on_handler_error ?(mode=`TCP) where_to_listen handle_request =
    Async_conduit.Server.create ?max_connections ?max_pending_connections
      ?buffer_age_limit ?on_handler_error mode
      where_to_listen (handle_client handle_request)
    >>| fun server ->
    { server }

end

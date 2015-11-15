(*{{{ Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

open Core.Std
open Async.Std
open Cohttp

module IO = Cohttp_async_io

module Net = struct

  let lookup uri =
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    | None -> Deferred.Or_error.error_string
                "Net.lookup: failed to get TCP port form Uri"
    | Some port ->
      let open Unix in
      Addr_info.get ~host [ Addr_info.AI_FAMILY PF_INET
                          ; Addr_info.AI_SOCKTYPE SOCK_STREAM]
      >>| function
      | { Addr_info.ai_addr=ADDR_INET (addr,_) }::_ ->
        Or_error.return (host, Ipaddr_unix.of_inet_addr addr, port)
      | _ -> Or_error.error "Failed to resolve Uri" uri Uri.sexp_of_t

  let connect_uri ?interrupt uri =
    lookup uri
    |> Deferred.Or_error.ok_exn
    >>= fun (host, addr, port) ->
    let mode =
      match Uri.scheme uri with
      | Some "https" -> `OpenSSL (host, addr, port)
      | Some "httpunix" -> `Unix_domain_socket host
      | _ -> `TCP (addr, port)
    in
    Conduit_async.connect ?interrupt mode
end

module Request = struct
  include Cohttp.Request
  include (Make(IO) : module type of Make(IO) with type t := t)
end

module Response = struct
  include Cohttp.Response
  include (Make(IO) : module type of Make(IO) with type t := t)
end

let pipe_of_body read_chunk ic =
  let open Cohttp.Transfer in
  Pipe.init (fun writer ->
    Deferred.repeat_until_finished () (fun () ->
      read_chunk ic >>= function
      | Chunk buf ->
        (* Even if [writer] has been closed, the loop must continue reading
         * from the input channel to ensure that it is left in a proper state
         * for the next request to be processed (in the case of keep-alive).
         *
         * The only case where [writer] will be closed is when
         * [Pipe.close_read] has been called on its read end. This could be
         * done by a request handler to signal that it does not need to
         * inspect the remainder of the body to fulfill the request.
        *)
        Pipe.write_when_ready writer ~f:(fun write -> write buf)
        >>| fun _ -> `Repeat ()
      | Final_chunk buf ->
        Pipe.write_when_ready writer ~f:(fun write -> write buf)
        >>| fun _ -> `Finished ()
      | Done -> return (`Finished ())))

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

  let to_string = function
    | #B.t as body -> return (B.to_string body)
    | `Pipe s -> Pipe.to_list s >>| String.concat

  let to_string_list = function
    | #B.t as body -> return (B.to_string_list body)
    | `Pipe s -> Pipe.to_list s

  let drain = function
    | #B.t -> return ()
    | `Pipe p -> Pipe.drain p

  let is_empty (body:t) =
    match body with
    | #B.t as body -> return (B.is_empty body)
    | `Pipe s ->
      Pipe.values_available s
      >>| function
      |`Eof -> false
      |`Ok ->
        match Pipe.peek s with
        | Some "" -> true
        | Some _ | None -> false

  let to_pipe = function
    | `Empty -> Pipe.of_list []
    | `String s -> Pipe.of_list [s]
    | `Strings sl -> Pipe.of_list sl
    | `Pipe p -> p

  let disable_chunked_encoding = function
    | #B.t as body -> return (body, B.length body)
    | `Pipe s ->
      Pipe.to_list s >>| fun l ->
      let body = `Strings l in
      let len = B.length body in
      body, len

  let transfer_encoding = function
    | #B.t as t -> B.transfer_encoding t
    | `Pipe _ -> Cohttp.Transfer.Chunked

  let of_string_list strings = `Pipe (Pipe.of_list strings)

  let write write_body body writer =
    match body with
    | `Empty -> return ()
    | `String s -> write_body writer s
    | `Strings sl -> Deferred.List.iter sl ~f:(write_body writer)
    | `Pipe p -> Pipe.iter p ~f:(write_body writer)

  let map t ~f =
    match t with
    | #Body.t as t -> (B.map f t :> t)
    | `Pipe p -> `Pipe (Pipe.map p ~f)

  let as_pipe t ~f = `Pipe (t |> to_pipe |> f)
end

module Client = struct

  let read_request ic =
    Response.read ic >>| function
    | `Eof -> failwith "Connection closed by remote host"
    | `Invalid reason -> failwith reason
    | `Ok res ->
      (* Build a response pipe for the body *)
      let reader = Response.make_body_reader res ic in
      let pipe = pipe_of_body Response.read_body_chunk reader in
      (res, pipe)

  let request ?interrupt ?(body=`Empty) req =
    (* Connect to the remote side *)
    Net.connect_uri ?interrupt req.Request.uri
    >>= fun (ic,oc) ->
    Request.write (fun writer -> Body.write Request.write_body body writer) req oc
    >>= fun () ->
    read_request ic >>| fun (resp, body) ->
    don't_wait_for (
      Pipe.closed body >>= fun () ->
      Deferred.all_ignore [Reader.close ic; Writer.close oc]);
    (resp, `Pipe body)

  let callv ?interrupt uri reqs =
    let reqs_c = ref 0 in
    let resp_c = ref 0 in
    Net.connect_uri ?interrupt uri >>| fun (ic, oc) ->
    reqs
    |> Pipe.iter ~f:(fun (req, body) ->
      incr reqs_c;
      Request.write (fun writer -> Body.write Request.write_body body writer)
        req oc)
    |> don't_wait_for;
    let last_body_drained = ref Deferred.unit in
    let responses = Reader.read_all ic (fun ic ->
      !last_body_drained >>= fun () ->
      if Pipe.is_closed reqs && (!resp_c >= !reqs_c) then
        return `Eof
      else
        ic |> read_request >>| fun (resp, body) ->
        incr resp_c;
        last_body_drained := Pipe.closed body;
        `Ok (resp, `Pipe body)
    ) in
    don't_wait_for (
      Pipe.closed reqs >>= fun () ->
      Pipe.closed responses >>= fun () ->
      Writer.close oc
    );
    responses

  let call ?interrupt ?headers ?(chunked=false) ?(body=`Empty) meth uri =
    (* Create a request, then make the request. Figure out an appropriate
       transfer encoding *)
    let req =
      match chunked with
      | false ->
        Body.disable_chunked_encoding body >>| fun (body, body_length) ->
        Request.make_for_client ?headers ~chunked ~body_length meth uri
      | true -> begin
          Body.is_empty body >>| function
          | true -> (* Don't used chunked encoding with an empty body *)
            Request.make_for_client ?headers ~chunked:false ~body_length:0L meth uri
          | false -> (* Use chunked encoding if there is a body *)
            Request.make_for_client ?headers ~chunked:true meth uri
        end
    in
    req >>= request ?interrupt ~body

  let get ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `GET uri

  let head ?interrupt ?headers uri =
    call ?interrupt ?headers ~chunked:false `HEAD uri
    >>| fun (res, body) ->
    (match body with
     | `Pipe p -> Pipe.close_read p;
     | _ -> ());
    res

  let post ?interrupt ?headers ?(chunked=false) ?body uri =
    call ?interrupt ?headers ~chunked ?body `POST uri

  let post_form ?interrupt ?headers ~params uri =
    let headers = Cohttp.Header.add_opt_unless_exists headers
        "content-type" "application/x-www-form-urlencoded" in
    let body = Body.of_string (Uri.encoded_of_query params) in
    post ?interrupt ~headers ~chunked:false ~body uri

  let put ?interrupt ?headers ?(chunked=false) ?body uri =
    call ?interrupt ?headers ~chunked ?body `PUT uri

  let patch ?interrupt ?headers ?(chunked=false) ?body uri =
    call ?interrupt ?headers ~chunked ?body `PATCH uri

  let delete ?interrupt ?headers ?(chunked=false) ?body uri =
    call ?interrupt ?headers ~chunked ?body `DELETE uri
end

module Server = struct

  type ('address, 'listening_on) t = {
    server: ('address, 'listening_on) Tcp.Server.t sexp_opaque;
  } with sexp_of

  type response = Response.t * Body.t with sexp_of

  let close t = Tcp.Server.close t.server
  let close_finished t = Tcp.Server.close_finished t.server
  let is_closed t = Tcp.Server.is_closed t.server

  let read_body req rd =
    match Request.has_body req with
    (* TODO maybe attempt to read body *)
    | `No | `Unknown -> (`Empty, Deferred.unit)
    | `Yes -> (* Create a Pipe for the body *)
      let reader = Request.make_body_reader req rd in
      let pipe = pipe_of_body Request.read_body_chunk reader in
      (`Pipe pipe, Pipe.closed pipe)

  let handle_client handle_request sock rd wr =
    let last_body_pipe_drained = ref Deferred.unit in
    let requests_pipe =
      Reader.read_all rd (fun rd ->
        !last_body_pipe_drained >>= fun () ->
        Request.read rd >>| function
        | `Eof | `Invalid _ -> `Eof
        | `Ok req ->
          let body, finished = read_body req rd in
          last_body_pipe_drained := finished;
          `Ok (req, body)
      ) in
    Pipe.iter requests_pipe ~f:(fun (req, body) ->
      handle_request ~body sock req
      >>= fun (res, res_body) ->
      let keep_alive = Request.is_keep_alive req in
      let flush = Response.flush res in
      let res =
        let headers = Cohttp.Header.add_unless_exists
                        (Cohttp.Response.headers res)
                        "connection"
                        (if keep_alive then "keep-alive" else "close") in
        { res with Response.headers } in
      Response.write ~flush (Body.write Response.write_body res_body) res wr
      >>= fun () ->
      Writer.(if keep_alive then flushed else close ?force_close:None) wr
      >>= fun () ->
      Body.drain body
    ) >>= fun () ->
    Writer.close wr >>= fun () ->
    Reader.close rd

  let respond ?(flush=true) ?(headers=Cohttp.Header.init ())
        ?(body=`Empty) status : response Deferred.t =
    let encoding = Body.transfer_encoding body in
    let resp = Response.make ~status ~flush ~encoding ~headers () in
    return (resp, body)

  let respond_with_pipe ?flush ?headers ?(code=`OK) body =
    respond ?flush ?headers ~body:(`Pipe body) code

  let respond_with_string ?flush ?headers ?(code=`OK) body =
    respond ?flush ?headers ~body:(`String body) code

  let respond_with_redirect ?headers uri =
    let headers = Cohttp.Header.add_opt_unless_exists headers "location" (Uri.to_string uri) in
    respond ~flush:false ~headers `Found

  let resolve_local_file ~docroot ~uri =
    (* This normalises the Uri and strips out .. characters *)
    Uri.(pct_decode (path (resolve "" (of_string "/") uri)))
    |> Filename.concat docroot

  let error_body_default =
    "<html><body><h1>404 Not Found</h1></body></html>"

  let respond_with_file ?flush ?headers ?(error_body=error_body_default) filename =
    Monitor.try_with ~run:`Now
      (fun () ->
         Reader.open_file filename
         >>= fun rd ->
         let body = `Pipe (Reader.pipe rd) in
         let mime_type = Magic_mime.lookup filename in
         let headers = Cohttp.Header.add_opt_unless_exists headers "content-type" mime_type in
         respond ?flush ~headers ~body `OK
      )
    >>= function
    |Ok res -> return res
    |Error exn -> respond_with_string ~code:`Not_found error_body

  let create ?max_connections ?max_pending_connections
        ?buffer_age_limit ?on_handler_error ?(mode=`TCP) where_to_listen handle_request =
    Conduit_async.serve ?max_connections ?max_pending_connections
      ?buffer_age_limit ?on_handler_error mode
      where_to_listen (handle_client handle_request)
    >>| fun server ->
    { server }

end

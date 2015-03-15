(*
 * Copyright (c) 2014 Andy Ray
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

module C = Cohttp
module CLB = Cohttp_lwt_body

module type Params = sig
  val chunked_response : bool
  val chunk_size : int
  val convert_body_string : Js.js_string Js.t -> string
end

module Body_builder(P : Params) = struct

  (* perform the body transfer in chunks. *)
  let chunked_body text =
    let body_len = text##length in
    let pos = ref 0 in
    let chunkerizer () =
      if !pos = body_len then
        Lwt.return C.Transfer.Done
      else
        if !pos + P.chunk_size >= body_len then begin
          let str = text##substring_toEnd(!pos) in
          pos := body_len;
          Lwt.return (C.Transfer.Final_chunk (P.convert_body_string str))
        end else begin
          let str = text##substring(!pos, !pos+P.chunk_size) in
          pos := !pos + P.chunk_size;
          Lwt.return (C.Transfer.Chunk (P.convert_body_string str))
        end
    in
    if body_len=0 then CLB.empty
    else CLB.of_stream (CLB.create_stream chunkerizer ())

  (* choose between chunked and direct transfer *)
  let get text =
    if P.chunked_response then chunked_body text
    else CLB.of_string (P.convert_body_string text)

end

module Make_api(X : sig

  module IO : S.IO
  module Request : Cohttp_lwt.Request
  module Response : Cohttp_lwt.Response

  val call :
      ?headers:Cohttp.Header.t ->
      ?body:Cohttp_lwt_body.t ->
      Cohttp.Code.meth ->
      Uri.t -> (Response.t * Cohttp_lwt_body.t) Lwt.t

end) = struct

  module IO = X.IO
  module Request = X.Request
  module Response = X.Response

  let default_ctx = ()
  type ctx = unit
  let sexp_of_ctx _ = Sexplib.Sexp.List []
  let close_in _ = ()
  let close_out _ = ()
  let close _ _ = ()

  let call ?ctx ?headers ?body ?chunked meth uri = X.call ?headers ?body meth uri

  (* The HEAD should not have a response body *)
  let head ?ctx ?headers uri =
    let open Lwt in
    call ?ctx ?headers ~chunked:false `HEAD uri
    >|= fst

  let get ?ctx ?headers uri = call ?ctx ?headers ~chunked:false `GET uri
  let delete ?ctx ?headers uri = call ?ctx ?headers ~chunked:false `DELETE uri
  let post ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `POST uri
  let put ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `PUT uri
  let patch ?ctx ?body ?chunked ?headers uri = call ?ctx ?headers ?body ?chunked `PATCH uri

  let post_form ?ctx ?headers ~params uri =
    let headers = C.Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
    let body = Cohttp_lwt_body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  (* No implementation (can it be done?).  What should the failure exception be? *)
  exception Cohttp_lwt_xhr_callv_not_implemented
  let callv ?ctx uri reqs = Lwt.fail Cohttp_lwt_xhr_callv_not_implemented (* ??? *)

end

module Make_client_async(P : Params) = Make_api(struct

  module IO = String_io_lwt
  module Response = Cohttp_lwt.Make_response(IO)
  module Request = Cohttp_lwt.Make_request(IO)
  module Header_io = Cohttp.Header_io.Make(IO)
  module Bb = Body_builder(P)

  let call ?headers ?body meth uri =
    let xml = XmlHttpRequest.create () in
    let (res : (Response.t Lwt.t * CLB.t) Lwt.t), wake = Lwt.task () in
    let () = xml##_open(Js.string (C.Code.string_of_method meth),
                        Js.string (Uri.to_string uri),
                        Js._true) (* asynchronous call *)
    in
    (* set request headers *)
    let () =
        match headers with
        | None -> ()
        | Some(headers) ->
          C.Header.iter
            (fun k v ->
              (* some headers lead to errors in the javascript console, should
                  we filter then out here? *)
              List.iter
                (fun v -> xml##setRequestHeader(Js.string k, Js.string v)) v)
            headers
    in

    xml##onreadystatechange <- Js.wrap_callback
      (fun _ ->
        match xml##readyState with
        | XmlHttpRequest.DONE -> begin
          (* construct body *)
          let body = Bb.get xml##responseText in
          (* (re-)construct the response *)
          let response =
            let resp_headers = Js.to_string (xml##getAllResponseHeaders()) in
            let channel = C.String_io.open_in resp_headers in
            Lwt.(Header_io.parse channel >>= fun resp_headers ->
              Lwt.return (Response.make
                            ~version:`HTTP_1_1
                            ~status:(C.Code.status_of_code xml##status)
                            ~flush:false (* ??? *)
                            ~encoding:(CLB.transfer_encoding body)
                            ~headers:resp_headers
                            ()))
          in
          (* Note; a type checker subversion seems to be possible here (4.01.0).
            * Remove the type constraint on Lwt.task above and return any old
            * guff here.  It'll compile and crash in the browser! *)
          Lwt.wakeup wake (response, body)
        end
        | _ -> ()
      );

    (* perform call *)
    lwt () =
      match body with
      | None -> Lwt.return (xml##send(Js.null))
      | Some(body) ->
        lwt body = CLB.to_string body in
        Lwt.return (xml##send(Js.Opt.return (Js.string body)))
    in
    Lwt.on_cancel res (fun () -> xml##abort ());

    (* unwrap the response *)
    Lwt.(res >>= fun (r, b) -> r >>= fun r -> Lwt.return (r,b))

end)

module Make_client_sync(P : Params) = Make_api(struct

  module IO = String_io_lwt
  module Response = Cohttp_lwt.Make_response(IO)
  module Request = Cohttp_lwt.Make_request(IO)
  module Header_io = Cohttp.Header_io.Make(IO)
  module Bb = Body_builder(P)

  let call ?headers ?body meth uri =
    let xml = XmlHttpRequest.create () in
    let () = xml##_open(Js.string (C.Code.string_of_method meth),
                        Js.string (Uri.to_string uri),
                        Js._false)  (* synchronous call *)
    in
    (* set request headers *)
    let () =
        match headers with
        | None -> ()
        | Some(headers) ->
          C.Header.iter
            (fun k v -> List.iter
              (* some headers lead to errors in the javascript console, should
                  we filter then out here? *)
              (fun v ->
                xml##setRequestHeader(Js.string k, Js.string v)) v)
            headers
    in
    (* perform call *)
    lwt () =
      match body with
      | None -> Lwt.return (xml##send(Js.null))
      | Some(body) ->
        lwt body = CLB.to_string body in
        Lwt.return (xml##send(Js.Opt.return (Js.string body)))
    in

    (* construct body *)
    let body = Bb.get xml##responseText in

    (* (re-)construct the response *)
    lwt resp_headers =
      let resp_headers = Js.to_string (xml##getAllResponseHeaders()) in
      let resp_headers = Header_io.parse (C.String_io.open_in resp_headers) in
      resp_headers
    in

    let response = Response.make
      ~version:`HTTP_1_1
      ~status:(Cohttp.Code.status_of_code xml##status)
      ~flush:false
      ~encoding:(CLB.transfer_encoding body)
      ~headers:resp_headers
      ()
    in

    Lwt.return (response,body)

end)

module Client = Make_client_async(struct
  let chunked_response = true
  let chunk_size = 128 * 1024
  let convert_body_string = Js.to_bytestring
end)

module Client_sync = Make_client_sync(struct
  let chunked_response = false
  let chunk_size = 0
  let convert_body_string = Js.to_bytestring
end)

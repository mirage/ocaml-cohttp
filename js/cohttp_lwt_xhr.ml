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


module String_io_lwt = struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.bind
  
  type ic = Cohttp.String_io.M.ic
  type oc = Cohttp.String_io.M.oc
  type conn = Cohttp.String_io.M.conn

  let iter = Lwt_list.iter_s
  let read_line ic = return (Cohttp.String_io.M.read_line ic)
  let read ic n = return (Cohttp.String_io.M.read ic n)
  let read_exactly ic n = return (Cohttp.String_io.M.read_exactly ic n)

  let write oc str = return (Cohttp.String_io.M.write oc str)
  let flush oc = return (Cohttp.String_io.M.flush oc)
end

module Client_async = struct

    module C = Cohttp
    module CLB = Cohttp_lwt_body

    module IO = String_io_lwt
    module Response = Cohttp_lwt.Make_response(String_io_lwt)
    module Request = Cohttp_lwt.Make_request(String_io_lwt)
    module Header_io = Cohttp.Header_io.Make(String_io_lwt)

    let default_ctx = ()
    type ctx = unit

    (* perform the body transfer in chunks.
     * not sure this is the correct interpretation of a 
     * chucked transfer encoding! *)
    let chunked_body chunk_size text = 
      let body_len = text##length in
      let pos = ref 0 in
      let chunkerizer () =
        if !pos = body_len then 
          Lwt.return C.Transfer.Done
        else
          if !pos + chunk_size >= body_len then begin
            let str = text##substring_toEnd(!pos) in
            pos := body_len;
            Lwt.return (C.Transfer.Final_chunk (Js.to_string str))
          end else begin
            let str = text##substring(!pos, !pos+chunk_size) in
            pos := !pos + chunk_size;
            Lwt.return (C.Transfer.Chunk (Js.to_string str))
          end
      in
      if body_len=0 then CLB.empty
      else CLB.of_stream (CLB.create_stream chunkerizer ()) 
  
    let call ?ctx ?headers ?body ?chunked meth uri = 

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
            let body_chunked = false in (* ??? *)
            let body = 
              if body_chunked then chunked_body 1024 xml##responseText
              else CLB.of_string (Js.to_string xml##responseText) 
            in
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
      let q = List.map (fun (k,v) -> k, [v]) (C.Header.to_list params) in
      let body = Cohttp_lwt_body.of_string (Uri.encoded_of_query q) in
      post ?ctx ~chunked:false ~headers ~body uri

    (* No implementation (can it be done?).  What should the failure exception be? *)
    exception Cohttp_lwt_xhr_callv_not_implemented
    let callv ?ctx uri reqs = Lwt.fail Cohttp_lwt_xhr_callv_not_implemented (* ??? *)
        
end

module Client_sync = struct

    module C = Cohttp
    module CLB = Cohttp_lwt_body

    module IO = C.String_io.M
    module Response = Cohttp_lwt.Make_response(C.String_io.M)
    module Request = Cohttp_lwt.Make_request(C.String_io.M)
    module Header_io = Cohttp.Header_io.Make(C.String_io.M)

    let default_ctx = ()
    type ctx = unit

    let call ?ctx ?headers ?body ?chunked meth uri = 
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
      let body_str = Js.to_string xml##responseText in
      let body = CLB.of_string body_str in
      
      (* (re-)construct the response *)
      let resp_headers = 
        let resp_headers = Js.to_string (xml##getAllResponseHeaders()) in
        let resp_headers = Header_io.parse (C.String_io.open_in resp_headers) in
        resp_headers
      in
      
      let response = Response.make 
        ~version:`HTTP_1_1
        ~status:(Cohttp.Code.status_of_code xml##status)
        ~flush:false
        ~encoding:(Cohttp.Transfer.Fixed (Int64.of_int (String.length body_str)))
        ~headers:resp_headers 
        ()
      in
      
      Lwt.return (response,body)

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
      let q = List.map (fun (k,v) -> k, [v]) (C.Header.to_list params) in
      let body = Cohttp_lwt_body.of_string (Uri.encoded_of_query q) in
      post ?ctx ~chunked:false ~headers ~body uri

    (* No implementation (can it be done?).  What should the failure exception be? *)
    exception Cohttp_lwt_xhr_callv_not_implemented
    let callv ?ctx uri reqs = Lwt.fail Cohttp_lwt_xhr_callv_not_implemented (* ??? *)
        
end


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

module Response = Cohttp_lwt.Make_response(Cohttp.String_io.M)
module Request = Cohttp_lwt.Make_request(Cohttp.String_io.M)

module Client = struct

    module IO = Cohttp.String_io.M
    module Response = Response
    module Request = Request
    module Header_io = Cohttp.Header_io.Make(Cohttp.String_io.M)

    let default_ctx = ()
    type ctx = unit
  
    (* XXX remove me *)
    let log_active = ref true
    let log fmt =
      Printf.ksprintf (fun s ->
        match !log_active with
        | false -> ()
        | true  -> prerr_endline (">>> GitHub: " ^ s)) fmt

    let call ?ctx ?headers ?body ?chunked meth uri = 
      let xml = XmlHttpRequest.create () in
      let () = xml##_open(Js.string (C.Code.string_of_method meth),
                          Js.string (Uri.to_string uri),
                          Js._false) (* For simplicity, do a sync call.  We should
                                        really make this async. See js_of_ocaml apis
                                        for an example *)
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
                  log "[req header] %s: %s" k v;
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
      
      let () = log "[resp status] %s" (Js.to_string xml##statusText) in
      
      (* construct body *)
      let body_str = Js.to_string xml##responseText in
      let body = CLB.of_string body_str in
      
      (* (re-)construct the response *)
      let resp_headers = 
        let resp_headers = Js.to_string (xml##getAllResponseHeaders()) in
        let resp_headers = Header_io.parse 
          Cohttp.String_io.({ str=resp_headers; pos=0; len=String.length resp_headers }) in
        C.Header.iter 
          (fun k v -> List.iter (fun v -> log "[resp header] %s: %s" k v) v) 
          resp_headers;
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
      
      (* log the response *)
      lwt () = 
        let b = Buffer.create 100 in
        let () = Response.write_header response b in
        let () = log "response:\n%s" (Buffer.contents b) in
        Lwt.return ()
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
    exception Cohttp_lwt_xhr_not_implemented
    let callv ?ctx uri reqs = Lwt.fail Cohttp_lwt_xhr_not_implemented (* ??? *)
        
end


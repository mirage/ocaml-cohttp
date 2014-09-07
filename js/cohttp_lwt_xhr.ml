(* input channel type - a string with a (file) position and length *)
type ic' = 
  {
    str : string;
    mutable pos : int;
    len : int;
  }

module String_io = struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let (>>=) = Lwt.bind
    
    let rec iter = Lwt_list.iter_s
    
    type ic = ic'

    (* output channels are just buffers *)
    type oc = Buffer.t
    
    (* the following read/write logic has only been lightly tested... *)
    let read_rest x = 
      let s = String.sub x.str x.pos (x.len-x.pos) in
      x.pos <- x.len;
      s
    
    let read_line' x = 
      if x.pos < x.len then
        let start = x.pos in
        try
          while x.str.[x.pos] != '\n' do
            x.pos <- x.pos + 1
          done;
          let s = String.sub x.str start (x.pos-start) in
          x.pos <- x.pos + 1;
          Some s
        with _ ->
          Some (read_rest x)
      else
        None
    
    let read_line x = return (read_line' x)

    let read_exactly' x n = 
      if x.len-x.pos < n then None
      else begin
        let s = String.sub x.str x.pos n in
        x.pos <- x.pos + n;
        Some s
      end
    
    let read_exactly x n = return (read_exactly' x n)
    
    let read x n =
      match read_exactly' x n with
      | None when x.pos >= x.len -> Lwt.fail End_of_file
      | None -> return (read_rest x)
      | Some(x) -> return x

    let rec write x s = Buffer.add_string x s; return ()
    
    let flush x = return ()
    
end

module C = Cohttp
module CLB = Cohttp_lwt_body

module Response = Cohttp_lwt.Make_response(String_io)
module Request = Cohttp_lwt.Make_request(String_io)

module Client = struct

    module IO = String_io
    module Response = Response
    module Request = Request

    module Header_io = Cohttp.Header_io.Make(String_io)

    (* XXX remove me *)
    let log_active = ref true
    let log fmt =
      Printf.ksprintf (fun s ->
        match !log_active with
        | false -> ()
        | true  -> prerr_endline (">>> GitHub: " ^ s)) fmt

    let call ?headers ?body ?chunked meth uri = 
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
      lwt resp_headers = 
        let resp_headers = Js.to_string (xml##getAllResponseHeaders()) in
        lwt resp_headers = Header_io.parse 
          String_io.({ str=resp_headers; pos=0; len=String.length resp_headers }) in
        C.Header.iter 
          (fun k v -> List.iter (fun v -> log "[resp header] %s: %s" k v) v) 
          resp_headers;
        Lwt.return resp_headers
      in
      
      let response = Response.make 
        ~version:`HTTP_1_1
        ~status:(Cohttp.Code.status_of_code xml##status)
        ~flush:false
        ~encoding:(Cohttp.Transfer.Fixed (String.length body_str))
        ~headers:resp_headers 
        ()
      in
      
      (* log the response *)
      lwt () = 
        let b = Buffer.create 100 in
        lwt () = Response.write_header response b in
        let () = log "response:\n%s" (Buffer.contents b) in
        Lwt.return ()
      in
      
      Lwt.return (response,body)
        
    (* The HEAD should not have a response body *)
    let head ?headers uri =
      let open Lwt in
      call ?headers ~chunked:false `HEAD uri
      >|= fst

    let get ?headers uri = call ?headers ~chunked:false `GET uri
    let delete ?headers uri = call ?headers ~chunked:false `DELETE uri
    let post ?body ?chunked ?headers uri = call ?headers ?body ?chunked `POST uri
    let put ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PUT uri
    let patch ?body ?chunked ?headers uri = call ?headers ?body ?chunked `PATCH uri

    let post_form ?headers ~params uri =
      let headers = C.Header.add_opt headers "content-type" "application/x-www-form-urlencoded" in
      let q = List.map (fun (k,v) -> k, [v]) (C.Header.to_list params) in
      let body = Cohttp_lwt_body.of_string (Uri.encoded_of_query q) in
      post ~chunked:false ~headers ~body uri

    (* No implementation (can it be done?).  What should the failure exception be? *)
    exception Cohttp_lwt_xhr_not_implemented
    let callv ?(ssl=false) host port reqs = Lwt.fail Cohttp_lwt_xhr_not_implemented (* ??? *)
        
end


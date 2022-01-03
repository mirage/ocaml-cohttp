module Sink = struct
  type _ t = String : string t | Discard : unit t

  let string = String
  let discard = Discard
end

module Source = struct
  type t = Empty | String of string

  let empty = Empty
  let string s = String s

  let to_curl_callback t =
    match t with
    | Empty -> fun _ -> ""
    | String s ->
        let len = String.length s in
        let pos = ref 0 in
        fun max_asked ->
          if !pos >= len then ""
          else
            let chunk_len = min (len - !pos) max_asked in
            let res = String.sub s !pos chunk_len in
            pos := !pos + chunk_len;
            res
end

module Request = struct
  type 'a t = {
    curl : Curl.t;
    body : 'a Sink.t;
    mutable body_buffer : Buffer.t option;
  }

  let curl t = t.curl

  let body (type a) (t : a t) : a =
    match t.body with
    | Discard ->
        assert (t.body_buffer = None);
        ()
    | String ->
        let res =
          Buffer.contents
            (match t.body_buffer with None -> assert false | Some s -> s)
        in
        t.body_buffer <- None;
        res

  let create (type a) ?timeout_ms ?headers method_ ~uri ~(input : Source.t)
      ~(output : a Sink.t) ~on_response : a t =
    let response_header_acc = ref [] in
    let response_body = ref None in
    let h = Curl.init () in
    let buf = Buffer.create 128 in
    Curl.setopt h (CURLOPT_URL uri);
    Curl.setopt h (CURLOPT_CUSTOMREQUEST (Http.Method.to_string method_));
    let () =
      match headers with
      | None -> ()
      | Some headers ->
          let headers =
            Http.Header.fold
              (fun key value acc ->
                Buffer.clear buf;
                Buffer.add_string buf key;
                Buffer.add_string buf ": ";
                Buffer.add_string buf value;
                Buffer.contents buf :: acc)
              headers []
            |> List.rev
          in
          Curl.setopt h (CURLOPT_HTTPHEADER headers)
    in
    Curl.setopt h
      (CURLOPT_HEADERFUNCTION
         (let status_code_ready = ref false in
          let response_http_version = ref None in
          fun header ->
            (match !status_code_ready with
            | false ->
                (match String.split_on_char ' ' header with
                | v :: _ ->
                    response_http_version := Some (Http.Version.of_string v)
                | _ -> (* TODO *) invalid_arg "invalid request");
                status_code_ready := true
            | true -> (
                match header with
                | "\r\n" ->
                    let response =
                      let headers =
                        Http.Header.of_list_rev !response_header_acc
                      in
                      response_header_acc := [];
                      let status =
                        match Curl.getinfo h CURLINFO_HTTP_CODE with
                        | CURLINFO_Long l -> Http.Status.of_int l
                        | _ -> assert false
                      in
                      let version =
                        match !response_http_version with
                        | None -> assert false
                        | Some v -> v
                      in
                      Http.Response.make ~version ~status ~headers ()
                    in
                    on_response response
                | _ ->
                    let k, v =
                      match Stringext.cut header ~on:":" with
                      | None -> invalid_arg "proper abort needed"
                      | Some (k, v) -> (String.trim k, String.trim v)
                    in
                    response_header_acc := (k, v) :: !response_header_acc));
            String.length header));
    Curl.setopt h (CURLOPT_READFUNCTION (Source.to_curl_callback input));
    Curl.setopt h
      (CURLOPT_WRITEFUNCTION
         (match output with
         | Discard -> fun s -> String.length s
         | String ->
             response_body := Some buf;
             fun s ->
               Buffer.add_string buf s;
               String.length s));
    (match timeout_ms with
    | None -> ()
    | Some tms -> Curl.setopt h (CURLOPT_TIMEOUTMS tms));
    { curl = h; body = output; body_buffer = !response_body }
end

module Private = struct
  module Sink = Sink
  module Source = Source
  module Request = Request
end

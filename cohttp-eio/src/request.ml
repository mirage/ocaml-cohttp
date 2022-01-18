type t = {
  req : Http.Request.t;
  reader : Reader.t;
  mutable read_complete : bool;
}

let reader t = t.reader
let headers t = t.req.headers
let meth t = t.req.meth
let scheme t = t.req.scheme
let resource t = t.req.resource
let version t = t.req.version
let is_keep_alive t = Http.Request.is_keep_alive t.req

let read_fixed t =
  match Http.Header.get_transfer_encoding t.req.headers with
  | Http.Transfer.Fixed content_length -> (
      if t.read_complete then Error "End of file"
      else
        let content_length = Int64.to_int content_length in
        try
          Result.ok @@ Reader.parse t.reader (Parser.fixed_body content_length)
        with e -> Error (Printexc.to_string e))
  | _ -> Error "Request is not a fixed content body"

let read_chunk t =
  match Http.Header.get_transfer_encoding t.req.headers with
  | Http.Transfer.Chunked ->
      let total_read = ref 0 in
      let rec chunk_loop f =
        if t.read_complete then Error "End of file"
        else
          let chunk = Reader.parse t.reader @@ Parser.chunk !total_read t.req in
          match chunk with
          | `Chunk (size, data, extensions) ->
              f (Chunk.Chunk { size; data; extensions });
              total_read := !total_read + size;
              (chunk_loop [@tailcall]) f
          | `Last_chunk (extensions, updated_request) ->
              t.read_complete <- true;
              f (Chunk.Last_chunk extensions);
              Ok { t with req = updated_request }
      in
      chunk_loop
  | _ -> fun _ -> Error "Request is not a chunked request"

let set_read_complete t = t.read_complete <- true

type t = {
  status : Http.Status.t;
  version : Http.Version.t;
  body : body;
  mutable headers : Http.Header.t;
}

and body =
  [ `String of string
  | `Chunked of write_chunk
  | `Custom of Eio.Flow.sink -> unit
  | `None ]

and write_chunk = (Chunk.t -> unit) -> unit

let create ?(headers = Http.Header.init ()) ?(status = `OK) body =
  { headers; status; version = `HTTP_1_1; body }

(* Response Details *)

let headers t = t.headers
let status t = t.status
let body t = t.body

(* Basic Response *)

let text body =
  let headers = Http.Header.init () in
  let headers =
    Http.Header.add_list headers
      [
        ("content-type", "text/plain; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (`String body)

let html body =
  let headers = Http.Header.init () in
  let headers =
    Http.Header.add_list headers
      [
        ("content-type", "text/html; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (`String body)

let not_found = create ~status:`Not_found `None
let internal_server_error = create ~status:`Internal_server_error `None
let bad_request = create ~status:`Bad_request `None

let write_chunked (client_conn : Client_connection.t) chunk_writer =
  let extensions exts =
    let buf = Buffer.create 0 in
    List.iter
      (fun { Chunk.name; value } ->
        let v =
          match value with None -> "" | Some v -> Printf.sprintf "=%s" v
        in
        Printf.sprintf ";%s%s" name v |> Buffer.add_string buf)
      exts;
    Buffer.contents buf
  in
  let write = function
    | Chunk.Chunk { size; data; extensions = exts } ->
        let buf =
          Printf.sprintf "%X%s\r\n%s\r\n" size (extensions exts)
            (Cstruct.to_string data)
        in
        Eio.Flow.copy_string buf client_conn.flow
    | Chunk.Last_chunk exts ->
        let buf = Printf.sprintf "%X%s\r\n" 0 (extensions exts) in
        Eio.Flow.copy_string buf client_conn.flow
  in
  chunk_writer write

let write (client_conn : Client_connection.t) t =
  Buffer.clear client_conn.response_buffer;
  let version = Http.Version.to_string t.version in
  let status = Http.Status.to_string t.status in
  Buffer.add_string client_conn.response_buffer version;
  Buffer.add_string client_conn.response_buffer " ";
  Buffer.add_string client_conn.response_buffer status;
  Buffer.add_string client_conn.response_buffer "\r\n";
  Http.Header.clean_dup t.headers
  |> Http.Header.iter (fun k v ->
         Buffer.add_string client_conn.response_buffer k;
         Buffer.add_string client_conn.response_buffer ": ";
         Buffer.add_string client_conn.response_buffer v;
         Buffer.add_string client_conn.response_buffer "\r\n");
  Buffer.add_string client_conn.response_buffer "\r\n";
  match t.body with
  | `String s ->
      Buffer.add_string client_conn.response_buffer s;
      Eio.Flow.copy_string
        (Buffer.contents client_conn.response_buffer)
        client_conn.flow
  | `Custom writer ->
      Eio.Flow.copy_string
        (Buffer.contents client_conn.response_buffer)
        client_conn.flow;
      writer (client_conn.flow :> Eio.Flow.sink)
  | `Chunked chunk_writer ->
      Eio.Flow.copy_string
        (Buffer.contents client_conn.response_buffer)
        client_conn.flow;
      write_chunked client_conn chunk_writer
  | `None ->
      Eio.Flow.copy_string
        (Buffer.contents client_conn.response_buffer)
        client_conn.flow

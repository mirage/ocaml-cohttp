type t = {
  headers : Http.Header.t;
  body : body;
  status : Http.Status.t;
  version : Http.Version.t;
}

and body =
  | String of string
  | Chunked of write_chunk
  | Custom of (Eio.Flow.sink -> unit)
  | Empty

and write_chunk = (Chunk.t -> unit) -> unit

let create ?(version = `HTTP_1_1) ?(status = `OK)
    ?(headers = Http.Header.init ()) body =
  { headers; body; status; version }

(* Response Details *)

let headers t = t.headers
let status t = t.status
let body t = t.body

(* https://datatracker.ietf.org/doc/html/rfc7230#section-4.1 *)
let write_chunked flow chunk_writer =
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
        Eio.Flow.copy_string buf flow
    | Chunk.Last_chunk exts ->
        let buf = Printf.sprintf "%X%s\r\n" 0 (extensions exts) in
        Eio.Flow.copy_string buf flow
  in
  chunk_writer write

let write t writer =
  let version = Http.Version.to_string t.version in
  let status = Http.Status.to_string t.status in
  Writer.write_string writer version;
  Writer.write_string writer " ";
  Writer.write_string writer status;
  Writer.write_string writer "\r\n";
  Http.Header.iter
    (fun k v ->
      Writer.write_string writer k;
      Writer.write_string writer ": ";
      Writer.write_string writer v;
      Writer.write_string writer "\r\n")
    t.headers;
  Writer.write_string writer "\r\n";
  match t.body with
  | String s -> Writer.write_string writer s
  | Custom f -> 
    Writer.wakeup writer;
    f (Writer.sink writer)
  | Chunked chunk_writer -> write_chunked (Writer.sink writer) chunk_writer
  | Empty -> ()

(* Basic Response *)

let text body =
  let headers =
    Http.Header.of_list
      [
        ("content-type", "text/plain; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in

  create ~headers (String body)

let html body =
  let headers =
    Http.Header.of_list
      [
        ("content-type", "text/html; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  create ~headers (String body)

let not_found = create ~status:`Not_found Empty
let internal_server_error = create ~status:`Internal_server_error Empty
let bad_request = create ~status:`Bad_request Empty

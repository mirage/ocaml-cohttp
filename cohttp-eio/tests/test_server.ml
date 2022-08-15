open Cohttp_eio

let app (req, reader, _) =
  match Http.Request.resource req with
  | "/get" -> Server.text_response (Fmt.to_to_string Http.Request.pp req)
  | "/get_error" -> (
      match Server.read_fixed req reader with
      | Some _ -> Server.text_response "FAIL"
      | None -> Server.text_response "PASS")
  | "/post" ->
      let body = Server.read_fixed req reader |> Option.get in
      let buf = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buf in
      Http.Request.pp fmt req;
      Format.fprintf fmt "\n\n%s%!" body;
      Server.text_response (Buffer.contents buf)
  | "/handle_chunk" -> (
      let dump_chunk buf chunk =
        let s = Format.asprintf "\n%a" Body.pp_chunk chunk in
        Buffer.add_string buf s
      in
      let chunk_buf = Buffer.create 0 in
      match Server.read_chunked req reader (dump_chunk chunk_buf) with
      | Some headers ->
          let req = { req with headers } in
          Buffer.contents chunk_buf
          |> Format.asprintf "%a@ %s%!" Http.Request.pp req
          |> Server.text_response
      | None -> Server.bad_request_response)
  | "/get_chunk" ->
      let rec body_writer chan chunks f =
        match In_channel.input_line chan with
        | Some data ->
            let extensions =
              if chunks = 0 then
                [
                  Body.{ name = "ext1"; value = Some "ext1_v" };
                  { name = "ext2"; value = Some "ext2_v" };
                  { name = "ext3"; value = None };
                ]
              else []
            in
            let chunk =
              Body.Chunk { size = String.length data; data; extensions }
            in
            f chunk;
            body_writer chan (chunks + 1) f
        | None ->
            let last_chunk = Body.Last_chunk [] in
            In_channel.close chan;
            f last_chunk
      in
      let trailer_writer f =
        let trailer_headers =
          Http.Header.of_list
            [
              ("Expires", "Wed, 21 Oct 2015 07:28:00 GMT");
              ("Header1", "Header1 value text");
              ("Header2", "Header2 value text");
            ]
        in
        f trailer_headers
      in
      let chan =
        In_channel.open_gen [ Open_text; Open_rdonly ] 0 "server_chunks.txt"
      in
      let headers =
        Http.Header.of_list
          [
            ("Transfer-Encoding", "chunked");
            ("Content-Type", "text/plain");
            ("Trailer", "Expires, Header1");
          ]
      in
      let response = Http.Response.make ~status:`OK ~headers () in
      let body =
        Body.Chunked { body_writer = body_writer chan 0; trailer_writer }
      in
      (response, body)
  | _ -> Server.bad_request_response

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";

  Eio_main.run @@ fun env -> Server.run ~port:!port env app

(*{{{ Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt.Syntax
open Lwt.Infix

module Body = struct
  module Substring = struct
    type t = { base : string; pos : int; len : int }
  end

  module Encoding = struct
    type t = Fixed of int64 | Chunked

    let fixed i = Fixed i
    let chunked = Chunked
  end

  type t =
    Encoding.t
    * [ `String of string | `Stream of unit -> Substring.t option Lwt.t ]

  let encoding = fst

  let string ?encoding s =
    let encoding =
      match encoding with
      | Some s -> s
      | None -> Encoding.Fixed (Int64.of_int (String.length s))
    in
    (encoding, `String s)

  let stream ?(encoding = Encoding.Chunked) f : t = (encoding, `Stream f)
  let chunk_size = 4096

  let write_chunk oc (sub : Substring.t) =
    let* () = Lwt_io.write oc (Printf.sprintf "%x\r\n" sub.len) in
    let* () = Lwt_io.write_from_string_exactly oc sub.base sub.pos sub.len in
    Lwt_io.write oc "\r\n"

  let next_chunk base ~pos =
    let len = String.length base in
    if pos >= len then None
    else Some { Substring.base; pos; len = min chunk_size (len - pos) }

  let rec write_string_as_chunks oc s ~pos =
    match next_chunk s ~pos with
    | None -> Lwt_io.write oc "\r\n"
    | Some chunk ->
        let* () = write_chunk oc chunk in
        let pos = pos + chunk.len in
        write_string_as_chunks oc s ~pos

  let rec write_fixed_stream oc f =
    f () >>= function
    | None -> Lwt.return_unit
    | Some { Substring.base; pos; len } ->
        let* () = Lwt_io.write_from_string_exactly oc base pos len in
        write_fixed_stream oc f

  let rec write_chunks_stream oc f =
    f () >>= function
    | None -> Lwt_io.write oc "\r\n"
    | Some chunk ->
        let* () = write_chunk oc chunk in
        write_chunks_stream oc f

  let write ((encoding, body) : t) oc =
    match body with
    | `String s -> (
        match encoding with
        | Fixed _ -> Lwt_io.write oc s
        | Chunked -> write_string_as_chunks oc s ~pos:0)
    | `Stream f -> (
        match encoding with
        | Fixed _ -> write_fixed_stream oc f
        | Chunked -> write_chunks_stream oc f)
end

module Input_channel = struct
  module Bytebuffer = Http_bytebuffer.Bytebuffer

  module Refill =
    Bytebuffer.Make
      (struct
        include Lwt

        let ( >>| ) = ( >|= )
      end)
      (struct
        type src = Lwt_io.input_channel

        let rec refill ic buf ~pos ~len =
          let open Lwt.Infix in
          if Lwt_io.is_closed ic then Lwt.return `Eof
          else
            Lwt.catch
              (fun () ->
                Lwt_io.direct_access ic (fun da ->
                    let available = da.da_max - da.da_ptr in
                    if available = 0 then
                      let+ read = da.da_perform () in
                      if read = 0 then `Eof else `Refill
                    else
                      let read_len = min available len in
                      Lwt_bytes.blit_to_bytes da.da_buffer da.da_ptr buf pos
                        read_len;
                      da.da_ptr <- da.da_ptr + read_len;
                      Lwt.return (`Ok read_len)))
              (function
                | Unix.Unix_error (ECONNRESET, _, _) | Lwt_io.Channel_closed _
                  ->
                    let* () = Lwt_io.close ic in
                    Lwt.return `Eof
                | exn -> raise exn)
            >>= function
            | `Eof ->
                let* () = Lwt_io.close ic in
                Lwt.return `Eof
            | `Ok n -> Lwt.return (`Ok n)
            | `Refill -> refill ic buf ~pos ~len
      end)

  type t = { buf : Bytebuffer.t; ic : Lwt_io.input_channel }

  let create ?(buf_len = 0x4000) ic = { buf = Bytebuffer.create buf_len; ic }
  let read_line_opt t = Refill.read_line t.buf t.ic
  let read t count = Refill.read t.buf t.ic count
  let refill t = Refill.refill t.buf t.ic
  let remaining t = Bytebuffer.length t.buf

  let with_input_buffer (t : t) ~f =
    let buf = Bytebuffer.unsafe_buf t.buf in
    let pos = Bytebuffer.pos t.buf in
    let len = Bytebuffer.length t.buf in
    let res, consumed = f (Bytes.unsafe_to_string buf) ~pos ~len in
    Bytebuffer.drop t.buf consumed;
    res

  let with_input_buffer' (t : t) ~f =
    let buf = Bytebuffer.unsafe_buf t.buf in
    let pos = Bytebuffer.pos t.buf in
    let len = Bytebuffer.length t.buf in
    let+ res, consumed = f (Bytes.unsafe_to_string buf) ~pos ~len in
    Bytebuffer.drop t.buf consumed;
    res
end

module Context = struct
  type request_body = Unread | Reading of unit Lwt.t

  type t = {
    request : Http.Request.t;
    ic : Input_channel.t;
    oc : Lwt_io.output_channel;
    mutable request_body : request_body;
    response_sent : Http.Response.t Lwt.t;
    response_send : Http.Response.t Lwt.u;
  }

  let request t = t.request

  let create request ic oc =
    let response_sent, response_send = Lwt.wait () in
    { request; ic; oc; response_sent; response_send; request_body = Unread }

  let rec step_fixed t ~(f : Body.Substring.t -> _ -> _ Lwt.t) ~init ~left :
      (_ * int) option Lwt.t =
    if left = 0 then Lwt.return_none
    else if Input_channel.remaining t.ic = 0 then
      Input_channel.refill t.ic >>= function
      | `Ok -> step_fixed t ~f ~init ~left
      | `Eof -> Lwt.return_none (* TODO invalid input *)
    else
      let+ res =
        Input_channel.with_input_buffer' t.ic ~f:(fun base ~pos ~len ->
            let len = min left len in
            let+ acc = f { Body.Substring.base; pos; len } init in
            ((acc, left - len), len))
      in
      Some res

  let parse_chunksize chunk_size_hex =
    let hex =
      (* From https://tools.ietf.org/html/rfc7230#section-4.1.1
          > The chunked encoding allows each chunk to include zero or
          > more chunk extensions, immediately following the chunk-size
      *)
      try String.sub chunk_size_hex 0 (String.index chunk_size_hex ';')
      with _ -> chunk_size_hex
    in
    Int64.of_string_opt ("0x" ^ hex)

  let step_chunked :
      'a.
      t ->
      f:(Body.Substring.t -> 'acc -> 'acc Lwt.t) ->
      init:'acc ->
      'acc option Lwt.t =
   fun t ~f ~init ->
    Input_channel.read_line_opt t.ic >>= function
    | None -> Lwt.return_none (* TODO invalid input *)
    | Some "" -> Lwt.return_none
    | Some line -> (
        match parse_chunksize line with
        | None -> Lwt.return_none
        | Some size ->
            let size = Int64.to_int size in
            let* base = Input_channel.read t.ic size in
            let chunk =
              { Body.Substring.base; pos = 0; len = String.length base }
            in
            let+ init = f chunk init in
            Some init)

  let read_body t (encoding : Body.Encoding.t) ~init ~f =
    match encoding with
    | Fixed i ->
        let rec loop init left =
          step_fixed t ~f ~init ~left >>= function
          | None -> Lwt.return init
          | Some (acc, left) -> loop acc left
        in
        loop init (Int64.to_int i)
    | Chunked ->
        let rec loop init =
          step_chunked t ~f ~init >>= function
          | None -> Lwt.return init
          | Some acc -> loop acc
        in
        loop init

  let with_body t ~init ~f =
    assert (t.request_body = Unread);
    match Http.Request.has_body t.request with
    | `Unknown | `No ->
        t.request_body <- Reading Lwt.return_unit;
        Lwt.return init
    | `Yes ->
        let rt, ru = Lwt.wait () in
        t.request_body <- Reading rt;
        let encoding =
          match Http.Header.get_transfer_encoding t.request.headers with
          | Chunked -> Body.Encoding.Chunked
          | Fixed i -> Fixed i
          | Unknown -> assert false
        in
        let+ acc = read_body t encoding ~init ~f in
        Lwt.wakeup_later ru ();
        acc

  let read_body t =
    let+ buf =
      with_body t ~init:(Buffer.create 128)
        ~f:(fun { Body.Substring.base; pos; len } acc ->
          Buffer.add_substring acc base pos len;
          Lwt.return acc)
    in
    Buffer.contents buf

  let discard_body t = with_body t ~init:() ~f:(fun _ () -> Lwt.return_unit)

  let respond t (response : Http.Response.t) (body : Body.t) =
    let headers =
      let encoding =
        match (Body.encoding body : Body.Encoding.t) with
        | Fixed i -> Http.Transfer.Fixed i
        | Chunked -> Chunked
      in
      Http.Header.add_transfer_encoding response.headers encoding
    in
    let* () =
      let* () = Lwt_io.write t.oc (Http.Version.to_string response.version) in
      let* () = Lwt_io.write_char t.oc ' ' in
      let* () = Lwt_io.write t.oc (Http.Status.to_string response.status) in
      let* () = Lwt_io.write t.oc "\r\n" in
      let* () =
        Http.Header.to_list headers
        |> Lwt_list.iter_s (fun (k, v) ->
               let* () = Lwt_io.write t.oc k in
               let* () = Lwt_io.write t.oc ": " in
               let* () = Lwt_io.write t.oc v in
               Lwt_io.write t.oc "\r\n")
      in
      let* () = Lwt_io.write t.oc "\r\n" in
      Body.write body t.oc
    in
    Lwt.wakeup_later t.response_send response;
    Lwt_io.flush t.oc
end

type on_exn = Hook | Callback of (exn -> unit)
type t = { callback : Context.t -> unit Lwt.t; on_exn : on_exn }

let create ?on_exn callback =
  let on_exn = match on_exn with None -> Hook | Some f -> Callback f in
  { on_exn; callback }

let rec read_request ic =
  let result =
    Input_channel.with_input_buffer ic ~f:(fun buf ~pos ~len ->
        match Http.Private.Parser.parse_request ~pos ~len buf with
        | Ok (req, consumed) -> (`Ok req, consumed)
        | Error Partial -> (`Partial, 0)
        | Error (Msg msg) -> (`Invalid msg, 0))
  in
  match result with
  | `Partial -> (
      Input_channel.refill ic >>= function
      | `Ok -> read_request ic
      | `Eof -> Lwt.return `Eof)
  | `Ok req -> Lwt.return (`Ok req)
  | `Invalid msg -> Lwt.return (`Error msg)

let handle_connection { callback; on_exn } (ic, oc) =
  let on_exn =
    match on_exn with
    | Hook -> fun exn -> !Lwt.async_exception_hook exn
    | Callback f -> f
  in
  let rec loop callback ic oc =
    read_request ic >>= function
    | `Error _ | `Eof -> Lwt.return_unit
    | `Ok req ->
        let context = Context.create req ic oc in
        Lwt.dont_wait (fun () -> callback context) on_exn;
        let* response =
          match context.request_body with
          | Unread -> assert false (* TODO *)
          | Reading body ->
              let+ (), response = Lwt.both body context.response_sent in
              response
        in
        let keep_alive =
          Http.Request.is_keep_alive req
          &&
          match Http.Header.connection (Http.Response.headers response) with
          | Some `Keep_alive -> true
          | Some `Close | Some (`Unknown _) -> false
          | None -> Http.Response.version response = `HTTP_1_1
        in
        if keep_alive then loop callback ic oc else Lwt.return_unit
  in
  loop callback (Input_channel.create ic) oc

module IO = struct
  type 'a t = 'a

  let ( >>= ) v f = f v
  let return v = v

  type ic = Eio.Buf_read.t
  type oc = Eio.Buf_write.t
  type conn = Eio.Net.Sockaddr.stream

  let refill ic =
    try
      let () = Eio.Buf_read.(ensure ic (buffered_bytes ic + 1)) in
      `Ok
    with End_of_file -> `Eof

  let with_input_buffer ic ~f =
    let contents = Eio.Buf_read.peek ic in
    let res, consumed =
      f (Cstruct.to_string contents) ~pos:0 ~len:(Cstruct.length contents)
    in
    let () = Eio.Buf_read.consume ic consumed in
    res

  let read_line ic =
    let line = Eio.Buf_read.take_while (fun c -> not (Char.equal c '\r')) ic in
    match Eio.Buf_read.any_char ic with
    | exception End_of_file -> None
    | _ ->
        let () =
          match Eio.Buf_read.peek_char ic with
          | Some '\n' -> Eio.Buf_read.consume ic 1
          | _ -> ()
        in
        Some line

  let read ic len =
    match Eio.Buf_read.ensure ic 1 with
    | exception End_of_file -> ""
    | () ->
        let len = Int.min len (Eio.Buf_read.buffered_bytes ic) in
        Eio.Buf_read.take len ic

  let write oc string = Eio.Buf_write.string oc string
  let flush = Eio.Buf_write.flush
end

module Request = Cohttp.Request.Private.Make (IO)
module Response = Cohttp.Response.Private.Make (IO)
module Transfer = Cohttp.Private.Transfer_io.Make (IO)

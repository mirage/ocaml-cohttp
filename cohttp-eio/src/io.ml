let src = Logs.Src.create "cohttp.eio.io" ~doc:"Cohttp Eio IO module"

module Logs = (val Logs.src_log src : Logs.LOG)

module IO = struct
  type 'a t = 'a

  let ( >>= ) v f = f v
  let return v = v

  type ic = Eio.Buf_read.t
  type oc = Eio.Buf_write.t
  type conn = Eio.Switch.t * Eio.Net.Sockaddr.stream

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
    try
      let line = Eio.Buf_read.line ic in
      let () = Logs.debug (fun f -> f "<<< %s" line) in
      Some line
    with End_of_file ->
      let () = Logs.debug (fun f -> f "<<< EOF") in
      None

  let read ic len =
    match Eio.Buf_read.ensure ic 1 with
    | exception End_of_file ->
        let () = Logs.debug (fun f -> f "<<< EOF") in
        ""
    | () ->
        let len = Int.min len (Eio.Buf_read.buffered_bytes ic) in
        let read = Eio.Buf_read.take len ic in
        let () = Logs.debug (fun f -> f "<<< %s" read) in
        read

  let write oc string =
    let () = Logs.debug (fun f -> f ">>> %s" (String.trim string)) in
    Eio.Buf_write.string oc string

  let flush = Eio.Buf_write.flush
end

module Request = Cohttp.Request.Private.Make (IO)
module Response = Cohttp.Response.Private.Make (IO)
module Transfer = Cohttp.Private.Transfer_io.Make (IO)

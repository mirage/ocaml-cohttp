let src = Logs.Src.create "cohttp.eio" ~doc:"Cohttp EIO backend"

module Logs = (val Logs.src_log src)

let flow_of_reader reader read_body_chunk =
  object
    inherit Eio.Flow.source
    val mutable buffered = None

    method read_into output =
      let output_length = Cstruct.length output in
      let send buffer pos =
        let available = String.length buffer - pos in
        if output_length >= available then
          let () = Cstruct.blit_from_string buffer pos output 0 available
          and () = buffered <- None in
          available
        else
          let () = Cstruct.blit_from_string buffer 0 output 0 output_length
          and () = buffered <- Some (buffer, pos + output_length) in
          output_length
      in
      match buffered with
      | Some (buffer, pos) -> send buffer pos
      | None -> (
          match read_body_chunk reader with
          | Cohttp.Transfer.Done ->
              let () = Logs.debug (fun m -> m "end of inbound body") in
              raise End_of_file
          | Chunk data | Final_chunk data ->
              let () =
                Logs.debug (fun m ->
                    m "received %d bytes of body" (String.length data))
              in
              send data 0)
  end

let flow_to_writer flow writer write_body =
  let input = Eio.Buf_read.of_flow ~max_size:max_int flow in
  let rec loop () =
    let () =
      let () = Eio.Buf_read.ensure input 1 in
      let contents = Eio.Buf_read.(take (buffered_bytes input) input) in
      let () =
        Logs.debug (fun m -> m "send %d bytes of body" (String.length contents))
      in
      write_body writer contents
    in
    loop ()
  in
  try loop ()
  with End_of_file ->
    let () = Logs.debug (fun m -> m "end of outbound body") in
    ()

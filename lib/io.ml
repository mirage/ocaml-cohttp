let max_req_rep = 16384 * 8

module Make (IO : S.IO) = struct
  open IO

  module T_io = Transfer_io.Make(IO)

  let (>>|) x f = x >>= (fun x -> return (f x))

  let err e = `Error e
  let err' e = return (`Error e)
  let ok v = `Ok v
  let ok' v = return (`Ok v)

  let read_until_rnrn ic =
    let rec loop acc max i =
      read_line ic >>= function
      | None -> err' `Eof
      | Some "" -> ok' acc
      | Some s when (String.length s + i) > max -> err' `Too_large
      | Some s -> loop (s :: acc) max (String.length s + i)
    in
    loop [] max_req_rep 0 >>| function
    | `Ok [] -> err `Empty
    | `Ok xs -> (`Ok (List.rev xs))
    | `Error _ as e -> e

  let rec source_to_oc source oc =
    match source () with
    | None -> return ()
    | Some chunk ->
      IO.write oc chunk >>= fun () ->
      source_to_oc source oc

  let rec write_list list oc =
    match list with
    | [] -> return ()
    | x::xs -> write oc x >>= fun () -> write_list xs oc

  let write_footer encoding oc =
    match encoding with
    | Transfer.Chunked ->
      (* TODO Trailer header support *)
      IO.write oc "0\r\n\r\n"
    | Transfer.Fixed _ | Transfer.Unknown -> return ()

  let write ?flush write_body list encoding (oc : IO.oc) =
    write_list list oc >>= fun () ->
    let writer = T_io.make_writer ?flush encoding oc in
    write_body writer >>= fun () ->
    write_footer encoding oc

  let write_req ?flush write_body req =
    let req = Request.prepare req in
    write ?flush write_body (Request.to_string_list req) req.Request.encoding

  let write_rep ?flush write_body rep =
    let rep = Response.prepare rep in
    write ?flush write_body (Response.to_string_list rep) rep.Response.encoding

  let make_body_reader = T_io.make_reader
  let read_body_chunk  = T_io.read
  let write_body       = T_io.write

  let read ic f =
    read_until_rnrn ic >>| function
    | `Ok lines ->
      begin match f lines with
      | `Error (s : string) -> `Error (`Invalid s)
      | `Ok v -> `Ok v
      end
    | `Error x -> `Error x

  let read_req ic = read ic Request.of_string_list

  let read_rep ic = read ic Response.of_lines

  let read_chunk = T_io.read
end

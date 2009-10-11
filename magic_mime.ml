(*pp camlp4o -I `ocamlfind query lwt.syntax` pa_lwt.cmo *)
open Lwt
open Lwt_io

let magic = Hashtbl.create 31

let init file_name =
  with_file ~flags:[Unix.O_RDONLY] ~mode:input file_name
    (fun ch ->
      let split = Pcre.split ~pat:"[ \r\t\n]+" in
      Lwt_stream.iter
        (fun l ->
           if l = "" || l.[0] <> '#' then (
             match split l with
             | [mime_type] -> ()
             | [] -> ()
             | mime_type :: exts ->
               let m = String.lowercase mime_type in
               List.iter
                 (fun ext -> 
                    Hashtbl.replace magic ext m
                 ) exts
          )
       ) (read_lines ch)
   )

let lookup ext =
  try
    Hashtbl.find magic (String.lowercase ext)
  with
    Not_found -> "application/octet-stream"

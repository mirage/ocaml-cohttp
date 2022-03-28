(** Utility to convert newlines into \r\n. *)
let rec convert buf =
  try
    let ln = read_line () in
    Buffer.add_string buf ln;
    Buffer.add_string buf "\r\n";
    convert buf
  with End_of_file -> Buffer.contents buf

let () =
  let s = convert (Buffer.create 0) in
  output_string stdout s

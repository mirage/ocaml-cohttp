module IO = Cohttp_eio.Private.IO

let bigstring_of_string s =
  let len = String.length s in
  let t = Cohttp.Bigstring.create len in
  Cohttp.Bigstring.blit_from_string s ~src_off:0 t ~dst_off:0 ~len;
  t

let written f =
  let buffer = Buffer.create 16 in
  Eio_main.run @@ fun _env ->
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buffer) f;
  Buffer.contents buffer

(* `Passthrough takes eio's scheduling path, `Copy its buffered one; both must
   land in order with the surrounding string writes. *)
let interleaved tag () =
  let bigstring = bigstring_of_string "xbodyx" in
  Alcotest.(check string)
    "in order" "headbodytail"
    (written (fun oc ->
         IO.write oc "head";
         IO.write_bigstring oc (tag bigstring) 1 4;
         IO.write oc "tail"))

let () =
  Alcotest.run "cohttp-eio write_bigstring"
    [
      ( "write_bigstring",
        [
          Alcotest.test_case "copy" `Quick (interleaved (fun b -> `Copy b));
          Alcotest.test_case "passthrough" `Quick
            (interleaved (fun b -> `Passthrough b));
        ] );
    ]

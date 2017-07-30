(*{{{ Copyright (c) 2012 David Sheets <sheets@alum.mit.edu>
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

open Printf

module A = Cohttp.Accept

let suite_of
  : type a. (string option -> a)
    -> a Alcotest.testable
    -> (string * a) list
    -> _ list
  = fun parser t ->
    List.map (fun (s, expected) ->
        let test () =
          Alcotest.(check t) s (parser (Some s)) expected in
        (s, `Quick, test))

let valid_media_ranges = [
  "text/plain", [1000,(A.MediaType ("text","plain"),[])];
  "text/*", [1000,(A.AnyMediaSubtype "text",[])];
  "*/*", [1000,(A.AnyMedia,[])];
  "*/*;q=1", [1000,(A.AnyMedia,[])];
  "*/*;q=0", [0,(A.AnyMedia,[])];
  "*/*;q=1.", [1000,(A.AnyMedia,[])];
  "*/*;q=1.0", [1000,(A.AnyMedia,[])];
  "*/*;q=.0", [0,(A.AnyMedia,[])];
  (* TODO invalid test "*/*;q=.", [0,(A.AnyMedia,[])]; *)
  "*/*;q=0.", [0,(A.AnyMedia,[])];
  "*/*;q=0.1", [100,(A.AnyMedia,[])];
  "image/*,text/*", [
    1000,(A.AnyMediaSubtype "image",[]);
    1000,(A.AnyMediaSubtype "text",[]);
  ];
  "text/plain; q=0.8; charset=utf-8,text/HTML;charset=utf-8;q=0.9", [
    800,(A.MediaType ("text","plain"),["charset",A.T"utf-8"]);
    900,(A.MediaType ("text","html"),["charset",A.T"utf-8"]);
  ];
  "text/*;foo=\"bar\"", [1000,(A.AnyMediaSubtype "text",["foo",A.S"bar"])];
  "*/*;qu=\"\\\"\"", [1000,(A.AnyMedia,["qu",A.S"\""])];
  "*/*;f=\";q=0,text/plain\"", [1000,(A.AnyMedia,["f",A.S";q=0,text/plain"])];
]

let valid_media_ranges_suite =
  let t_media_ranges =
    Alcotest.testable (Fmt.of_to_string A.string_of_media_ranges) (=) in
  suite_of A.media_ranges t_media_ranges valid_media_ranges

let valid_charsets = [
  "utf-8", [1000,A.Charset "utf-8"];
  "UTF-8", [1000,A.Charset "utf-8"];
  "iso-8859-1", [1000,A.Charset "iso-8859-1"];
  "ISO-8859-1; q = 0.8, *; q=0.7", [
    800,A.Charset "iso-8859-1";
    700,A.AnyCharset;
  ];
]

let valid_charsets_suite =
  let t_charsets =
    Alcotest.testable (Fmt.of_to_string A.string_of_charsets) (=) in
  suite_of A.charsets t_charsets valid_charsets

let valid_encodings = [
  "compress, gzip", [1000,A.Compress; 1000,A.Gzip];
  "", [];
  "*", [1000, A.AnyEncoding];
  "compress;q=0.5, gzip;q=1.0", [500,A.Compress; 1000,A.Gzip];
  "Gzip;q=1.0, identity; q=0.5, *;q=0", [
    1000,A.Gzip;
    500,A.Identity;
    0,A.AnyEncoding;
  ];
]

let valid_encodings_suite =
  let t_encodings =
    Alcotest.testable (Fmt.of_to_string A.string_of_encodings) (=) in
  suite_of A.encodings t_encodings valid_encodings

let valid_languages = [
  "en",[1000,A.Language["en"]];
  "en-US",[1000,A.Language["en";"us"]];
  "en-cockney",[1000,A.Language["en";"cockney"]];
  "i-cherokee",[1000,A.Language["i";"cherokee"]];
  "x-pig-latin",[1000,A.Language["x";"pig";"latin"]];
  "da, en-gb;q=0.8, en;q=0.7",[
    1000,A.Language["da"];
    800,A.Language["en";"gb"];
    700,A.Language["en"];
  ];
  "en-US, *;q=0.9",[1000,A.Language["en";"us"]; 900,A.AnyLanguage];
]

let valid_languages_suite =
  let t_languages =
    Alcotest.testable (Fmt.of_to_string A.string_of_languages) (=) in
  suite_of A.languages t_languages valid_languages

let () = Printexc.record_backtrace true

let () =
  Alcotest.run "test_accept" [
    "Valid Accept",valid_media_ranges_suite;
    "Valid Accept-Charset", valid_charsets_suite;
    "Valid Accept-Encoding", valid_encodings_suite;
    "Valid Accept-Language", valid_languages_suite;
  ]

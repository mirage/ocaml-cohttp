open OUnit

let printer = String.concat "-"

let test_split_1 () =
  let strings = Stringext.split "test:one:two" ~on:':' in
  assert_equal ~printer strings ["test";"one";"two"]

let test_split_bounded_1 () =
  let strings = Stringext.split "testing:foo:bar" ~on:':' ~max:2 in
  assert_equal ~printer strings ["testing";"foo:bar"]

let test_split_none () =
  let s = "foo:bar" in
  assert_equal ~printer [s] (Stringext.split s ~on:'=')

let test_fixtures =
  "test various string functions" >:::
  [
    "test split char 1" >:: test_split_1;
    "test split bounded 1" >:: test_split_bounded_1;
    "test split none" >:: test_split_none;
  ]

let _ = run_test_tt_main test_fixtures



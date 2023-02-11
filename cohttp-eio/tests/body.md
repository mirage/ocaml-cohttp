# Body 

```ocaml
open Cohttp_eio
```

A `Buffer.t` sink to test `Body.writer`.

```ocaml
let test_writer w =
  Eio_main.run @@ fun env ->
  let b = Buffer.create 10 in
  let s = Eio.Flow.buffer_sink b in
  let f ~name ~value = Buffer.add_string b (name ^ ": " ^ value ^ "\n") in
  Eio.Buf_write.with_flow s (fun bw ->
    w#write_header f;
    w#write_body bw;
  );
  Eio.traceln "%s" (Buffer.contents b);;
```

## content_writer

```ocaml
# test_writer @@ Body.content_writer ~content:"hello world" ~content_type:"text/plain" ;;
+Content-Length: 11
+Content-Type: text/plain
+hello world
- : unit = ()
```

## form_values_writer

```ocaml
# test_writer @@ Body.form_values_writer [("name1", ["val a"; "val b"; "val c"]); ("name2", ["val c"; "val d"; "val e"])] ;;
+Content-Length: 59
+Content-Type: application/x-www-form-urlencoded
+name1=val%20a,val%20b,val%20c&name2=val%20c,val%20d,val%20e
- : unit = ()
```

## read_content

```ocaml
let test_reader body headers f =
  Eio_main.run @@ fun env ->
    let buf_read = Eio.Buf_read.of_string body in
    let headers = Http.Header.of_list headers in
    let r = object
        method headers = headers
        method buf_read = buf_read
      end
    in
    f r;;
```

`read_content` reads the contents of a reader if `headers` contains valid `Content-Length` header.

```ocaml
# test_reader "hello world" ["Content-Length","11"] Body.read_content ;;
- : string option = Some "hello world"
```

None if 'Content-Length' is not valid.

```ocaml
# test_reader "hello world" ["Content-Length","12a"] Body.read_content ;;
- : string option = None
```

Or if it is missing.

```ocaml
# test_reader "hello world" [] Body.read_content ;;
- : string option = None
```

## read_form_values 

The reader below has both "Content-Length" and "Content-Type" header set correctly, so we are able
to parse the body correctly.

```ocaml
# let body = "name1=val%20a,val%20b,val%20c&name2=val%20c,val%20d,val%20e" in
  test_reader
    body
    [("Content-Length", (string_of_int (String.length body))); ("Content-Type", "application/x-www-form-urlencoded")]
    Body.read_form_values ;;
- : (string * string list) list =
[("name1", ["val a"; "val b"; "val c"]);
 ("name2", ["val c"; "val d"; "val e"])]
```

Note that the reader below doesn't have "Content-Type" header. Thus `read_form_values` returns am empty list.

```ocaml
# let body = "name1=val%20a,val%20b,val%20c&name2=val%20c,val%20d,val%20e" in
  test_reader
    body
    [("Content-Length", (string_of_int (String.length body)))]
    Body.read_form_values ;;
- : (string * string list) list = []
```

Note that the reader below doesn't have "Content-Length" header. Thus `read_form_values` returns am empty list.

```ocaml
# let body = "name1=val%20a,val%20b,val%20c&name2=val%20c,val%20d,val%20e" in
  test_reader
    body
    [("Content-Type", "application/x-www-form-urlencoded")]
    Body.read_form_values ;;
- : (string * string list) list = []
```

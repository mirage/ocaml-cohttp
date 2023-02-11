# Header

```ocaml
open Cohttp_eio
```

## Header.pp

```ocaml
# let t = Header.of_list [("Content-Length", "200");("Content-Type", "text/plain"); ("Header1", "hello")];;
val t : Header.t = <abstr>

# Header.pp Format.std_formatter t;;
{
  Content-Length:  200;
  Content-Type:  text/plain;
  Header1:  hello
}
- : unit = ()
```

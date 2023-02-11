# Method

```ocaml
open Cohttp_eio
```

## to_string

```ocaml
# Method.(to_string Get) ;;
- : string = "GET"

# Method.(to_string Head) ;;
- : string = "HEAD"

# Method.(to_string Delete) ;;
- : string = "DELETE"

# Method.(to_string Options) ;;
- : string = "OPTIONS"

# Method.(to_string Trace) ;;
- : string = "TRACE"

# Method.(to_string Post) ;;
- : string = "POST"

# Method.(to_string Put) ;;
- : string = "PUT"

# Method.(to_string Patch) ;;
- : string = "PATCH"

# Method.(to_string Connect) ;;
- : string = "CONNECT"
```

## of_string

```ocaml
# Method.(of_string "Get") ;;
- : '_weak1 Method.t = Cohttp_eio.Method.Get

# Method.(of_string "Head") ;;
- : '_weak2 Method.t = Cohttp_eio.Method.Head

# Method.(of_string "DeleTE") ;;
- : '_weak3 Method.t = Cohttp_eio.Method.Delete

# Method.(of_string "Options") ;;
- : '_weak4 Method.t = Cohttp_eio.Method.Options

# Method.(of_string "Trace") ;;
- : '_weak5 Method.t = Cohttp_eio.Method.Trace

# Method.(of_string "Post") ;;
- : '_weak6 Method.t = Cohttp_eio.Method.Post

# Method.(of_string "Put") ;;
- : '_weak7 Method.t = Cohttp_eio.Method.Put

# Method.(of_string "Patch") ;;
- : '_weak8 Method.t = Cohttp_eio.Method.Patch

# Method.(of_string "Connect") ;;
- : '_weak9 Method.t = Cohttp_eio.Method.Connect
```

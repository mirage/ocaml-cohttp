# Setup

```ocaml
# module H = Cohttp_eio.Request.Header ;;
module H = Cohttp_eio.Request.Header

# module R = Cohttp_eio.Request ;;
module R = Cohttp_eio.Request
```

Add header key values using type-safe API - add, add_lazy.

```ocaml
# let h = H.empty ;;
val h : H.t = <abstr>

# let h = H.add R.User_agent "firefox" h ;;
val h : H.t = <abstr>

# let h = H.add R.Content_length 10 h ;;
val h : H.t = <abstr>
```

Retrieve values using type-safe API - find and find_opt.

```ocaml
# H.find R.User_agent h ;;
- : string = "firefox"

# H.find R.Content_length h ;;
- : int = 10

# H.find_opt R.Content_length h ;;
- : int option = Some 10
```

`find` and `find_opt` works even if we add the values using untyped add API.

```ocaml
# let h = H.add_key_val ~key:"content-length" ~value:"100" h;;
val h : H.t = <abstr>

# H.find_opt R.Content_length h ;;
- : int option = Some 100

```

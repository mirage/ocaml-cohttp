# Setup

```ocaml
# module R = Cohttp_eio.Request.R ;;
module R = Cohttp_eio.Request.R
# module H = R.Header ;;
module H = R.Header
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

Headers which are undefined can be retrieved via `Hdr "hdr"`

```ocaml
# let h = H.add_key_val ~key:"age" ~value:"9" h;;
val h : H.t = <abstr>

# H.find_opt (R.Hdr "age") h;;
- : string option = Some "9"
```

# Setup

```ocaml
# module H = Cohttp_eio.Request.Header ;;
module H = Cohttp_eio.Request.Header

# module R = Cohttp_eio.Request ;;
module R = Cohttp_eio.Request
```

Add header key values using type-safe API.

```ocaml
# let h = H.empty ;;
val h : H.t = <abstr>

# let h = H.add R.User_agent "firefox" h ;;
val h : H.t = <abstr>

# let h = H.add R.Content_length 10 h ;;
val h : H.t = <abstr>
```

Retrieve values using type-safe API.

```ocaml
# H.find R.User_agent h ;;
- : string = "firefox"

# H.find_opt R.Content_length h ;;
- : int option = Some 10
```


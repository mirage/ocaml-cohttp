# Cohttp_eio.Header unit tests

```ocaml
open Cohttp_eio
```

## Name Value

`canonical_name`

```ocaml
# Header.canonical_name "accept-encoding";;
- : Header.name = "Accept-Encoding"

# Header.canonical_name "content-length";;
- : Header.name = "Content-Length"

# Header.canonical_name "Age";;
- : Header.name = "Age"

# Header.canonical_name "cONTENt-tYPE";;
- : Header.name = "Content-Type"
```

`lname`

```ocaml
# let content_type = Header.lname "Content-type";;
val content_type : Header.lname = "content-type"

# let age = Header.lname "Age";;
val age : Header.lname = "age"
```

## Codec

User defined custom headers.

```ocaml
# type 'a Header.header += 
    | Header1 : int Header.header 
    | Header2: float Header.header
    ;;
type 'a Cohttp_eio.Header.header +=
    Header1 : int Header.header
  | Header2 : float Header.header

# let custom_codec : Header.Codec.t = object
  method header : type a. Header.lname -> a Header.header =
    fun nm ->
    match (nm :> string) with
    | "header1" -> Obj.magic Header1
    | "header2" -> Obj.magic Header2
    | _ -> Header.Codec.v#header nm

  method equal: type a b. a Header.header -> b Header.header -> (a, b) Header.eq option =
    fun a b ->
      match a, b with
      | Header1, Header1 -> Some Eq
      | Header2, Header2 -> Some Eq
      | _ -> Header.Codec.v#equal a b

  method decoder: type a. a Header.header -> a Header.decoder = function
    | Header1 -> int_of_string
    | Header2 -> float_of_string
    | hdr -> Header.Codec.v#decoder hdr

  method encoder: type a. a Header.header -> a Header.encoder = function
    | Header1 -> string_of_int
    | Header2 -> string_of_float
    | hdr -> Header.Codec.v#encoder hdr

  method name: type a. a Header.header -> Header.name =
    fun hdr -> 
      match hdr with
      | Header1 -> Header.canonical_name "header1"
      | Header2 -> Header.canonical_name "header2"
      | hdr -> Header.Codec.v#name hdr
  end ;;
val custom_codec : Header.Codec.t = <obj>
```

## Create 

`make` 

```ocaml
# let t = Header.(make Codec.v) ;;
val t : Header.t = <obj>
```

`of_name_values`

```ocaml
# let l : (string * string) list =
  [("Content-Type", "text/html"); ("Age", "40");
   ("Transfer-Encoding", "chunked"); ("Content-Length", "2000")];;
val l : (string * string) list =
  [("Content-Type", "text/html"); ("Age", "40");
   ("Transfer-Encoding", "chunked"); ("Content-Length", "2000")]

# let t3 = Header.(of_name_values t l);;
val t3 : Header.t = <obj>

# Header.length t3 = List.length l ;;
- : bool = true
```

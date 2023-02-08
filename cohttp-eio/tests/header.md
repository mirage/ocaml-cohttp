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

# let custom_codec : Header.codec = 
  object 
  inherit Header.codec as super
  method! header : type a. Header.lname -> a Header.header =
    fun nm ->
    match (nm :> string) with
    | "header1" -> Obj.magic Header1
    | "header2" -> Obj.magic Header2
    | _ -> super#header nm

  method! equal: type a b. a Header.header -> b Header.header -> (a, b) Header.eq option =
    fun a b ->
      match a, b with
      | Header1, Header1 -> Some Eq
      | Header2, Header2 -> Some Eq
      | _ -> super#equal a b

  method! decoder: type a. a Header.header -> a Header.decoder = function
    | Header1 -> int_of_string
    | Header2 -> float_of_string
    | hdr -> super#decoder hdr

  method! encoder: type a. a Header.header -> a Header.encoder = function
    | Header1 -> string_of_int
    | Header2 -> string_of_float
    | hdr -> super#encoder hdr

  method! name: type a. a Header.header -> Header.name =
    fun hdr -> 
      match hdr with
      | Header1 -> Header.canonical_name "header1"
      | Header2 -> Header.canonical_name "header2"
      | hdr -> super#name hdr
  end ;;
val custom_codec : Header.codec = <obj>

# let ch = Header.make custom_codec ;;
val ch : Header.t = <obj>

# Header.add ch Header1 1000;;
- : unit = ()

# Header.add ch Header2 100.232 ;; 
- : unit = ()

# Header.length ch ;;
- : int = 2

# Header.find ch Header2 ;;
- : float = 100.232

# Header.find ch Header1 ;;
- : int = 1000

# Header.name ch Header2 ;;
- : Header.name = "Header2"

# Header.encode ch Header1 1000 ;;
- : string = "1000"

# Header.to_name_values ch ;;
- : (Header.name * string) list =
[("Header2", "100.232"); ("Header1", "1000")]
```

`name`

```ocaml
# Header.(name ch Content_length) ;;
- : Header.name = "Content-Length"

# Header.name ch Header1 ;;
- : Header.name = "Header1"
```

## Create 

`make` 

```ocaml
# let t = Header.(make (new codec)) ;;
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

## Add

`add`, `add_lazy`, `add_value` and `add_name_value`

```ocaml
# Header.(add t Content_length 200) ;;
- : unit = ()

# Header.(add_lazy t Transfer_encoding (lazy [`chunked])) ;;
- : unit = ()

# Header.(add_value t (H age) "20") ;; 
- : unit = ()

# Header.(add_name_value t ~name:content_type ~value:"text/html") ;;
- : unit = ()
```

## Encode, Decode

`encode`

```ocaml
# Header.(encode t Content_length 10) ;;
- : string = "10"
```

## Find

`exists`, `find`, `find_opt`, `find_all`

```ocaml
# let f = object
  method f: type a. a Header.header -> a Header.value -> bool =
    fun t v ->
      let v = Header.decode v in
      match t, v with
      | Header.Content_length, 200 -> true
      | _ -> false
  end ;;
val f : < f : 'a. 'a Header.header -> 'a Header.value -> bool > = <obj>

# Header.exists t f ;;
- : bool = true

# Header.(find t Content_length) ;;
- : int = 200

# Header.(find t Transfer_encoding) ;;
- : [ `chunked | `compress | `deflate | `gzip ] list = [`chunked]

# Header.(find_opt t Content_length) ;;
- : int option = Some 200

# Header.(find t (H age)) ;;
- : string = "20"

# Header.(find t (H content_type)) ;;
- : string = "text/html"
```

`find_all` returns all values of a given header.

```ocaml
# let blah = Header.lname "blah";;
val blah : Header.lname = "blah"

# Header.length t ;;
- : int = 4

# Header.(add t (H blah) "blah 1"; add t (H blah) "blah 2"; add t (H blah) "blah 3");;
- : unit = ()

# Header.(find_all t (H blah)) |> List.map Header.decode ;;
- : string list = ["blah 3"; "blah 2"; "blah 1"]
```

## Update, Remove

`update`

First we add a new header item (H "blah2"), which we will remove via `update`. Additionally
we will update Content_length and Age header.

```ocaml
# let blah2 = Header.lname "blah2";;
val blah2 : Header.lname = "blah2"

# Header.(add t (H blah2) "blah2") ;;
- : unit = ()

# Header.(find_opt t (H blah2)) ;;
- : string option = Some "blah2"
```

Apply `update`.

```ocaml
# let f = object
  method f: type a. a Header.header -> a Header.value -> a Header.value option =
    fun h v ->
      let v' = Header.decode v in
      match h, v' with
      | Header.Content_length, 200 -> Some (Header.value (lazy 2000))
      | Header.H nm, "20" when ((nm :> string) = "age") -> Some (Header.value (lazy "40"))
      | Header.H nm, "blah2" when ((nm :> string) = "blah2") -> None
      | _ -> Some v
  end;;
val f :
  < f : 'a. 'a Header.header -> 'a Header.value -> 'a Header.value option > =
  <obj>

# Header.update t f ;;
- : unit = ()
```

`remove` with parameter `~all:false` - the default value - removes the last added header item.

```ocaml
# Header.length t ;;
- : int = 7

# Header.(remove t (H blah)) ;;
- : unit = ()

# Header.(find_opt t (H blah)) ;;
- : string option = Some "blah 1"

# Header.length t ;;
- : int = 6
```

`remove ~all:true` removes all occurences of a given header.


```ocaml
# Header.length t;;
- : int = 6

# Header.(remove ~all:true t (H blah)) ;;
- : unit = ()

# Header.(find_all t (H blah)) ;;
- : string Header.value list = []

# Header.length t ;;
- : int = 4
```

## Iter, Fold, Seq

`iter` - print Age header using `iter`.

```ocaml
# let f = object
  method f: type a. a Header.header -> a Header.value -> unit =
    fun h v ->
      let v = Header.decode v in
      let value = Header.encode t h v in
      let name = (Header.name t h :> string) in
      Printf.printf "\n%s: %s" name value
  end;;
val f : < f : 'a. 'a Header.header -> 'a Header.value -> unit > = <obj>

# Header.iter t f ;;
Content-Type: text/html
Age: 40
Transfer-Encoding: chunked
Content-Length: 2000
- : unit = ()
```

`fold_left

We get a list of headers in string form using `fold_left`.

```ocaml
# let f = object
  method f: type a. a Header.header -> a Header.value -> 'b -> 'b =
    fun h v acc ->
      let v = Header.decode v in
      match h with
      | Header.Content_length -> ("Content-Length", string_of_int v) :: acc
      | Header.H nm when ((nm :> string) = "age") -> ("Age", v) :: acc
      | _ -> acc
  end;;
val f :
  < f : 'a.
          'a Header.header ->
          'a Header.value -> (string * string) list -> (string * string) list > =
  <obj>

# Header.fold_left t f [];;
- : (string * string) list = [("Content-Length", "2000"); ("Age", "40")]
```

`to_seq`

```ocaml
# let headers = Header.to_seq t;;
val headers : Header.binding Seq.t = <fun>

# Seq.iter (fun (Header.B (h, v)) ->
    let v = Header.decode v in
    let value = Header.encode t h v in
    let name = (Header.name t h :> string) in
    Printf.printf "\n%s: %s" name value
  ) headers
  ;;
Content-Type: text/html
Age: 40
Transfer-Encoding: chunked
Content-Length: 2000
- : unit = ()
```

`to_name_values`

```ocaml
# let l = Header.to_name_values t ;;
val l : (Header.name * string) list =
  [("Content-Type", "text/html"); ("Age", "40");
   ("Transfer-Encoding", "chunked"); ("Content-Length", "2000")]
```

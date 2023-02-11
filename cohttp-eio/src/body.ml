class virtual writer =
  object
    method virtual write_body : Eio.Buf_write.t -> unit
    method virtual write_header : (name:string -> value:string -> unit) -> unit
  end

class none =
  object
    inherit writer
    method write_body _ = ()
    method write_header _ = ()
  end

let none = new none

let content_writer ~content ~content_type =
  let content_length = String.length content in
  object
    method write_body w = Buf_write.string w content

    method write_header f =
      f ~name:"Content-Length" ~value:(string_of_int content_length);
      f ~name:"Content-Type" ~value:content_type
  end

let form_values_writer assoc_list =
  let content = Uri.encoded_of_query assoc_list in
  content_writer ~content ~content_type:"application/x-www-form-urlencoded"

class virtual reader =
  object
    method virtual headers : Http.Header.t
    method virtual buf_read : Eio.Buf_read.t
  end

let ( let* ) o f = Option.bind o f

let read_content (t : #reader) =
  match Http.Header.get t#headers "Content-Length" with
  | Some l -> (
      try Some (Buf_read.take (int_of_string l) t#buf_read) with _ -> None)
  | None -> None

let read_form_values (t : #reader) =
  match
    let* content = read_content t in
    let* content_type = Http.Header.get t#headers "Content-Type" in
    if
      String.(
        equal (lowercase_ascii content_type) "application/x-www-form-urlencoded")
    then Some (Uri.query_of_encoded content)
    else None
  with
  | Some l -> l
  | None -> []

type part = {
  content_type: string option;
  name: string option;
  filename: string option;
  transfer_encoding : string option;
}

let quote s = "\"" ^ s ^ "\""

let map_default s f default =
  match s with
  | Some x -> f x
  | None -> default

let content_disposition { name ; filename } =
  match name, filename with
  | None, None -> []
  | _, _ ->
    let kv k v = [k ^ "=" ^ (quote v)] in
    let name = map_default name (kv "name") [] in
    let fname = map_default filename (kv "filename") [] in
    let v = [["form-data"] ; name ; fname]
            |> List.concat |> String.concat "; " in
    ["content-disposition", v]

let header_of_part part =
  Header.init () |> fun h ->
  map_default part.content_type (Header.add h "content-type") h |> fun h ->
  Header.add_list h (content_disposition part) |> fun h ->
  map_default part.transfer_encoding
    (Header.add h "content-transfer-encoding") h

let create_part ?content_type ?name ?filename
      ?transfer_encoding () =
  { content_type; name ; filename ; transfer_encoding }

let add_header ~boundary header =
  Header.add header "content-type"
    ("multipart/form-data; boundary=" ^ boundary)

let parts ~boundary parts =
  let parts =
    parts
    |> List.map (fun (part, body) ->
      [ `String ("\r\n--" ^ boundary ^ "\r\n")
      ; `Header (header_of_part part)
      ; `Part body ])
    |> List.concat in
  parts @ [`String ("\r\n--" ^ boundary ^ "--\r\n")]

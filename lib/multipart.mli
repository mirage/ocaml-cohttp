type part = {
  content_type: string option;
  name: string option;
  filename: string option;
  transfer_encoding : string option;
}

val add_header : boundary:string -> Header.t -> Header.t

val create_part : ?content_type:string
  -> ?name:string
  -> ?filename:string
  -> ?transfer_encoding:string
  -> unit
  -> part

val parts : boundary:string
  -> (part * 'body) list
  -> [`String of string | `Header of Header.t | `Part of 'body] list


(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Http_common;;
open Http_constants;;
open Http_types;;
open Printf;;

  (* remove all bindings of 'name' from hashtbl 'tbl' *)
let rec hashtbl_remove_all tbl name =
  if not (Hashtbl.mem tbl name) then
    raise (Header_not_found name);
  Hashtbl.remove tbl name;
  if Hashtbl.mem tbl name then hashtbl_remove_all tbl name
;;

class virtual message ~body ~headers ~version ~clisockaddr ~srvsockaddr =

  let ((cliaddr, cliport), (srvaddr, srvport)) =
    (Http_misc.explode_sockaddr clisockaddr,
     Http_misc.explode_sockaddr srvsockaddr)
  in

  object (self)

    val _contentsBuf = Buffer.create 1024
    val _headers = Hashtbl.create 11
    val mutable _version: version option = version

    initializer
      self#setBody body;
      self#addHeaders headers

    method version = _version
    method setVersion v = _version <- Some v

    method body = Buffer.contents _contentsBuf
    method setBody c =
      Buffer.clear _contentsBuf;
      Buffer.add_string _contentsBuf c
    method bodyBuf = _contentsBuf
    method setBodyBuf b =
      Buffer.clear _contentsBuf;
      Buffer.add_buffer _contentsBuf b
    method addBody s = Buffer.add_string _contentsBuf s
    method addBodyBuf b = Buffer.add_buffer _contentsBuf b

    method addHeader ~name ~value =
      let name = String.lowercase name in
      Http_parser_sanity.heal_header (name, value);
      Hashtbl.add _headers name value
    method addHeaders =
      List.iter (fun (name, value) -> self#addHeader ~name ~value)
    method replaceHeader ~name ~value =
      let name = String.lowercase name in
      Http_parser_sanity.heal_header (name, value);
      Hashtbl.replace _headers name value
    method replaceHeaders =
      List.iter (fun (name, value) -> self#replaceHeader ~name ~value)
    method removeHeader ~name =
      let name = String.lowercase name in
      hashtbl_remove_all _headers name
    method hasHeader ~name =
      let name = String.lowercase name in
      Hashtbl.mem _headers name
    method header ~name =
      if not (self#hasHeader name) then raise (Header_not_found name);
      let name = String.lowercase name in
      String.concat ", " (List.rev (Hashtbl.find_all _headers name))
    method headers =
      List.rev
        (Hashtbl.fold
          (fun name _ headers -> (name, self#header ~name)::headers)
          _headers
          [])

    method clientSockaddr = clisockaddr
    method clientAddr = cliaddr
    method clientPort = cliport

    method serverSockaddr = srvsockaddr
    method serverAddr = srvaddr
    method serverPort = srvport

    method private virtual fstLineToString: string
    method toString =
      self#fstLineToString ^  (* {request,status} line *)
      crlf ^
      (String.concat  (* headers, crlf terminated *)
        ""
        (List.map (fun (h,v) -> h ^ ": " ^ v ^ crlf) self#headers)) ^
      (sprintf "Content-Length: %d" (String.length self#body)) ^ crlf ^
      crlf ^
      self#body (* body *)
    method serialize outchan =
      output_string outchan self#toString;
      flush outchan

  end


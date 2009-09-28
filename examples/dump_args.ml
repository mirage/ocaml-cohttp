
(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2004> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Printf
open Http_types

let callback req outchan =
  let str = 
    (sprintf "request path = %s\n"  req#path) ^
    (sprintf "request GET params = %s\n"
      (String.concat ";"
        (List.map (fun (h,v) -> String.concat "=" [h;v]) req#params_GET))) ^
    (sprintf "request POST params = %s\n"
      (String.concat ";"
        (List.map (fun (h,v) -> String.concat "=" [h;v]) req#params_POST))) ^
    (sprintf "request ALL params = %s\n"
      (String.concat ";"
        (List.map (fun (h,v) -> String.concat "=" [h;v]) req#params))) ^
    (sprintf "request BODY = '%s'\n\n" req#body)
  in
  Http_daemon.respond ~code:(`Code 200) ~body: str outchan

let spec =
  { Http_daemon.default_spec with
      callback = callback;
      port = 9999;
  }

let _ = Http_daemon.main spec


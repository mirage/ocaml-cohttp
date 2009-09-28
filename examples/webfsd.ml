
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

open Http_types

let def_port = 80
let def_addr = "0.0.0.0"
let def_root = Sys.getcwd ()

let port = ref def_port
let addr = ref def_addr
let root = ref def_root
let argspec =
  [ "-p", Arg.Int (fun p -> port := p),
      "TCP port on which listen, default: " ^ string_of_int !port;
    "-a", Arg.String (fun a -> addr := a),
      "IP address on which listen, default: " ^ !addr;
    "-r", Arg.String (fun r -> root := r),
      "DocumentRoot, default: current working directory";
  ]

let _ =
  Arg.parse argspec (fun _ -> ()) "";
  let spec =
    { Http_daemon.default_spec with
        address = !addr;
        port = !port;
        root_dir = Some !root
    }
  in
  Http_daemon.Trivial.main spec


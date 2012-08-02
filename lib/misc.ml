(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>
	              2006-2009 Citrix Systems Inc.
	              2010 Thomas Gazagnaire <thomas@gazagnaire.com>

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

open Printf
open Lwt

open Types

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; 
                 "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let rfc822_of_float x =
  let open Unix in
  let time = gmtime x in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    days.(time.tm_wday) time.tm_mday
    months.(time.tm_mon) (time.tm_year+1900)
    time.tm_hour time.tm_min time.tm_sec

let date_822 () =
  rfc822_of_float (Unix.gettimeofday ())

let list_assoc_all key pairs =
  snd (List.split (List.filter (fun (k, v) -> k = key) pairs))

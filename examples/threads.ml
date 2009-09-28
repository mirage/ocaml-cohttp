
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

let m = Mutex.create ()
let m_locked = ref true

let critical f =
  Mutex.lock m;
    m_locked := true;
    Lazy.force f;
    m_locked := false;
  Mutex.unlock m

  (** ocaml's Thread.unlock suspend the invoking process if the mutex is already
  * unlocked, therefore we unlock it only if we know that it's currently locked
  *)
let safe_unlock _ _ = if !m_locked then Mutex.unlock m

let i = ref 10
let dump_i outchan =
  Http_daemon.respond ~body:(Printf.sprintf "i = %d\n" !i) outchan

let callback req outchan =
  match req#path with
  | "/incr" -> critical (lazy (incr i; dump_i outchan; Unix.sleep 5))
  | "/decr" -> critical (lazy (decr i; dump_i outchan; Unix.sleep 5))
  | "/get"  -> critical (lazy (dump_i outchan))
  | bad_request -> Http_daemon.respond_error outchan

let spec =
  { Http_daemon.default_spec with
      port = 9999;
      mode = `Thread;
      callback = callback;
      exn_handler = Some safe_unlock;
        (** ocaml-http's default exn_handler is Pervasives.ignore. This means
        * that threads holding the "m" mutex above may die without unlocking it.
        * Using safe_unlock as an exception handler we ensure that "m" mutex is
        * unlocked in case of exceptions (e.g. SIGPIPE) *)
  }

let _ = Http_daemon.main spec


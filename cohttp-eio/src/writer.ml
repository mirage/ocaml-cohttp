(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    All rights reserved.
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:
    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.
    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
module Optional_thunk : sig
  type t

  val none : t
  val some : (unit -> unit) -> t
  val call_if_some : t -> unit
end = struct
  type t = unit -> unit

  let none = Sys.opaque_identity (fun () -> ())

  let some f =
    if f == none then
      failwith
        "Optional_thunk: this function is not representable as a some value";
    f

  let call_if_some t = t ()
end

type t = {
  sink : Eio.Flow.sink;
  buf : Buffer.t;
  mutable wakeup : Optional_thunk.t;
}

let create sink =
  let buf = Buffer.create 0x1000 in
  { sink; buf; wakeup = Optional_thunk.none }

let wakeup t =
  let f = t.wakeup in
  t.wakeup <- Optional_thunk.none;
  Optional_thunk.call_if_some f

let write_string t s = Buffer.add_string t.buf s

let run t =
  let rec loop () =
    if Buffer.length t.buf > 0 then (
      Eio.Flow.copy_string (Buffer.contents t.buf) t.sink;
      Buffer.clear t.buf;
      loop ())
    else t.wakeup <- Optional_thunk.some loop
  in
  loop ()

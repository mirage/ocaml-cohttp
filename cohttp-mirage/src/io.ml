(*
 * Copyright (c) 2012-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * %%NAME%% %%VERSION%%
 *)

open Lwt.Infix

module Make (Channel : Mirage_channel.S) = struct
  module Input_channel = Input_channel.Make (Channel)

  type error =
    | Read_error of Channel.error
    | Write_error of Channel.write_error

  let pp_error f = function
    | Read_error e -> Channel.pp_error f e
    | Write_error e -> Channel.pp_write_error f e

  type 'a t = 'a Lwt.t
  type ic = Input_channel.t
  type oc = Channel.t
  type conn = Channel.flow

  exception Write_exn of Channel.write_error

  let () =
    Printexc.register_printer (function
      | Input_channel.Read_exn e ->
          Some (Format.asprintf "IO read error: %a" Channel.pp_error e)
      | Write_exn e ->
          Some (Format.asprintf "IO write error: %a" Channel.pp_write_error e)
      | _ -> None)

  let read_line ic = Input_channel.read_line_opt ic
  let read ic len = Input_channel.read ic len
  let refill ic = Input_channel.refill ic
  let with_input_buffer ic ~f = Input_channel.with_input_buffer ic ~f

  let write oc buf =
    Channel.write_string oc buf 0 (String.length buf);
    Channel.flush oc >>= function
    | Ok () -> Lwt.return_unit
    | Error `Closed -> Lwt.fail_with "Trying to write on closed channel"
    | Error e -> Lwt.fail (Write_exn e)

  let flush _ =
    (* NOOP since we flush in the normal writer functions above *)
    Lwt.return_unit

  let ( >>= ) = Lwt.( >>= )
  let return = Lwt.return

  let catch f =
    Lwt.try_bind f Lwt.return_ok (function
      | Input_channel.Read_exn e -> Lwt.return_error (Read_error e)
      | Write_exn e -> Lwt.return_error (Write_error e)
      | ex -> Lwt.fail ex)
end

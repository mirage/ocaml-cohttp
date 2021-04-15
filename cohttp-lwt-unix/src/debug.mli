(*{{{ Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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
  }}}*)

(** Debugging output for Cohttp Unix *)

val default_reporter : Lwt_unix.file_descr -> Format.formatter -> Logs.reporter
(** [default_reporter fd formatter] provides a reporter that sends the logging
    output to a specified file descriptor [fd] with a specified formatter
    [formatter]. For example, the code below enables logging at level [level] to
    stderr, using coloured output if possible.

    {[
      Fmt_tty.setup_std_outputs ();
      Logs.set_level ~all:true (Some level);
      Logs.set_reporter (Debug.default_reporter Lwt_unix.stderr Fmt.stderr)
    ]} *)

val activate_debug : unit -> unit
(** [activate_debug] enables debugging output that will be sent to standard
    error. *)

val debug_active : unit -> bool
(** [debug_active] returns true if [activate_debug] has been called and false
    otherwise *)

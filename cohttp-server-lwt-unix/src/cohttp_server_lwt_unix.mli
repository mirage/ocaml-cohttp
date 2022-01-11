module Input_channel : sig
  type t
end

module Io :
  Cohttp_lwt.S.IO
    with type ic = Input_channel.t
     and type oc = Lwt_io.output_channel
     and type conn = unit
     and type error = exn

include Cohttp_lwt.S.Server with module IO = Io

val handle_connection :
  t -> Lwt_io.input_channel * Lwt_io.output_channel -> unit Lwt.t

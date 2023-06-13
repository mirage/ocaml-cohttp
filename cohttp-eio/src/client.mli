include module type of Client_intf

include
  S
    with type 'a with_context = Eio.Net.t -> sw:Eio.Switch.t -> 'a
     and module Io = Io.IO

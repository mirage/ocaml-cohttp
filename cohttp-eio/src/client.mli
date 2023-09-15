type t

include
  Cohttp.Client.S
    with type 'a with_context = t -> sw:Eio.Switch.t -> 'a
     and type 'a io = 'a
     and type body = Body.t

val make : _ Eio.Net.t -> t

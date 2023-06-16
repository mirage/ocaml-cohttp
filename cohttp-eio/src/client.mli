include
  Cohttp.Client.S
    with type 'a with_context = Eio.Net.t -> sw:Eio.Switch.t -> 'a
     and type 'a io = 'a
     and type body = Body.t

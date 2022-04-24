include
  Cohttp_test.S
    with type 'a io = 'a Lwt.t
     and type body = Cohttp_lwt.Body.t
     and type ic = Cohttp_lwt_unix.Private.Input_channel.t
     and type oc = Lwt_io.output_channel

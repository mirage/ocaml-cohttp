open Lwt.Infix

module Server (Flow : Mirage_flow.S) = struct
  module Channel = Mirage_channel.Make (Flow)
  module HTTP_IO = Io.Make (Channel)
  module Input_channel = Input_channel.Make (Channel)
  include Cohttp_lwt.Make_server (HTTP_IO)

  let listen spec flow =
    let ch = Channel.create flow in
    Lwt.finalize
      (fun () -> callback spec flow (Input_channel.create ch) ch)
      (fun () -> Channel.close ch >|= fun _ -> ())
end

module Sleep = struct
  let sleep_ns ns =
    Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.)
end

include Cohttp_lwt.Make_client (Io) (Net) (Sleep)

let custom_ctx = Net.init

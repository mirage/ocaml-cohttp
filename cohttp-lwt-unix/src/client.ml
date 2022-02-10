module Sleep = struct
  type 'a promise = 'a Lwt.t
  let sleep_ns ns =
    Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.)
end

include Cohttp_lwt.Make_client (Io) (Net)

module Connection_cache = Cohttp_lwt.Connection_cache.Make (Connection) (Sleep)

let custom_ctx = Net.init

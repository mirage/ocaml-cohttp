include Make.Server(Conduit_mirage_flow)

let connect
  : type cfg t flow.
     (_, flow) Conduit_mirage.protocol
  -> (cfg, t, flow) Conduit_mirage.Service.service
  -> (cfg -> _ -> unit Lwt.t) Lwt.t
  = fun protocol service ->
    let server = fun cfg spec ->
      let handler flow =
        let flow = Conduit_mirage.pack protocol flow in
        listen spec flow in
      let _, run = Conduit_mirage.serve ~service ~handler cfg in
      run in
    Lwt.return server

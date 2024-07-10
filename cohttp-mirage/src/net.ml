module Make
    (P : Mirage_clock.PCLOCK)
    (R : Resolver_mirage.S)
    (S : Conduit_mirage.S) =
struct
  module Channel = Mirage_channel.Make (S.Flow)
  module Input_channel = Input_channel.Make (Channel)
  module Endpoint = Conduit_mirage.Endpoint (P)
  module IO = Io.Make (Channel)
  open IO

  type ctx = {
    resolver : R.t;
    conduit : S.t option;
    authenticator : X509.Authenticator.t option;
  }

  let sexp_of_ctx { resolver; _ } = R.sexp_of_t resolver

  let default_ctx =
    lazy { resolver = R.localhost; conduit = None; authenticator = None }

  type endp = Conduit.endp
  type client

  let tunnel = failwith "Unimplemented"
  let connect_client = failwith "Unimplemented"
  let resolve ~ctx uri = R.resolve_uri ~uri ctx.resolver

  let connect_endp ~ctx endp =
    Endpoint.client ?tls_authenticator:ctx.authenticator endp >>= fun client ->
    match ctx.conduit with
    | None -> failwith "conduit not initialised"
    | Some c ->
        S.connect c client >>= fun flow ->
        let ch = Channel.create flow in
        Lwt.return (flow, Input_channel.create ch, ch)

  let connect_uri ~ctx uri = resolve ~ctx uri >>= connect_endp ~ctx
  let close_in _ = ()
  let close_out _ = ()

  let close ic _oc =
    Lwt.ignore_result
    @@ Lwt.catch
         (fun () -> Input_channel.close ic)
         (fun e ->
           Logs.warn (fun f ->
               f "Closing channel failed: %s" (Printexc.to_string e));
           Lwt.return @@ Ok ())
end

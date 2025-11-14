open Cohttp_eio

let authenticator =
  match Ca_certs.authenticator () with
  | Ok x -> x
  | Error (`Msg m) ->
      Fmt.failwith "Failed to create system store X509 authenticator: %s" m

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs_threaded.enable ();
  Logs.Src.set_level Cohttp_eio.src (Some Debug)

let https ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("tls configuration problem: " ^ msg)
    | Ok tls_config -> tls_config
  in
  fun uri socket ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config socket

let get_request_exn ~sw client url =
  let resp, body = Client.get ~sw client url in
  match resp.status with
  | `OK ->
      Eio.traceln "%s"
      @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
  | otherwise -> Fmt.epr "Unexpected HTTP status: %a\n" Http.Status.pp otherwise

let run_client url cacert all_proxy no_proxy http_proxy https_proxy proxy_auth =
  let scheme_proxy =
    List.filter_map Fun.id
      [
        Option.map (fun p -> ("http", p)) http_proxy;
        Option.map (fun p -> ("https", p)) https_proxy;
      ]
  in
  let proxy_headers =
    Option.map
      (fun credential ->
        Http.Header.init_with "Proxy-Authorization"
          (Cohttp.Auth.string_of_credential credential))
      proxy_auth
  in

  Mirage_crypto_rng_unix.use_default ();

  Eio_main.run @@ fun env ->
  let net = env#net in

  let authenticator =
    match cacert with
    | None -> authenticator
    | Some pem ->
        (* Load a custom cacert from a file *)
        let fs = Eio.Stdenv.fs env in
        X509_eio.authenticator (`Ca_file Eio.Path.(fs / pem))
  in

  Client.set_proxies ?proxy_headers ?default_proxy:all_proxy
    ?no_proxy_patterns:no_proxy ~scheme_proxies:scheme_proxy ();

  let client = Client.make ~https:(Some (https ~authenticator)) net in

  Eio.traceln ">>> Make calls in sequence";
  Eio.Switch.run (fun sw ->
      get_request_exn ~sw client url;
      get_request_exn ~sw client url;
      get_request_exn ~sw client url);

  Eio.traceln ">>> Make calls concurrently";
  Eio.Switch.run (fun sw ->
      for _ = 0 to 5 do
        Eio.Fiber.fork ~sw (fun () -> get_request_exn ~sw client url)
      done);

  Eio.traceln ">>> Make calls in parallel";
  let dm = Eio.Stdenv.domain_mgr env in
  Eio.Fiber.all
    [
      (fun () ->
        Eio.Domain_manager.run dm (fun () ->
            Eio.Switch.run (fun sw -> get_request_exn ~sw client url)));
      (fun () ->
        Eio.Domain_manager.run dm (fun () ->
            Eio.Switch.run (fun sw -> get_request_exn ~sw client url)));
      (fun () ->
        Eio.Domain_manager.run dm (fun () ->
            Eio.Switch.run (fun sw -> get_request_exn ~sw client url)));
      (fun () ->
        Eio.Domain_manager.run dm (fun () ->
            Eio.Switch.run (fun sw -> get_request_exn ~sw client url)));
      (fun () ->
        Eio.Domain_manager.run dm (fun () ->
            Eio.Switch.run (fun sw -> get_request_exn ~sw client url)));
    ]

(* CLI Interface *)

let uri_conv =
  let parser s =
    match Uri.of_string s with
    | uri -> Ok uri
    | exception Failure _ -> Error "unable to parse URI"
  in

  let pp ppf u = Fmt.pf ppf "%s" (Uri.to_string u) in
  Cmdliner.Arg.Conv.make ~parser ~pp ~docv:"URI" ()

let credential_conv =
  let parser s =
    match Base64.encode s with
    | Ok s ->
        s |> Fmt.str "Basic %s" |> Cohttp.Auth.credential_of_string |> Result.ok
    | Error (`Msg m) -> Error m
  in
  let pp ppf c = Fmt.pf ppf "%s" (Cohttp.Auth.string_of_credential c) in
  Cmdliner.Arg.Conv.make ~parser ~pp ~docv:"CREDENTIAL" ()

let uri =
  Cmdliner.Arg.(
    required
    & pos 0 (some uri_conv) None
    & info [] ~docv:"URI"
        ~doc:"string of the remote address (e.g. https://ocaml.org)")

let all_proxy =
  let env = Cmdliner.Cmd.Env.info "ALL_PROXY" in
  Cmdliner.Arg.(
    value
    & opt (some uri_conv) None
    & info [ "all-proxy" ] ~env ~docv:"ALL_PROXY" ~doc:"Default proxy server")

let no_proxy =
  let env = Cmdliner.Cmd.Env.info "NO_PROXY" in
  Cmdliner.Arg.(
    value
    & opt (some string) None
    & info [ "no-proxy" ] ~env ~docv:"NO_PROXY"
        ~doc:"Exclude matching hosts from proxying")

let http_proxy =
  let env = Cmdliner.Cmd.Env.info "HTTP_PROXY" in
  Cmdliner.Arg.(
    value
    & opt (some uri_conv) None
    & info [ "http-proxy" ] ~env ~docv:"HTTP_PROXY"
        ~doc:"Proxy to use for requests using http")

let https_proxy =
  let env = Cmdliner.Cmd.Env.info "HTTPS_PROXY" in
  Cmdliner.Arg.(
    value
    & opt (some uri_conv) None
    & info [ "https-proxy" ] ~env ~docv:"HTTPS_PROXY"
        ~doc:"Proxy to use for requests using https")

let proxy_auth =
  Cmdliner.Arg.(
    value
    & opt (some credential_conv) None
    & info [ "proxy-auth" ] ~docv:"CREDENTIAL" ~doc:"Proxy credentials")

let cacert =
  Cmdliner.Arg.(
    value
    & opt (some string) None
    & info [ "cacert" ] ~docv:"PEM_FILE"
        ~doc:"Custom cert file for https authentication")

let cmd =
  let info =
    let version = Cohttp.Conf.version in
    let doc = "retrieve a remote URI contents" in
    Cmdliner.Cmd.info "client_proxy" ~version ~doc
  in

  let term =
    Cmdliner.Term.(
      const run_client
      $ uri
      $ cacert
      $ all_proxy
      $ no_proxy
      $ http_proxy
      $ https_proxy
      $ proxy_auth)
  in
  Cmdliner.Cmd.v info term

let () = exit @@ Cmdliner.Cmd.eval cmd

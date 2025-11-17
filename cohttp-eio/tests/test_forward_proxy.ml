(* Tests the core behaviour if the forward proxy *)

let () =
  Logs.set_level ~all:true @@ Some Logs.Debug;
  Logs.set_reporter (Logs_fmt.reporter ())

(* Used to pass data out of the server *)
module Req_data = struct
  let side_channel : Http.Request.t Eio.Stream.t = Eio.Stream.create 1
  let send t = Eio.Stream.add side_channel t

  let get () =
    if Eio.Stream.is_empty side_channel then failwith "no requests pending";
    Eio.Stream.take side_channel
end

let t_meth : Http.Method.t Alcotest.testable =
  Alcotest.testable Http.Method.pp (fun a b -> Http.Method.compare a b = 0)

(* The proxy server sends every request to the `Req_data` side channel and
   always responds with 200. *)
let run_proxy_server server_port net sw =
  let handler ~sw _conn request body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Eio.Fiber.fork ~sw (fun () -> Req_data.send request);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"" ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true ~reuse_port:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, server_port))
  and server = Cohttp_eio.Server.make ~callback:(handler ~sw) () in
  Eio.Fiber.fork_daemon ~sw @@ fun () ->
  let () = Cohttp_eio.Server.run socket server ~on_error:raise in
  `Stop_daemon

let () =
  (* Different tests run in parallel, so the port should be unique among
     tests *)
  let server_port = 4243 in
  let () =
    Cohttp_eio.Client.set_proxies
      ~default_proxy:
        (Uri.of_string @@ Printf.sprintf "http://127.0.0.1:%d" server_port)
      ()
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let () = run_proxy_server server_port env#net sw in
  let client =
    let noop_https_wrapper = Some (fun _ f -> f) in
    Cohttp_eio.Client.make ~https:noop_https_wrapper env#net
  in
  let get_success uri =
    let resp, _ = Cohttp_eio.Client.get ~sw client uri in
    match Http.Response.status resp with
    | `OK -> ()
    | unexpected ->
        Alcotest.failf "unexpected response from test_forward_proxy server %a"
          Http.Status.pp unexpected
  in

  (* TESTS CASES *)
  let direct_proxied_request () =
    (* When the remote host is over HTTP *)
    let uri = Uri.of_string "http://foo.org" in
    get_success uri;
    let req = Req_data.get () in
    let meth = Http.Request.meth req in
    Alcotest.(check' t_meth)
      ~msg:"should be a GET request" ~actual:meth ~expected:`GET;
    let host =
      let headers = Http.Request.headers req in
      Http.Header.get headers "host"
    in
    Alcotest.(check' (option string))
      ~msg:"should request from remote host" ~actual:host
      ~expected:(Some "foo.org")
  and tunnelled_proxied_request () =
    (* When the remote host is over HTTPS *)
    let uri = Uri.of_string "https://foo.org" in
    get_success uri;
    let req = Req_data.get () in
    let meth = Http.Request.meth req in
    Alcotest.(check' t_meth)
      ~msg:"should first initiate a CONNECT request" ~actual:meth
      ~expected:`CONNECT;
    let host =
      let headers = Http.Request.headers req in
      Http.Header.get headers "host"
    in
    Alcotest.(check' (option string))
      ~msg:"should request from remote host (with port)" ~actual:host
      ~expected:(Some "foo.org:443");

    let req' = Req_data.get () in
    let meth' = Http.Request.meth req' in
    Alcotest.(check' t_meth)
      ~msg:"should then send a GET request" ~actual:meth' ~expected:`GET;
    let host =
      let headers = Http.Request.headers req in
      Http.Header.get headers "host"
    in
    Alcotest.(check' (option string))
      ~msg:"should request from remote host (with port)" ~actual:host
      ~expected:(Some "foo.org:443")
  and unset_proxy () =
    let () = Cohttp_eio.Client.set_proxies ?default_proxy:None () in
    (* .invalid domains are guaranteed to not have hosts:
         https://www.rfc-editor.org/rfc/rfc2606 *)
    let uri = Uri.of_string "http://foo.invalid" in
    match Cohttp_eio.Client.get ~sw client uri with
    | exception Failure _ ->
        (* This should fail, since we are not using the proxy *)
        ()
    | unexepcted_resp, _ ->
        Alcotest.failf
          "Resolution of uri should have failed, but succeeded with %a"
          Http.Response.pp unexepcted_resp
  in
  Alcotest.run "cohttp-eio client"
    [
      ( "cohttp-eio forward proxy",
        [
          ("direct get", `Quick, direct_proxied_request);
          ("tunnelled proxied request", `Quick, tunnelled_proxied_request);
          ("unessting the proxy config", `Quick, unset_proxy);
        ] );
    ]

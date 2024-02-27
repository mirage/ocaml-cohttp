let () = Logs.set_reporter (Logs_fmt.reporter ())
and () = Logs.Src.set_level Cohttp_eio.src (Some Debug)

let ( / ) = Eio.Path.( / )

(* To stream a file, we take the extra [writer] argument explicitly.
   This means that we stream the response while the function is still
   running and the file is still open. *)
let handler dir _socket request _body writer =
  let path =
    Http.Request.resource request
    |> String.split_on_char '/'
    |> List.filter (( <> ) "")
    |> String.concat "/"
  in
  let path = if path = "" then "index.html" else path in
  Eio.Path.with_open_in (dir / path) @@ fun flow ->
  Cohttp_eio.Server.respond () ~status:`OK
    ~headers:(Http.Header.of_list [ ("content-type", "text/html") ])
    ~body:flow writer

let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

let () =
  let port = ref 8080 in
  Arg.parse
    [ ("-p", Arg.Set_int port, " Listening port number(8080 by default)") ]
    ignore "An HTTP/1.1 server";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* Restrict to current directory: *)
  let htdocs = Eio.Stdenv.cwd env in
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
  and server = Cohttp_eio.Server.make ~callback:(handler htdocs) () in
  Cohttp_eio.Server.run socket server ~on_error:log_warning

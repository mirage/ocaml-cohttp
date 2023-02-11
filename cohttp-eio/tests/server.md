# Server

```ocaml
open Cohttp_eio

let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8081)

let now = ref 1623940778.27033591

let fake_clock real_clock = object (_ : #Eio.Time.clock)
  method now = !now
  method sleep_until time =
    Eio.Time.sleep_until real_clock time;
    now := max !now time
end

let handler req =
  match Request.resource req with
  | "/" -> Response.text "root"
  | "/upload" -> (
    match Body.read_content req with
    | Some a -> Response.text a
    | None -> Response.bad_request
    )
  | _ -> Response.not_found

exception Graceful_shutdown
```

## Server.run

```ocaml
# Eio_main.run @@ fun env ->
  try
    Eio.Fiber.both 
      (fun () -> Server.run ~port:8081 ~on_error:raise env#domain_mgr env#net (fake_clock env#clock) handler)
      (fun () ->
        Eio.Switch.run @@ fun sw ->
        let client = Client.make sw env#net in
        let res = Client.get client "localhost:8081" in
        Eio.traceln "Route: /";
        Eio.traceln "%a" Http.Header.pp_hum (Response.headers res);
        Eio.traceln "%s" (Body.read_content res |> Option.get);
        Eio.traceln "";
        Eio.traceln "Route: /upload";
        let body = Body.content_writer ~content:"hello world" ~content_type:"text/plain" in
        let res = Client.post client body "localhost:8081/upload" in
        Eio.traceln "%a" Http.Header.pp_hum (Response.headers res);
        Eio.traceln "%s" (Body.read_content res |> Option.get);

        raise Graceful_shutdown 
      )
  with Graceful_shutdown -> () ;;
+Route: /
+Header {
+Content-Type = "text/plain; charset=UTF-8"; Content-Length = "4";
+Date = "Thu, 17 Jun 2021 14:39:38 GMT" }
+root
+
+Route: /upload
+Header {
+Content-Type = "text/plain; charset=UTF-8"; Content-Length = "11";
+Date = "Thu, 17 Jun 2021 14:39:38 GMT" }
+hello world
- : unit = ()
```

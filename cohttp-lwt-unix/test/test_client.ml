open Lwt.Infix
open OUnit

open Cohttp
open Cohttp_lwt_unix
module Body = Cohttp_lwt.Body

(* This file implements a key-value store utilising HTTP as RPC-Interface.
 * The URI path is used as key. The body is used as value.
 * The Backend is a Hashtbl.t.
 * The Hashtbl functions translate to HTTP methods as follows:
 * replace -> PUT
 * get -> GET
 * mem -> HEAD
 * remove -> DELETE *)

(* This is the server side. *)
let test_server tests =
  let store = Hashtbl.create ~random:true 8 in
  let spec req body =
    Body.to_string body >>= fun body ->
    let uri = Request.uri req in
    let path = Uri.path uri in
    begin match Request.meth req with
    | `PUT ->
      let status = if Hashtbl.mem store path then `Created else `No_content in
      Hashtbl.replace store path body;
      Server.respond_string ~status ~body:"" ()
    | `DELETE ->
      if Hashtbl.mem store path
      then begin
        Hashtbl.remove store path;
        Server.respond_string ~status:`No_content ~body:"" ()
      end
      else Server.respond_not_found ~uri ()
    | `HEAD ->
      if Hashtbl.mem store path
      then Server.respond_string ~body:"" ~status:`OK ()
      else Server.respond_string ~body:"" ~status:`Not_found ()
    | `GET ->
      begin match Hashtbl.find_opt store path with
      | Some body ->
        Server.respond_string ~status:`OK ~body ()
      | None ->
        Server.respond_not_found ~uri ()
      end
    | meth ->
      Server.respond_string
        ~status:`Method_not_allowed
        ~body:("Unsupported method " ^ Code.string_of_method meth)
        ()
    end
    >|= Cohttp_lwt_unix_test.response
  in
  Cohttp_lwt_unix_test.test_server_s
    ~name:"Mutable ressources"
    spec tests

(* Client side of the RPC interface *)
let methods (handler :?body:_ -> _ -> _) uri =
  let put k v =
    let body = Body.of_string v in
    Request.make ~meth:`PUT @@ Uri.with_path uri k
    |> handler ~body >>= fun (res, body) ->
    Body.drain_body body >>= fun () ->
    match Response.status res with
    | `Created | `No_content | `OK -> Lwt.return_unit
    | _ -> Lwt.fail_with "put failed"
  and get k =
    Request.make ~meth:`GET @@ Uri.with_path uri k
    |> handler >>= fun (res, body) ->
    match Response.status res with
    | `OK | `No_content -> Body.to_string body
    | _ -> Body.drain_body body >>= fun () -> Lwt.fail Not_found
  and delete k =
    Request.make ~meth:`DELETE @@ Uri.with_path uri k
    |> handler >>= fun (res, body) ->
    Body.drain_body body >>= fun () ->
    match Response.status res with
    | `OK | `No_content -> Lwt.return_unit
    | _ -> Lwt.fail Not_found
  and mem k =
    Request.make ~meth:`HEAD @@ Uri.with_path uri k
    |> handler >>= fun (res, body) ->
    Body.drain_body body >|= fun () ->
    match Response.status res with
    | `OK | `No_content -> true
    | _ -> false
  in
  put, get, delete, mem

(* Use the key-value store implemented above to store a map of numbers 0-6 to
 * corresponding days of the week. *)
let tests handler uri =
  let put, get, delete, mem =
    methods handler uri
  in
  let weekdays =
  [| "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday";
     "Saturday"; "Sunday" |]
  in
  (* Pipelining is facilitated by not binding (>>=) on responses, but joining them. *)
  Array.mapi (fun i d -> put (string_of_int i) d) weekdays |> Array.to_list
  |> Lwt.join >>= fun () ->
  List.init 7 (fun i -> mem (string_of_int i) >|= assert_bool "mem")
  |> Lwt.join >>= fun () ->
  mem "7" >>= fun b -> assert_bool "mem" (not b);
  List.init 7 (fun i -> get (string_of_int i) >|= assert_equal weekdays.(i))
  |> Lwt.join >>= fun () ->
  List.init 7 (fun i -> delete (string_of_int i))
  |> Lwt.join

(* Now run those tests through the different low- and highlevel interfaces *)

module Connection = Cohttp_lwt_unix.Connection

(* Use the high-level Client interface *)
let test_client uri =
  let uri = Uri.with_host uri (Some "localhost") in

  (* high-level convenience functions. *)
  Client.put ~body:(`String "Spring") (Uri.with_path uri "season") >>= fun _ ->
  Client.get (Uri.with_path uri "season") >>= fun (_response, body) ->
  Body.to_string body >>= fun body ->
  assert_equal ~printer:Fun.id "Spring" body;

  (* simple request function accepting custom requests. *)
  let handler ?body request = Client.request ?body request in
  tests handler uri

(* The Client.{request, get, put, ...} functions by default use a new
 * connection for each request. In a high-latency environment or when
 * connection setup is expensive due to TLS one might want to use a persistent
 * connection. This can be done by using the lower-level Connection or
 * Connection_cache interface or by providing a different default connection
 * cache to the Client module. *)

(* Simple case: The server is known to support pipelining and won't close the
 * connection unexpectantly (timeout or number of requests may be limited). *)
let test_persistent uri =
  let uri = Uri.with_host uri (Some "localhost") in
  Connection.Net.resolve ~ctx:Connection.Net.default_ctx uri (* resolve hostname. *)
  >>= Connection.connect ~persistent:true >>= fun connection -> (* open connection *)
  let handler ?body request = Connection.request connection ?body request in
  (* a partial application would work nicely, too:
  let handler = Connection.request connection in *)
  tests handler uri >|= fun () ->
  Connection.close connection

(* In case persistent connections are not used, a new connection needs to be
 * opened for each request.
 * This might result in a massive amount of parallel connections. *)
let test_non_persistent uri =
  let uri = Uri.with_host uri (Some "localhost") in
  (* the resolved endpoint may be buffered to avoid stressing the resolver: *)
  Connection.Net.resolve ~ctx:Connection.Net.default_ctx uri >>= fun endp ->
  let handler ?body request =
    Connection.connect ~persistent:false endp >>= fun connection ->
    Connection.request connection ?body request
  in
  tests handler uri

(* Now the difficult case: We want to use persistent connections, but they may
 * not be supported or the server may close the connection unexpectedly.
 * In such a case the pending requests will fail with Connection.Retry. *)
let test_unknown uri =
  let uri = Uri.with_host uri (Some "localhost") in
  Connection.Net.resolve ~ctx:Connection.Net.default_ctx uri
  >>= fun endp -> (* buffer resolved endp *)
  Connection.connect ~persistent:false endp >>= fun c ->
  let connection = ref c in (* reference to open connection *)
  let rec handler ?body request =
    Lwt.catch
      (fun () -> Connection.request !connection ?body request)
      (function
        | Connection.Retry ->
          (* We may safely retry. The request has not yet been processed by the
           * remote host. The connection has been gracefully shutdown. *)
          assert_bool "connection closed" (Connection.is_closed !connection);
          Connection.connect ~persistent:false endp >>= fun c ->
          connection := c;
          begin match body with
          (* Still, body may have been (partially) consumed and needs re-creation. *)
          | Some `Stream _ -> Lwt.fail Connection.Retry
          | None | Some (`Empty | `String _ | `Strings _) -> handler ?body request
          end
        | e -> Lwt.fail e
      )
  in
  tests handler uri

(* In that difficult case one might be better of using a Connection_cache which
 * will take care of those trivial retries and reconnecting: *)

module Cache = Cohttp_lwt_unix.Connection_cache

let test_cache uri =
  let uri = Uri.with_host uri (Some "localhost") in
  let cache = Cache.create () in
  let handler = Cache.request cache in (* <- this is a partial application *)
  tests handler uri

(* In case you want to stick with the convenience Client.{request, get, put, ...}
 * functions, you may set another default connection cache: *)

let test_client_cached uri =
  let cache = Cache.create () in
  Client.set_cache (Cache.request cache);
  test_client uri

let tests uri =
  [ ("high-level interface", fun () -> test_persistent uri)
  ; ("persistent connection", fun () -> test_persistent uri)
  ; ("non-persistent connection", fun () -> test_non_persistent uri)
  ; ("unknown persistence connection", fun () -> test_unknown uri)
  ; ("cache", fun () -> test_cache uri)
  ; ("high-level cached interface", fun () -> test_client_cached uri)
  ]

let _ = test_server tests |> Cohttp_lwt_unix_test.run_async_tests |> Lwt_main.run

(* open Utils *)

(* TODO: Should the connections be scoped to a single client, instead of to a
   switch, requiring clients to be resource carying (and thus to require a switch for creating)?

   This is the approach in https://learn.microsoft.com/en-us/dotnet/fundamentals/networking/http/httpclient-guidelines
  *)

(* TODO: Instead of fiber local storage, we could use a background process that
   receives calls via a stream *)

(* Implementation notes

    - Every switch defines a root fiber (relative to that switch). See
      https://github.com/ocaml-multicore/eio/blob/c44ee5ce96c120b7ccc23a12d241dc8672e2888f/lib_eio/core/switch.ml#L123
    - Switches are not threadsafe (in general). See TODO
    - Fiber local variables ensure that the state stored is only available for 

 *)

open Eio.Std
module Connection = Client_connection

type conn = { conn : Connection.t; created : Mtime.t }

type t = {
  keep : Mtime.Span.t;
  parallel : int;
  mutex : Eio.Mutex.t;
  lru : (Eio.Net.Sockaddr.stream, conn Eio.Pool.t) Hashtbl.t; (* TODO *)
  sw : Eio.Switch.t;
  time : Eio.Time.Mono.ty Eio.Std.r;
}

(* Following the example of
   https://github.com/apache/httpcomponents-client/blob/06634831effa5c83536f4ed035fb13af8ffc78ea/httpclient5/src/main/java/org/apache/hc/client5/http/impl/io/PoolingHttpClientConnectionManager.java#L118-L119 *)
(* let default_max_total_connections = 25 *)

(* Following
   https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Connection_management_in_HTTP_1.x#domain_sharding *)
(* let default_max_connections_per_host = 6 *)

let key = Fiber.create_key ()

let with_cache ?(keep = 60_000_000_000L) ?(parallel = 6) ~time f =
  Eio.Switch.run @@ fun sw ->
  let keep = Mtime.Span.of_uint64_ns keep in
  let time = (time :> Eio.Time.Mono.ty Eio.Std.r) in
  Fiber.with_binding key
    {
      parallel;
      keep;
      mutex = Eio.Mutex.create ();
      sw;
      lru = Hashtbl.create 100;
      time;
    }
    (fun () -> f sw)

let get () = Eio.Fiber.get key

(* let remove {hashtbl; mutex} addr = *)
(*   Eio.Mutex.use_rw ~protect:true mutex (fun () -> Hashtbl.remove hashtbl addr) *)

let use ({ parallel; sw; lru; mutex; keep; time } : t) ~https ~net addr_info f =
  let protect = true in
  let conns =
    Eio.Mutex.use_rw ~protect mutex @@ fun () ->
    let addr = Connection.to_address addr_info in
    traceln "Looking up connection for %a" Eio.Net.Sockaddr.pp addr;
    match Hashtbl.find_opt lru addr with
    | Some conns -> conns
    | None ->
        let conns =
          Eio.Pool.create
            ~validate:(fun c ->
              let now = Eio.Time.Mono.now time in
              Mtime.Span.is_longer ~than:keep @@ Mtime.span c.created now)
            ~dispose:(fun c -> Eio.Net.close c.conn)
            parallel
            (fun () ->
              {
                conn = Connection.make ~sw ~https net addr_info;
                created = Eio.Time.Mono.now time;
              })
        in
        Hashtbl.add lru addr conns;
        conns
  in
  Eio.Pool.use conns (fun c -> f c.conn)

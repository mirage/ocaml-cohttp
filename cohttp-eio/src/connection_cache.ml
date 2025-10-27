(* TODO open Eio.Std *)
open Utils

module Connection : sig
  type t
  val create : ?limit:int -> sw:Eio.Switch.t -> S.connection -> t
  (** TODO(doc): Create a new connection *)

  val close : t -> unit
  (** TODO(doc): Close the connection *)

  (* val send : ?body:Body.t -> request:Http.Request.t -> t -> (Http.Response.t * Body.t) *)
  (** TODO(doc): Send a request and wait for the response *)

  val call : headers:Http.Header.t option ->
             body:Body.t option ->
             chunked:bool ->
             absolute_form:bool option ->
             Cohttp.Code.meth ->
             Uri.t ->
             t ->
             Http.Response.t * Body.t
  (** TODO(doc): Form and send a request, and wait for the response *)
end = struct

  type req = {
      body:Body.t option;
      request: Http.Request.t;
      resolver: (Http.Response.t * Body.t) Eio.Promise.u
    }

  (* A connection is a forked process that reads from a stream of messages.
     its principle type a function that lets users send messages along the stream.  *)
  type t = req option -> unit

  (** TODO: Doc

      - Send [None] to terminate the process *)
  let create ?(limit=max_int) ~sw socket : t =
    let request_stream : req option Eio.Stream.t = Eio.Stream.create limit in
    (* By giving constructors a send function, we provide a write-only stream, 
       making it impossible for other parts of the program to steal from our stream *)
    let send_request req = Eio.Stream.add request_stream req in
    let () =
      Eio.Buf_write.with_flow socket begin fun output ->
        let loop () =
          match Eio.Stream.take request_stream with
          | None -> () (* Stream "closed", so we terminate *)
          | Some {request; body; resolver} ->
             let () =
               Io.Request.write ~flush:false
                 (fun writer ->
                   match body with
                   | None -> ()
                   | Some body -> flow_to_writer body writer Io.Request.write_body)
                 request output
             in
             let input = Eio.Buf_read.of_flow ~max_size:max_int socket in
             match Io.Response.read input with
             | `Eof -> failwith "connection closed by peer"
             | `Invalid reason -> failwith reason
             | `Ok response ->
                let response = match Cohttp.Response.has_body response with
                  | `No -> (response, Eio.Flow.string_source "")
                  | `Yes | `Unknown ->
                     let body =
                       let reader = Io.Response.make_body_reader response input in
                       flow_of_reader (fun () -> Io.Response.read_body_chunk reader)
                     in
                     (response, body)
                in
                Eio.Promise.resolve resolver response
        in
        (* TODO: should we instead fork this loop from outside? *)
        Eio.Fiber.fork ~sw loop
        end
    in
    send_request

  let close (t : t) = t None 

  let send ?body ~request (t : t) : (Http.Response.t * Body.t) Eio.Promise.t =
    let promise, resolver = Eio.Promise.create () in
    t (Some {body; request; resolver});
    promise

  let call ~headers ~body ~chunked ~absolute_form meth uri conn =
    let body_length =
      if chunked then None
      else
        match body with
        | None -> Some 0L
        | Some (Eio.Resource.T (body, ops)) ->
           let module X = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
           List.find_map
             (function
              | Body.String m -> Some (String.length (m body) |> Int64.of_int)
              | _ -> None)
             X.read_methods
    in
    let request =
      Cohttp.Request.make_for_client ?headers
        ~chunked:(Option.is_none body_length)
        ?body_length ?absolute_form meth uri
    in
    Eio.Promise.await @@ send ?body ~request conn
end


module No_cache = struct
  type t = unit

  let call () : S.cache_call =
   fun t ~sw ?headers ?body ?(chunked = false) ?absolute_form meth uri ->
    let _addr, socket = t ~sw uri in
    let conn = Connection.create ~sw socket in
    let resp = Connection.call ~headers ~body ~chunked ~absolute_form meth uri conn in
    Connection.close conn;
    resp

  let create () = ()
end

module Cache = struct
  module Tbl = struct
    type t = {
      (* We only cache for [stream] sockets, because we only care for what we can connect to via [Eio.Net.connect] *)
      hashtbl : (Eio.Net.Sockaddr.stream, Connection.t) Hashtbl.t;
      mutex : Eio.Mutex.t;
    }

    let get k t =
      Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find_opt k t.hashtbl)

    (* let create limit = *)
  end

  type t = { cache : Tbl.t }

  (* let get_connection t endpoint = *)
  (*   match lookup_in_map endpoint with *)
  (*   | Some c -> c *)
  (*   | None _ -> add_to_mapp endpoint *)
  let _f (t : t) =
    let _ = Tbl.get in
    t.cache

  let create = raise (Failure "TODO")
  let call = raise (Failure "TODO")
end

module Proxy = struct
  type t = unit

  let create = raise (Failure "TODO")
  let call = raise (Failure "TODO")
end

(* TODO open Eio.Std *)
open Utils

module No_cache = struct
  type t = unit

  let call () : S.cache_call =
   fun t ~sw ?headers ?body ?(chunked = false) ?absolute_form meth uri ->
    let socket = t ~sw uri in
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
    Eio.Buf_write.with_flow socket @@ fun output ->
    let () =
      Eio.Fiber.fork ~sw @@ fun () ->
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
    | `Ok response -> (
        match Cohttp.Response.has_body response with
        | `No -> (response, Eio.Flow.string_source "")
        | `Yes | `Unknown ->
            let body =
              let reader = Io.Response.make_body_reader response input in
              flow_of_reader (fun () -> Io.Response.read_body_chunk reader)
            in
            (response, body))

  let create () = ()
end

module Cache = struct
  module M = struct
    (* We need to cache based on the Sockaddrs that are used here *)
    (* https://github.com/tarides/ocaml-cohttp/blob/5465d00bfcee55fa077e0cd1f19964457bb88db6/cohttp-eio/src/client.ml#L53-L61 *)
    (* which means we need to expose those to the cache. *)

    (* cohttp-lwt-unix's caches based on [Conduit.endp].
       The equivalent in Eio is Eio.Net.Sockaddr.stream. *)
    type 'v t = {
      (* We only cache for [stream] sockets, because we only care for what we can connect to via [Eio.Net.connect] *)
      hashtbl : (Eio.Net.Sockaddr.stream, 'v) Hashtbl.t;
      mutex : Eio.Mutex.t;
    }

    let get k t =
      Eio.Mutex.use_ro t.mutex (fun () -> Hashtbl.find_all k t.hashtbl)
  end

  type t = { cache : [ `TODO ] M.t }

  (* let get_connection t endpoint = *)
  (*   match lookup_in_map endpoint with *)
  (*   | Some c -> c *)
  (*   | None _ -> add_to_mapp endpoint *)
  let _f (t : t) =
    let _ = M.get in
    t.cache

  let create = raise (Failure "TODO")
  let call = raise (Failure "TODO")
end

module Proxy = struct
  type t = unit

  let create = raise (Failure "TODO")
  let call = raise (Failure "TODO")
end

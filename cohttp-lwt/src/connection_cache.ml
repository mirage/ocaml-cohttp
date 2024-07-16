exception Retry = Connection.Retry

module Make_no_cache (Connection : S.Connection) = struct
  module Net = Connection.Net
  module IO = Net.IO
  open IO

  type t = S.call

  let call = Fun.id

  let create ?(ctx = Lazy.force Net.default_ctx) () ?headers ?body
      ?absolute_form meth uri =
    Net.resolve ~ctx uri
    (* TODO: Support chunked encoding without ~persistent:true ? *)
    >>= Connection.connect ~ctx ~persistent:true
    >>= fun connection ->
    let res =
      Connection.call connection ?headers ?body ?absolute_form meth uri
    in
    (* this can be simplified when https://github.com/mirage/ocaml-conduit/pull/319 is released. *)
    Lwt.dont_wait
      (fun () ->
        res >>= fun (_, body) ->
        (match body with
        | `Empty | `String _ | `Strings _ -> Lwt.return_unit
        | `Stream stream -> Lwt_stream.closed stream)
        >>= fun () ->
        Connection.close connection;
        Lwt.return_unit)
      (function Retry -> () | e -> raise e);
    res
end

module Make (Connection : S.Connection) (Sleep : S.Sleep) = struct
  module Net = Connection.Net
  module IO = Net.IO
  open IO

  type ctx = Net.ctx

  type t = {
    cache : (Net.endp, Connection.t) Hashtbl.t;
    ctx : ctx;
    keep : int64;
    retry : int;
    parallel : int;
    depth : int;
  }

  let create ?(ctx = Lazy.force Net.default_ctx) ?(keep = 60_000_000_000L)
      ?(retry = 2) ?(parallel = 4) ?(depth = 100) () =
    {
      cache = Hashtbl.create ~random:true 10;
      ctx;
      keep;
      retry;
      parallel;
      depth;
    }

  let rec get_connection self endp =
    let finalise connection =
      let rec remove keep =
        let current = Hashtbl.find self.cache endp in
        Hashtbl.remove self.cache endp;
        if current == connection then
          List.iter (Hashtbl.add self.cache endp) keep
        else remove (current :: keep)
      in
      remove [];
      Lwt.return_unit
    in
    let create () =
      let connection = Connection.create ~finalise ~ctx:self.ctx endp
      and timeout = ref Lwt.return_unit in
      let rec busy () =
        Lwt.cancel !timeout;
        if Connection.length connection = 0 then (
          timeout :=
            Sleep.sleep_ns self.keep >>= fun () ->
            Connection.close connection;
            (* failure is ignored *)
            Lwt.return_unit);
        Lwt.on_termination (Connection.notify connection) busy
      in
      busy ();
      connection
    in
    match Hashtbl.find_all self.cache endp with
    | [] ->
        let connection = create () in
        Hashtbl.add self.cache endp connection;
        Lwt.return connection
    | conns -> (
        let rec search length = function
          | [ a ] -> (a, length + 1)
          | a :: b :: tl when Connection.length a < Connection.length b ->
              search (length + 1) (a :: tl)
          | _ :: tl -> search (length + 1) tl
          | [] -> assert false
        in
        match search 0 conns with
        | shallowest, _ when Connection.length shallowest = 0 ->
            Lwt.return shallowest
        | _, length when length < self.parallel ->
            let connection = create () in
            Hashtbl.add self.cache endp connection;
            Lwt.return connection
        | shallowest, _ when Connection.length shallowest < self.depth ->
            Lwt.return shallowest
        | _ ->
            Lwt.try_bind
              (fun () -> Lwt.choose (List.map Connection.notify conns))
              (fun _ -> get_connection self endp)
              (fun _ -> get_connection self endp))

  let call self ?headers ?body ?absolute_form meth uri =
    Net.resolve ~ctx:self.ctx uri >>= fun endp ->
    let rec request retry =
      get_connection self endp >>= fun conn ->
      Lwt.catch
        (fun () -> Connection.call conn ?headers ?body ?absolute_form meth uri)
        (function
          | Retry -> (
              match body with
              | Some (`Stream _) -> raise Retry
              | None | Some `Empty | Some (`String _) | Some (`Strings _) ->
                  if retry <= 0 then raise Retry else request (retry - 1))
          | e -> Lwt.reraise e)
    in
    request self.retry
end

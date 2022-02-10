exception Retry = Connection.Retry

module Make_no_cache (Connection : S.Connection) : sig
  include S.Connection_cache with module IO = Connection.Net.IO

  val create : ?ctx:Connection.Net.ctx -> unit -> t
end
=
struct
  module Net = Connection.Net
  module IO = Net.IO
  open IO

  type t = ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) IO.t

  let request = Fun.id

  let create ?(ctx = Net.default_ctx) () ?body req =
    Net.resolve ~ctx (Cohttp.Request.uri req)
    >>= Connection.connect ~ctx ~persistent:false
    >>= fun connection ->
    let res = Connection.request connection ?body req in
    (* this can be simplified when https://github.com/mirage/ocaml-conduit/pull/319 is released. *)
    Lwt.async begin fun () ->
      res >>= fun (_, body) ->
      match body with
      | `Empty | `String _ | `Strings _ -> Lwt.return_unit
      | `Stream stream ->
        Lwt_stream.closed stream >>= fun () ->
        Connection.close connection;
        Lwt.return_unit
    end;
    res
end

module Make (Connection : S.Connection) (Sleep : S.Sleep with type 'a promise = 'a Lwt.t) : sig
  include S.Connection_cache with module IO = Connection.Net.IO

  val create :
    ?ctx:Connection.Net.ctx ->
    ?keep:int64 ->
    ?retry:int ->
    ?parallel:int ->
    ?depth:int ->
    unit -> t
end
=
struct
  module Net = Connection.Net
  module IO = Net.IO
  open IO

  type ctx = Net.ctx

  module Endp_map = Map.Make (Net.Endp)

  (* type t = < request : ?body:Body.t -> Cohttp.Request.t -> (Cohttp.Response.t * Body.t) IO.t > *)
  type t =
    { mutable cache : Connection.t list Endp_map.t
    ; ctx : ctx
    ; keep : int64
    ; retry : int
    ; parallel : int
    ; depth : int
    }

  (*
   * Select from existing connections by hostname, not IP.
   * One hostname may resolve to multiple IP addresses.
   * In such a case we want only one connection.
   * The downside is that some resolver redirections may break.
   *)

  let create ?(ctx = Net.default_ctx) ?(keep=60_000_000_000L) ?(retry=2) ?(parallel=4) ?(depth=100) () =
    { cache = Endp_map.empty
    ; ctx ; keep ; retry ; parallel ; depth
    }

  let rec get_connection self endp =
    let finalise connection =
      let conns = Endp_map.find endp self.cache in
      let conns = List.filter ((!=) connection) conns in
      if conns = []
      then self.cache <- self.cache |> Endp_map.remove endp
      else self.cache <- self.cache |> Endp_map.add endp conns;
      Lwt.return_unit
    in
    let create () =
      let connection = Connection.create ~finalise ~ctx:self.ctx endp
      and timeout = ref Lwt.return_unit in
      let rec busy () =
        Lwt.cancel !timeout;
        if Connection.length connection = 0 then begin
          timeout :=
            Sleep.sleep_ns self.keep >>= fun () ->
            Connection.close connection; (* failure is ignored *)
            Lwt.return_unit
        end;
        Lwt.on_termination (Connection.notify connection) busy
      in busy ();
      connection
    in
    match Endp_map.find_opt endp self.cache with
    | None ->
      let connection = create () in
      self.cache <- self.cache |> Endp_map.add endp [connection];
      Lwt.return connection
    | Some conns ->
      let rec search length = function
        | a :: [] -> a, length + 1
        | a :: b :: tl
          when Connection.length a < Connection.length b ->
          search (length + 1) (a :: tl)
        | _ :: tl ->
          search (length + 1) tl
        | [] -> assert false
      in
      match search 0 conns with
      | shallowest, _ when Connection.length shallowest = 0 ->
        Lwt.return shallowest
      | _, length when length < self.parallel ->
        let connection = create () in
        self.cache <- self.cache |> Endp_map.add endp (connection :: conns);
        Lwt.return connection
      | shallowest, _ when Connection.length shallowest < self.depth ->
        Lwt.return shallowest
      | _ ->
        Lwt.try_bind
          (fun () -> Lwt.choose (List.map Connection.notify conns))
          (fun _ -> get_connection self endp)
          (fun _ -> get_connection self endp)

  let request self ?body req =
    Net.resolve ~ctx:self.ctx (Cohttp.Request.uri req) >>= fun endp ->
    let rec request retry =
      get_connection self endp >>= fun conn ->
      Lwt.catch (fun () -> Connection.request conn ?body req)
        begin function
        | Retry ->
          begin match body with
          | Some `Stream _ -> Lwt.fail Retry
          | None | Some `Empty | Some `String _ | Some `Strings _ ->
            if retry <= 0
            then Lwt.fail Retry
            else request (retry - 1)
          end
        | e -> Lwt.fail e
        end
    in request self.retry
end

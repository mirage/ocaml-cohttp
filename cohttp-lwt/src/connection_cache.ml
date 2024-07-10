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
    proxy : Uri.t option;
  }

  let create ?(ctx = Lazy.force Net.default_ctx) ?(keep = 60_000_000_000L)
      ?(retry = 2) ?(parallel = 4) ?(depth = 100) ?proxy () =
    {
      cache = Hashtbl.create ~random:true 10;
      ctx;
      keep;
      retry;
      parallel;
      depth;
      proxy;
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
      let connection =
        Connection.create ~persistent:true ~finalise ~ctx:self.ctx endp
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

  let prepare self ?headers ?absolute_form meth uri =
    match self.proxy with
    | None ->
        let absolute_form = Option.value ~default:false absolute_form in
        Net.resolve ~ctx:self.ctx uri >>= fun endp ->
        Lwt.return (endp, absolute_form, headers)
    | Some proxy_uri ->
        let absolute_form =
          Option.value
            ~default:
              (not
                 (meth = `CONNECT
                 || (meth = `OPTIONS && Uri.path_and_query uri = "*")))
            absolute_form
        in
        Net.resolve ~ctx:self.ctx proxy_uri >>= fun endp ->
        Lwt.return (endp, absolute_form, headers)

  let call self ?headers ?body ?absolute_form meth uri =
    prepare self ?headers ?absolute_form meth uri
    >>= fun (endp, absolute_form, headers) ->
    let rec request retry =
      get_connection self endp >>= fun conn ->
      Lwt.catch
        (fun () -> Connection.call conn ?headers ?body ~absolute_form meth uri)
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

module Make_tunnel (Connection : S.Connection) (Sleep : S.Sleep) : sig
  include S.Connection_cache

  val create :
    ?ctx:Connection.Net.ctx ->
    ?keep:int64 ->
    ?retry:int ->
    ?parallel:int ->
    ?depth:int ->
    ?proxy_headers:Http.Header.t ->
    Uri.t ->
    unit ->
    t
end = struct
  module Net = Connection.Net
  module IO = Net.IO
  open IO

  type ctx = Net.ctx
  type tunnel = { proxy : Connection.t; remote : Connection.t }

  type t = {
    cache : (string, tunnel) Hashtbl.t; (* remote host * tunnel *)
    proxy_uri : Uri.t;
    mutable proxy_endp : Net.endp option;
    proxy_headers : Http.Header.t;
    ctx : ctx;
    keep : int64;
    retry : int;
    parallel : int;
    depth : int;
  }

  let proxy_default_scheme uri =
    match Uri.scheme uri with
    | None -> Uri.with_scheme uri (Some "http")
    | _ -> uri

  let create ?(ctx = Lazy.force Net.default_ctx) ?(keep = 60_000_000_000L)
      ?(retry = 2) ?(parallel = 4) ?(depth = 100)
      ?(proxy_headers = Http.Header.init ()) proxy_uri () =
    if Uri.host proxy_uri = None then
      Printf.ksprintf invalid_arg "No host was provided in URI %s."
        (Uri.to_string proxy_uri);
    {
      cache = Hashtbl.create ~random:true 10;
      proxy_uri = proxy_default_scheme proxy_uri;
      proxy_endp = None;
      proxy_headers;
      ctx;
      keep;
      retry;
      parallel;
      depth;
    }

  let rec request conn ?headers ?body ?absolute_form meth uri retry =
    Lwt.catch
      (fun () -> Connection.call conn ?headers ?body ?absolute_form meth uri)
      (function
        | Retry -> (
            match body with
            | Some (`Stream _) -> Lwt.fail Retry
            | None | Some `Empty | Some (`String _) | Some (`Strings _) ->
                if retry <= 0 then Lwt.fail Retry
                else
                  request conn ?headers ?body ?absolute_form meth uri (retry - 1)
            )
        | e -> Lwt.fail e)

  let rec get_connection self ~proxy_endp ~remote ~remote_host ~remote_uri =
    let finalise connection =
      let rec remove keep =
        let current = Hashtbl.find self.cache remote in
        Hashtbl.remove self.cache remote;
        if current.proxy == connection || current.remote == connection then
          List.iter (Hashtbl.add self.cache remote) keep
        else remove (current :: keep)
      in
      remove [];
      Lwt.return_unit
    in
    let create () =
      let proxy =
        Connection.create ~persistent:true ~finalise ~ctx:self.ctx proxy_endp
      in
      request proxy ~headers:self.proxy_headers `CONNECT remote_uri self.retry
      >>= fun (resp, _body) ->
      let code = resp |> Http.Response.status |> Cohttp.Code.code_of_status in
      if not (Cohttp.Code.is_success code) then
        Printf.ksprintf failwith "Could not setup tunnel. Response code: %d\n"
          code;
      let remote =
        Connection.create_tunnel ~finalise ~ctx:self.ctx proxy remote_host
      and timeout = ref Lwt.return_unit in
      let rec busy () =
        Lwt.cancel !timeout;
        if Connection.length remote = 0 then (
          timeout :=
            Sleep.sleep_ns self.keep >>= fun () ->
            Connection.close remote;
            Connection.close proxy;
            (* failure is ignored *)
            Lwt.return_unit);
        Lwt.on_termination (Connection.notify remote) busy
      in
      busy ();
      Lwt.return { proxy; remote }
    in
    match Hashtbl.find_all self.cache remote with
    | [] ->
        create () >>= fun tunnel ->
        Hashtbl.add self.cache remote tunnel;
        Lwt.return tunnel
    | tunnels -> (
        let rec search length = function
          | [ a ] -> (a, length + 1)
          | a :: b :: tl
            when Connection.length a.remote < Connection.length b.remote ->
              search (length + 1) (a :: tl)
          | _ :: tl -> search (length + 1) tl
          | [] -> assert false
        in
        match search 0 tunnels with
        | shallowest, _ when Connection.length shallowest.remote = 0 ->
            Lwt.return shallowest
        | _, length when length < self.parallel ->
            create () >>= fun tunnel ->
            Hashtbl.add self.cache remote tunnel;
            Lwt.return tunnel
        | shallowest, _ when Connection.length shallowest.remote < self.depth ->
            Lwt.return shallowest
        | _ ->
            Lwt.try_bind
              (fun () ->
                Lwt.choose
                  (List.map
                     (fun { remote; _ } -> Connection.notify remote)
                     tunnels))
              (fun _ ->
                get_connection self ~proxy_endp ~remote ~remote_host ~remote_uri)
              (fun _ ->
                get_connection self ~proxy_endp ~remote ~remote_host ~remote_uri)
        )

  let call self ?headers ?body ?absolute_form meth uri =
    (match self.proxy_endp with
    | None ->
        Net.resolve ~ctx:self.ctx self.proxy_uri >>= fun proxy_endp ->
        self.proxy_endp <- Some proxy_endp;
        Lwt.return proxy_endp
    | Some proxy_endp -> Lwt.return proxy_endp)
    >>= fun proxy_endp ->
    let remote_port =
      match Uri_services.tcp_port_of_uri uri with
      | Some p -> p
      | None -> failwith "A port is required for the CONNECT method."
    in
    let remote_host = Option.get (Uri.host uri) in
    let remote = remote_host ^ ":" ^ string_of_int remote_port
    and remote_uri = Uri.with_port uri (Some remote_port) in
    get_connection self ~proxy_endp ~remote ~remote_host ~remote_uri
    >>= fun tunnel ->
    request tunnel.remote ?headers ?body ?absolute_form meth uri self.retry
end

type no_proxy_pattern = Name of string | Ipaddr_prefix of Ipaddr.Prefix.t
type no_proxy = Wildcard | Patterns of no_proxy_pattern list

let trim_dots ~first_leading s =
  let len = String.length s in
  let i = ref 0 in
  if first_leading && !i < len && String.unsafe_get s !i = '.' then incr i;
  let j = ref (len - 1) in
  while !j >= !i && String.unsafe_get s !j = '.' do
    decr j
  done;
  if !j >= !i then String.sub s !i (!j - !i + 1) else ""

let strncasecompare a b n =
  let a = String.(sub a 0 (min (length a) n) |> lowercase_ascii)
  and b = String.(sub b 0 (min (length b) n) |> lowercase_ascii) in
  String.compare a b = 0

let no_proxy_from_env no_proxy =
  if no_proxy = "*" then Wildcard
  else
    let patterns =
      no_proxy
      |> String.split_on_char ','
      |> List.filter_map (fun pattern ->
             if pattern = "" then None else Some (String.trim pattern))
      |> List.map (fun pattern ->
             match Ipaddr.of_string pattern with
             | Ok addr -> Ipaddr_prefix (Ipaddr.Prefix.of_addr addr)
             | Error _ -> (
                 match Ipaddr.Prefix.of_string pattern with
                 | Ok prefix -> Ipaddr_prefix prefix
                 | Error _ -> Name (trim_dots ~first_leading:true pattern)))
    in
    Patterns patterns

let check_no_proxy_patterns host = function
  | Wildcard -> true
  | _ when String.length host = 0 -> true
  | Patterns patterns -> (
      match Ipaddr.of_string host with
      | Ok hostip ->
          List.exists
            (function
              | Name _ -> false
              | Ipaddr_prefix network -> Ipaddr.Prefix.mem hostip network)
            patterns
      | Error _ ->
          let name = trim_dots ~first_leading:false host in
          List.exists
            (function
              | Ipaddr_prefix _ -> false
              | Name pattern ->
                  let patternlen = String.length pattern
                  and namelen = String.length name in
                  if patternlen = namelen then
                    strncasecompare pattern name namelen
                  else if patternlen < namelen then
                    name.[namelen - patternlen - 1] = '.'
                    && strncasecompare pattern
                         (String.sub name (namelen - patternlen)
                            (patternlen - namelen - patternlen))
                         patternlen
                  else false)
            patterns)

let tunnel_schemes = [ "https" ]

module Make_proxy (Connection : S.Connection) (Sleep : S.Sleep) = struct
  module Connection_cache = Make (Connection) (Sleep)
  module Connection_tunnel = Make_tunnel (Connection) (Sleep)

  type proxy = Direct of Connection_cache.t | Tunnel of Connection_tunnel.t

  type t = {
    proxies : (string * proxy) list;
    direct : proxy option;
    tunnel : proxy option;
    no_proxy : Connection_cache.t;
    no_proxy_patterns : no_proxy;
  }

  let create ?ctx ?keep ?retry ?parallel ?depth ?(scheme_proxy = []) ?all_proxy
      ?no_proxy ?proxy_headers () =
    let create_default () =
      Connection_cache.create ?ctx ?keep ?retry ?parallel ?depth ()
    and create_direct proxy =
      Connection_cache.create ?ctx ?keep ?retry ?parallel ?depth ~proxy ()
    and create_tunnel proxy_uri =
      Connection_tunnel.create ?ctx ?keep ?retry ?parallel ?depth ?proxy_headers
        proxy_uri ()
    in
    let no_proxy_patterns =
      match no_proxy with
      | None -> Patterns []
      | Some no_proxy -> no_proxy_from_env no_proxy
    in
    let no_proxy = create_default () in
    let proxies =
      List.map
        (fun (scheme, uri) ->
          let proxy =
            if List.mem scheme tunnel_schemes then Tunnel (create_tunnel uri)
            else Direct (create_direct uri)
          in
          (scheme, proxy))
        scheme_proxy
    in
    let direct, tunnel =
      match all_proxy with
      | Some uri ->
          (Some (Direct (create_direct uri)), Some (Tunnel (create_tunnel uri)))
      | None -> (None, None)
    in
    { no_proxy; direct; tunnel; proxies; no_proxy_patterns }

  let call self ?headers ?body ?absolute_form meth uri =
    let proxy =
      if
        check_no_proxy_patterns
          (Uri.host_with_default ~default:"" uri)
          self.no_proxy_patterns
      then None
        (* Connection_cache.call self.no_proxy ?headers ?body ?absolute_form meth uri *)
      else
        let scheme = Option.value ~default:"" (Uri.scheme uri) in
        match List.assoc scheme self.proxies with
        | proxy -> Some proxy
        | exception Not_found ->
            if List.mem scheme tunnel_schemes then self.tunnel else self.direct
    in
    match proxy with
    | None ->
        Connection_cache.call self.no_proxy ?headers ?body ?absolute_form meth
          uri
    | Some (Tunnel proxy) ->
        Connection_tunnel.call proxy ?headers ?body ?absolute_form meth uri
    | Some (Direct proxy) ->
        Connection_cache.call proxy ?headers ?body ?absolute_form meth uri
end

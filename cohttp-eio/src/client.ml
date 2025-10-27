open Eio.Std

type t = S.t

let cache = Atomic.make Connection_cache.No_cache.(call (create ()))
let set_cache c = Atomic.set cache c

include
  Cohttp.Generic.Client.Make
    (struct
      type 'a io = 'a
      type body = Body.t
      type 'a with_context = S.t -> sw:Eio.Switch.t -> 'a

      let map_context v f t ~sw = f (v t ~sw)

      let call t ~sw ?headers ?body ?(chunked = false) meth uri =
        (Atomic.get cache) t ~sw ?headers ?body ~chunked ~absolute_form:false
          meth uri
    end)
    (Io.IO)

let make_generic fn = (fn :> S.t)

let unix_address uri =
  match Uri.host uri with
  | Some path -> `Unix path
  | None -> Fmt.failwith "no host specified (in %a)" Uri.pp uri

let tcp_address ~net uri =
  let service =
    match Uri.port uri with
    | Some port -> Int.to_string port
    | _ -> Uri.scheme uri |> Option.value ~default:"http"
  in
  match
    Eio.Net.getaddrinfo_stream ~service net
      (Uri.host_with_default ~default:"localhost" uri)
  with
  | ip :: _ -> ip
  | [] -> failwith "failed to resolve hostname"

let make ~https net : S.t =
  let net = (net :> [ `Generic ] Eio.Net.ty r) in
  let https =
    (https
      :> (Uri.t -> [ `Generic ] Eio.Net.stream_socket_ty r -> S.connection)
         option)
  in
  fun ~sw uri ->
    match Uri.scheme uri with
    | Some "httpunix" ->
        (* FIXME: while there is no standard, http+unix seems more widespread *)
        let addr = unix_address uri in
        (addr, (Eio.Net.connect ~sw net addr :> S.connection))
    | Some "http" ->
        let addr = tcp_address ~net uri in
        (addr, (Eio.Net.connect ~sw net addr :> S.connection))
    | Some "https" -> (
        match https with
        | Some wrap ->
            let addr = tcp_address ~net uri in
            (addr, wrap uri @@ Eio.Net.connect ~sw net addr)
        | None -> Fmt.failwith "HTTPS not enabled (for %a)" Uri.pp uri)
    | x ->
        Fmt.failwith "Unknown scheme %a"
          Fmt.(option ~none:(any "None") Dump.string)
          x

module Make (Ipaddr : sig
  type t

  val of_string : string -> (t, [> `Msg of string ]) result

  module Prefix : sig
    type addr = t
    type t

    val of_addr : addr -> t
    val of_string : string -> (t, [> `Msg of string ]) result
    val mem : addr -> t -> bool
  end
end) =
struct
  type pattern = Name of string | Ipaddr_prefix of Ipaddr.Prefix.t
  type no_proxy_patterns = Wildcard | Patterns of pattern list

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

  let no_proxy_from_env_value no_proxy =
    match no_proxy with
    | None -> Patterns []
    | Some no_proxy ->
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
                       | Error _ -> Name (trim_dots ~first_leading:true pattern)
                       ))
          in
          Patterns patterns

  let check_no_proxy uri pattern =
    let host = Uri.host_with_default ~default:"" uri in
    match pattern with
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

  type ('direct, 'tunnel) t = Direct of 'direct | Tunnel of 'tunnel

  type ('direct, 'tunnel) servers = {
    by_scheme : (string * ('direct, 'tunnel) t) list;
    no_proxy_patterns : no_proxy_patterns;
    default_tunnel : ('direct, 'tunnel) t option;
    default_direct : ('direct, 'tunnel) t option;
  }

  (* Uri schemes that should be used with tunnelled proxies  *)
  let is_tunnel_scheme = function "https" -> true | _ -> false

  let make_servers ~no_proxy_patterns ~(default_proxy : Uri.t option)
      ~(scheme_proxies : (string * Uri.t) list) ~(direct : Uri.t -> 'direct)
      ~(tunnel : Uri.t -> 'tunnel) : ('direct, 'tunnel) servers =
    let by_scheme =
      List.map
        (fun (scheme, uri) ->
          let proxy =
            if is_tunnel_scheme scheme then Tunnel (tunnel uri)
            else Direct (direct uri)
          in
          (scheme, proxy))
        scheme_proxies
    in
    let no_proxy_patterns = no_proxy_from_env_value no_proxy_patterns in
    let default_tunnel, default_direct =
      match default_proxy with
      | None -> (None, None)
      | Some uri -> (Some (Direct (direct uri)), Some (Tunnel (tunnel uri)))
    in
    { by_scheme; no_proxy_patterns; default_tunnel; default_direct }

  let get (servers : ('direct, 'tunnel) servers) (uri : Uri.t) :
      ('direct, 'tunnel) t option =
    if check_no_proxy uri servers.no_proxy_patterns then None
    else
      let scheme = Option.value ~default:"" (Uri.scheme uri) in
      match List.assoc scheme servers.by_scheme with
      | proxy -> Some proxy
      | exception Not_found ->
          if is_tunnel_scheme scheme then servers.default_tunnel
          else servers.default_direct
end

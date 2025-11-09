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
end

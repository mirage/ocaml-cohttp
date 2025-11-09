module Proxy = Cohttp.Proxy.Forward.Make (Ipaddr)

let http_proxy = "http://proxy.com"
let https_proxy = "http://https-proxy.com"
let fallback_proxy = "http://fallback-proxy.com"

let proxies ~no_proxy_patterns =
  Proxy.make_servers ~no_proxy_patterns
    ~default_proxy:(Some (Uri.of_string fallback_proxy))
    ~scheme_proxies:
      [
        ("http", Uri.of_string http_proxy); ("https", Uri.of_string https_proxy);
      ]
    ~direct:Uri.to_string ~tunnel:Uri.to_string

let proxy =
  let pp fmt = function
    | Proxy.Direct s -> Format.fprintf fmt "Direct(%S)" s
    | Proxy.Tunnel s -> Format.fprintf fmt "Tunnel(%S)" s
  in
  Alcotest.testable pp ( = )

let select_http_proxy () =
  let proxies = proxies ~no_proxy_patterns:None in
  let expected = Some (Proxy.Direct http_proxy) in
  let actual = Proxy.get proxies @@ Uri.of_string "http://example.com" in
  Alcotest.(check' @@ option proxy)
    ~msg:"should select configured http proxy as Direct" ~actual ~expected

let select_https_proxy () =
  let proxies = proxies ~no_proxy_patterns:None in
  let expected = Some (Proxy.Tunnel https_proxy) in
  let actual = Proxy.get proxies @@ Uri.of_string "https://example.com" in
  Alcotest.(check' @@ option proxy)
    ~msg:"should select configured https proxy as Tunnel" ~actual ~expected

let select_default_proxy () =
  let proxies = proxies ~no_proxy_patterns:None in
  let expected = Some (Proxy.Tunnel fallback_proxy) in
  let actual = Proxy.get proxies @@ Uri.of_string "ftp://example.com" in
  Alcotest.(check' @@ option proxy)
    ~msg:"should select fallback proxy for unconfigured scheme" ~actual
    ~expected

let no_proxy_wildcard () =
  let proxies = proxies ~no_proxy_patterns:(Some "*") in
  let expected = None in
  let actual = Proxy.get proxies @@ Uri.of_string "http://example.com" in
  Alcotest.(check' @@ option proxy)
    ~msg:"should ensure no proxy is selected" ~actual ~expected

let no_proxy_literal_pattern () =
  let proxies = proxies ~no_proxy_patterns:(Some "example.com") in
  let expected = None in
  let actual = Proxy.get proxies @@ Uri.of_string "http://example.com" in
  Alcotest.(check' @@ option proxy)
    ~msg:"should ensure example.com is not proxied" ~actual ~expected

let no_proxy_list_of_patterns () =
  let proxies = proxies ~no_proxy_patterns:(Some "foo.com,example.com") in

  let msg = "should ensure example.com is not proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://example.com" in
  Alcotest.(check' @@ option proxy) ~msg ~actual ~expected:None;

  let msg = "should ensure foo.com is not proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://foo.com" in
  Alcotest.(check' @@ option proxy) ~msg ~actual ~expected:None

let no_proxy_subdomain_patterns () =
  (* As per https://everything.curl.dev/usingcurl/proxies/env.html#no-proxy

     > If a name in the exclusion list starts with a dot (.), then the name matches
     that entire domain. For example .example.com matches both www.example.com and
     home.example.com but not nonexample.com. *)
  let proxies = proxies ~no_proxy_patterns:(Some ".example.com") in

  let msg = "should ensure www.example.com is not proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://www.example.com" in
  Alcotest.(check' @@ option proxy) ~msg ~actual ~expected:None;

  let msg = "should ensure home.example.com is not proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://home.example.com" in
  Alcotest.(check' @@ option proxy) ~msg ~actual ~expected:None;

  let msg = "should ensure example.com is proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://example.com" in
  Alcotest.(check' @@ option proxy) ~msg ~actual ~expected:None;

  let msg = "nonexample.com should be proxied" in
  let actual = Proxy.get proxies @@ Uri.of_string "http://nonexample.com" in
  Alcotest.(check' @@ option proxy)
    ~msg ~actual ~expected:(Some (Direct http_proxy))

let () =
  Alcotest.run "test_proxy"
    [
      ( "NO_PROXY",
        [
          ("wildcard pattern", `Quick, no_proxy_wildcard);
          ("literal pattern", `Quick, no_proxy_literal_pattern);
          ("list of patterns", `Quick, no_proxy_list_of_patterns);
          ("subdomain patterns", `Quick, no_proxy_subdomain_patterns);
        ] );
      ( "scheme proxies",
        [
          ("selects http proxy", `Quick, select_http_proxy);
          ("selects https proxy", `Quick, select_https_proxy);
          ("selects default proxy", `Quick, select_default_proxy);
        ] );
    ]

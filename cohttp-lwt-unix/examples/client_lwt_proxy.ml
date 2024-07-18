open Lwt
open Cohttp
open Cohttp_lwt_unix

let () =
  if not @@ Debug.debug_active () then (
    Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ();
    Logs.set_level ~all:true (Some Logs.Debug);
    Logs.set_reporter Debug.default_reporter)

let proxy_uri = ref None
let uri = ref []
let proxy_authorization = ref None
let set_proxy_uri uri = proxy_uri := Some (Uri.of_string uri)

let set_proxy_authorization auth =
  proxy_authorization :=
    Some (Cohttp.Auth.credential_of_string ("Basic " ^ Base64.encode_exn auth))

let usage_msg =
  {|Usage: test_client_proxy -proxy <uri> <resource>
Examples:
$ test_client_proxy -proxy http://localhost:8080 http://example.com
$ test_client_proxy -proxy https://localhost:8080 https://example.com
Options:|}

let anon_fun args = uri := !uri @ [ args ]

let speclist =
  [
    ("-proxy", Arg.String set_proxy_uri, "<uri>  Proxy uri");
    ("-proxyauth", Arg.String set_proxy_authorization, " Proxy authorization");
  ]

(* Boilerplate code to handle redirects *)

let rec http_get_and_follow ~max_redirects ?headers uri =
  let open Lwt.Syntax in
  let* ans = Cohttp_lwt_unix.Client.get ?headers uri in
  follow_redirect ~max_redirects ?headers uri ans

and follow_redirect ~max_redirects ?headers request_uri (response, body) =
  let open Lwt.Syntax in
  let status = Http.Response.status response in
  (* The unconsumed body would otherwise leak memory *)
  let* () =
    if status <> `OK then Cohttp_lwt.Body.drain_body body else Lwt.return_unit
  in
  match status with
  | `OK -> Lwt.return (response, body)
  | `Permanent_redirect | `Moved_permanently ->
      handle_redirect ~permanent:true ~max_redirects ?headers request_uri
        response
  | `Found | `Temporary_redirect ->
      handle_redirect ~permanent:false ~max_redirects ?headers request_uri
        response
  | `Not_found | `Gone -> failwith "Not found"
  | status ->
      Printf.ksprintf failwith "Unhandled status: %s"
        (Cohttp.Code.string_of_status status)

and handle_redirect ~permanent ~max_redirects ?headers request_uri response =
  if max_redirects <= 0 then failwith "Too many redirects"
  else
    let headers' = Http.Response.headers response in
    let location = Http.Header.get headers' "location" in
    match location with
    | None -> failwith "Redirection without Location header"
    | Some url ->
        let open Lwt.Syntax in
        let uri = Uri.of_string url in
        let* () =
          if permanent then
            Logs_lwt.warn (fun m ->
                m "Permanent redirection from %s to %s"
                  (Uri.to_string request_uri)
                  url)
          else Lwt.return_unit
        in
        http_get_and_follow ?headers uri ~max_redirects:(max_redirects - 1)

(* Interesting stuff *)

let getenv_opt k =
  match Sys.getenv_opt k with
  | Some v -> Some (k, Uri.of_string v)
  | None -> None

let getenv_opt_case k =
  match getenv_opt (String.lowercase_ascii k) with
  | None -> getenv_opt (String.uppercase_ascii k)
  | v -> v

let main ~proxy ~uri ~credential () =
  let all_proxy, no_proxy, scheme_proxy =
    match proxy with
    | None ->
        ( Option.map Uri.of_string (Sys.getenv_opt "ALL_PROXY"),
          Sys.getenv_opt "NO_PROXY",
          [
            getenv_opt_case "httpunix_proxy";
            getenv_opt_case "https_proxy";
            getenv_opt "http_proxy";
          ]
          |> List.filter_map (function
               | Some (k, v) -> Some (String.(sub k 0 (rindex k '_')), v)
               | n -> n) )
    | v -> (v, None, [])
  in

  let proxy_headers =
    Option.map
      (fun credential ->
        Http.Header.init_with "Proxy-Authorization"
          (Cohttp.Auth.string_of_credential credential))
      credential
  in

  let module Cache = Cohttp_lwt_unix.Connection_proxy in
  let cache =
    Cache.create ?all_proxy ~scheme_proxy ?no_proxy ?proxy_headers ()
  in
  Client.set_cache (Cache.call cache);

  http_get_and_follow ~max_redirects:2 (Uri.of_string uri)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  print_endline ("Received body\n" ^ body)

(* Argument parsing *)

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.length !uri <> 1 then (
    prerr_endline "Expected a single resource uri.";
    prerr_endline usage_msg;
    exit 1);
  let proxy = !proxy_uri
  and uri = List.hd !uri
  and credential = !proxy_authorization in
  Lwt_main.run (main ~proxy ~uri ~credential ())

open Lwt

let google_static_ip = Ipaddr.of_string_exn "213.104.143.99"

let () =
  let point_to_static_ip to_ip svc uri =
     let port =
       match Uri.port uri with
       | None -> svc.Resolver.port 
       | Some port -> port in
     return (`TCP (to_ip,port))
  in
 
  let service = Resolver_lwt_unix.static_service in
  let rewrites = [ "", (point_to_static_ip google_static_ip) ] in
  let resolver = Resolver_lwt.init ~service ~rewrites () in
  let ctx = Cohttp_lwt_unix.Client.custom_ctx ~resolver () in

  let fetch uri =
    Lwt_unix.run (
      Cohttp_lwt_unix.Client.get ~ctx (Uri.of_string uri)
      >>= fun (r,b) ->
      Cohttp_lwt_body.to_string b
    ) in

  prerr_endline (fetch "https://google.com");
  prerr_endline (fetch "http://google.com")

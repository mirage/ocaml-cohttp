open Eio.Std
include Client_intf
open Utils

type connection = [`Generic] Eio.Net.stream_socket_ty r

type t = sw:Switch.t -> Uri.t -> connection

include
  Cohttp.Client.Make
    (struct
      type 'a io = 'a
      type body = Body.t
      type 'a with_context = t -> sw:Eio.Switch.t -> 'a

      let map_context v f t ~sw = f (v t ~sw)

      let call (t:t) ~sw ?headers ?body ?(chunked = false) meth uri =
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
                    | Body.String m ->
                        Some (String.length (m body) |> Int64.of_int)
                    | _ -> None)
                  X.read_methods
        in
        let request =
          Cohttp.Request.make_for_client ?headers
            ~chunked:(Option.is_none body_length)
            ?body_length meth uri
        in
        Eio.Buf_write.with_flow socket @@ fun output ->
        let () =
          Eio.Fiber.fork ~sw @@ fun () ->
          Io.Request.write
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
    end)
    (Io.IO)

let make net : t =
  let net = (net :> [ `Generic ] Eio.Net.ty r) in
  fun ~sw uri ->
  Eio.Net.connect ~sw net @@ 
  match Uri.scheme uri with
  | Some "httpunix"
    (* FIXME: while there is no standard, http+unix seems more widespread *)
    -> (
        match Uri.host uri with
        | Some path -> `Unix path
        | None -> failwith "no host specified with httpunix")
  | _ -> (
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
      | [] -> failwith "failed to resolve hostname")

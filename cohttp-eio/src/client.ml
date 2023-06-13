include Client_intf
open Utils

module Make (Base : BASE) = struct
  include Base

  let call =
    map_context call (fun call ?headers ?body meth uri ->
        let () =
          Logs.info (fun m -> m "%a %a" Http.Method.pp meth Uri.pp uri)
        in
        call ?headers ?body meth uri)

  let delete =
    map_context call (fun call ?headers uri -> call ?headers `DELETE uri)

  let get = map_context call (fun call ?headers uri -> call ?headers `GET uri)
  let head = map_context call (fun call ?headers uri -> call ?headers `HEAD uri)

  let patch =
    map_context call (fun call ?headers uri -> call ?headers `PATCH uri)

  let post = map_context call (fun call ?headers uri -> call ?headers `POST uri)
  let put = map_context call (fun call ?headers uri -> call ?headers `PUT uri)
end

include Make (struct
  type 'a with_context = Eio.Net.t -> sw:Eio.Switch.t -> 'a

  let map_context v f net ~sw = f (v net ~sw)

  let call net ~sw ?headers ?body meth uri =
    let ( let* ) = Result.bind in
    let* addr =
      match Uri.scheme uri with
      | Some "httpunix"
      (* FIXME: while there is no standard, http+unix seems more widespread *)
        -> (
          match Uri.host uri with
          | Some path -> Result.Ok (`Unix path)
          | None -> Result.Error "no host specified with httpunix")
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
          | ip :: _ -> Result.Ok ip
          | [] -> Result.Error "failed to resolve hostname")
    in
    let socket = Eio.Net.connect ~sw net addr in
    let body_length =
      match body with
      | None -> Some 0L
      | Some body ->
          List.find_map
            (function
              | Body.String s -> Some (String.length s |> Int64.of_int)
              | _ -> None)
            (Eio.Flow.read_methods body)
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
    | `Eof -> Result.Error "connection closed by peer"
    | `Invalid reason -> Result.Error reason
    | `Ok response -> (
        match Cohttp.Response.has_body response with
        | `No -> Result.Ok (response, Eio.Flow.string_source "")
        | `Yes | `Unknown ->
            let body =
              let reader = Io.Response.make_body_reader response input in
              flow_of_reader reader Io.Response.read_body_chunk
            in
            Result.Ok (response, body))

  module Io = Io.IO
end)

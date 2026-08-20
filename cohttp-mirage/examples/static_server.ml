open Lwt.Infix

module Dir : sig
  include Mirage_kv.RO
  val connect : string -> t
end = struct
  type t = { root : string }
  type key = Mirage_kv.Key.t
  type error = Mirage_kv.error

  let pp_error = Mirage_kv.pp_error
  let connect root = { root }
  let disconnect _ = Lwt.return_unit

  let path t key =
    let segment s =
      match s with
      | "" | "." | ".." -> None
      | s when String.exists (function '/' | '\000' -> true | _ -> false) s -> None
      | s -> Some s
    in
    let rec go acc = function
      | [] -> Some acc
      | s :: rest -> (
          match segment s with
          | None -> None
          | Some s -> go (Filename.concat acc s) rest)
    in
    go t.root (Mirage_kv.Key.segments key)

  let stat t key =
    match path t key with
    | None -> Lwt.return None
    | Some path ->
        Lwt.catch
          (fun () -> Lwt_unix.stat path >|= fun stat -> Some (path, stat))
          (function
            | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
                Lwt.return None
            | e -> Lwt.reraise e)

  let get t key =
    stat t key >>= function
    | Some (path, { Unix.st_kind = Unix.S_REG; _ }) -> Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read >|= Result.ok
    | Some _ -> Lwt.return (Error (`Value_expected key))
    | None -> Lwt.return (Error (`Not_found key))

  let exists t key =
    stat t key >|= function
    | Some (_, { Unix.st_kind = Unix.S_REG; _ }) -> Ok (Some `Value)
    | Some (_, { Unix.st_kind = Unix.S_DIR; _ }) -> Ok (Some `Dictionary)
    | Some _ | None -> Ok None

  let size t key =
    stat t key >|= function
    | Some (_, { Unix.st_size = size; _ }) -> Ok (Optint.Int63.of_int size)
    | None -> Error (`Not_found key)

  let list t key =
    match path t key with
    | None -> Lwt.return (Error (`Not_found key))
    | Some dir ->
        Lwt.catch (fun () ->
          Lwt_unix.files_of_directory dir |> Lwt_stream.to_list
          >>= Lwt_list.filter_map_s (fun name ->
                if name = "." || name = ".." then Lwt.return_none
                else Lwt_unix.stat (Filename.concat dir name) >|= fun { Unix.st_kind; _ } ->
                match st_kind with
                | Unix.S_REG -> Some (Mirage_kv.Key.add key name, `Value)
                | Unix.S_DIR -> Some (Mirage_kv.Key.add key name, `Dictionary)
                | _ -> None)
          >|= Result.ok)
        (function
          | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
              Lwt.return (Error (`Not_found key))
          | e -> Lwt.reraise e)

  let get_partial _ key ~offset:_ ~length:_ = Lwt.return (Error (`Not_found key))
  let last_modified _ key = Lwt.return (Error (`Not_found key))
  let digest _ key = Lwt.return (Error (`Not_found key))
end

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&#39;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let html_of_listing dir entries =
  let segments = Mirage_kv.Key.segments dir in
  let link segments text =
    let href = segments |> List.map (Uri.pct_encode ~component:`Path) |> String.concat "/" |> html_escape in
    Printf.sprintf {|<li><a href="/%s">%s</a></li>|} href (html_escape text)
  in
  let parent =
    match List.rev segments with
    | [] -> []
    | _ :: up -> [ link (List.rev up) "../" ]
  in
  let entry (key, kind) =
    let name = Mirage_kv.Key.basename key in
    let text = match kind with `Dictionary -> name ^ "/" | `Value -> name in
    (name, link (segments @ [ name ]) text)
  in
  let entries =
    List.map entry entries
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> List.map snd
  in
  let title = "Index of " ^ html_escape (Mirage_kv.Key.to_string dir) in
  String.concat "\n"
    ([
       "<!DOCTYPE html>";
       "<html>";
       "<head><title>" ^ title ^ "</title></head>";
       "<body>";
       "<h1>" ^ title ^ "</h1>";
       "<ul>";
     ]
    @ parent @ entries
    @ [ "</ul>"; "</body>"; "</html>"; "" ])

module Generated_index (KV : Mirage_kv.RO) : Mirage_kv.RO with type t = KV.t =
struct
  include KV

  let index = "index.html"

  let generates t key =
    if Mirage_kv.Key.basename key <> index then Lwt.return_false
    else
      KV.exists t (Mirage_kv.Key.parent key) >|= function
      | Ok (Some `Dictionary) -> true
      | Ok (Some `Value | None) | Error _ -> false

  let get t key =
    KV.get t key >>= function
    | Ok _ as ok -> Lwt.return ok
    | Error _ as error -> (
        generates t key >>= function
        | false -> Lwt.return error
        | true -> (
            let dir = Mirage_kv.Key.parent key in
            KV.list t dir >|= function
            | Ok entries -> Ok (html_of_listing dir entries)
            | Error _ -> error))

  let exists t key =
    KV.exists t key >>= function
    | Ok None -> (
        generates t key >|= function
        | true -> Ok (Some `Value)
        | false -> Ok None)
    | r -> Lwt.return r
end

module Store = Generated_index (Dir)
module Server = Cohttp_lwt_unix.Server
module Static = Cohttp_mirage.Static.HTTP (Store) (Server)

let request_fn uri headers =
  Cohttp.Header.add headers "x-served-by"
    (Printf.sprintf "cohttp-mirage static (%s)" (Uri.path uri))

let () =
  let docroot = if Array.length Sys.argv > 1 then Sys.argv.(1) else "htdocs" in
  let port =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 8080
  in
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Logs.Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.info (fun f -> f "serving %s on http://0.0.0.0:%d/" docroot port);
  Lwt_main.run
    (Static.start ~http_port:port ~request_fn (Dir.connect docroot)
       (fun (`TCP port) spec -> Server.create ~mode:(`TCP (`Port port)) spec))

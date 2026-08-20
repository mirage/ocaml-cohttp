(* Tests for [Cohttp_mirage.Static.HTTP]. *)

let run p =
  match Lwt.state p with
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e
  | Lwt.Sleep -> Alcotest.fail "promise did not resolve"

(* An in-memory [Mirage_kv.RO] over a list of (key, contents) pairs. *)
module Mock_fs : sig
  include Mirage_kv.RO

  val create : (string * string) list -> t
end = struct
  type t = (string * string) list
  type key = Mirage_kv.Key.t
  type error = Mirage_kv.error

  let pp_error = Mirage_kv.pp_error
  let create entries = entries
  let disconnect _ = Lwt.return_unit
  let path key = String.concat "/" (Mirage_kv.Key.segments key)

  let is_dictionary t path =
    let prefix = path ^ "/" in
    List.exists (fun (k, _) -> String.starts_with ~prefix k) t

  let get t key =
    let path = path key in
    match List.assoc_opt path t with
    | Some contents -> Lwt.return (Ok contents)
    | None when is_dictionary t path -> Lwt.return (Error (`Value_expected key))
    | None -> Lwt.return (Error (`Not_found key))

  let exists t key =
    let path = path key in
    if List.mem_assoc path t then Lwt.return (Ok (Some `Value))
    else if is_dictionary t path then Lwt.return (Ok (Some `Dictionary))
    else Lwt.return (Ok None)

  let unsupported _ = Alcotest.fail "unexpected key-value store operation"
  let get_partial _ _ ~offset:_ ~length:_ = unsupported ()
  let list _ _ = unsupported ()
  let last_modified _ _ = unsupported ()
  let digest _ _ = unsupported ()
  let size _ _ = unsupported ()
end

module Mock_server : sig
  include Cohttp_lwt.S.Server

  val serve : t -> string -> (Http.Response.t * Cohttp_lwt.Body.t) Lwt.t
end = struct
  module IO = struct
    type 'a t = 'a Lwt.t

    let ( >>= ) = Lwt.bind
    let return = Lwt.return

    type ic = unit
    type oc = unit
    type conn = unit
    type error = |

    let refill () = Lwt.return `Eof
    let with_input_buffer () ~f = fst (f "" ~pos:0 ~len:0)
    let read_line () = Lwt.return None
    let read () _ = Lwt.return ""
    let write () _ = Lwt.return_unit
    let flush () = Lwt.return_unit
    let catch f = Lwt.map Result.ok (f ())
    let pp_error _ = function (_ : error) -> .
  end

  type body = Cohttp_lwt.Body.t
  type conn = IO.conn * Cohttp.Connection.t [@@warning "-3"]
  type response = Http.Response.t * body

  type response_action =
    [ `Expert of Http.Response.t * (IO.ic -> IO.oc -> unit IO.t)
    | `Response of response ]

  type t = { handler : conn -> Http.Request.t -> body -> response Lwt.t }

  let make ?conn_closed:_ ~callback () = { handler = callback }
  let unsupported _ = Alcotest.fail "unexpected server operation"
  let make_response_action ?conn_closed:_ ~callback:_ () = unsupported ()
  let make_expert ?conn_closed:_ ~callback:_ () = unsupported ()
  let callback _ _ _ _ = unsupported ()

  let resolve_local_file ~docroot ~uri =
    Cohttp.Path.resolve_local_file ~docroot ~uri

  let respond ?(headers = Http.Header.init ()) ~status ~body () =
    Lwt.return (Http.Response.make ~status ~headers (), body)

  let respond_string ?headers ~status ~body () =
    respond ?headers ~status ~body:(`String body) ()

  let respond_error ?headers ?(status = `Internal_server_error) ~body () =
    respond_string ?headers ~status ~body ()

  let respond_redirect ?(headers = Http.Header.init ()) ~uri () =
    let headers = Http.Header.add headers "location" (Uri.to_string uri) in
    respond_string ~headers ~status:`Found ~body:"" ()

  let respond_need_auth ?(headers = Http.Header.init ()) ~auth () =
    let headers = Cohttp.Header.add_authorization_req headers auth in
    respond_string ~headers ~status:`Unauthorized ~body:"" ()

  let respond_not_found ?uri:_ () =
    respond_string ~status:`Not_found ~body:"Not found" ()

  let serve t target =
    let request = Http.Request.make ~meth:`GET target in
    t.handler ((), (Cohttp.Connection.create () [@warning "-3"])) request `Empty
end

module Static = Cohttp_mirage.Static.HTTP (Mock_fs) (Mock_server)

let store =
  Mock_fs.create
    [
      ("index.html", "root index");
      ("foo/index.html", "foo index");
      ("foo/bar/index.html", "foo/bar index");
      ("style.css", "css body");
      ("my file.txt", "spaced");
    ]

let request ?request_fn target =
  let t = Static.start ~http_port:8080 ?request_fn store (fun _ t -> t) in
  let response, body = run (Mock_server.serve t target) in
  (response, run (Cohttp_lwt.Body.to_string body))

let check_served name ~target ~expected =
  let response, body = request target in
  Alcotest.(check int)
    (name ^ ": status") 200
    (Http.Status.to_int (Http.Response.status response));
  Alcotest.(check string) (name ^ ": body") expected body

let check_not_found name ~target =
  let response, _ = request target in
  Alcotest.(check int)
    (name ^ ": status") 404
    (Http.Status.to_int (Http.Response.status response))

let test_index_pages () =
  check_served "root" ~target:"/" ~expected:"root index";
  check_served "directory" ~target:"/foo" ~expected:"foo index";
  check_served "directory trailing slash" ~target:"/foo/" ~expected:"foo index";
  check_served "nested directory" ~target:"/foo/bar" ~expected:"foo/bar index";
  check_served "nested directory trailing slash" ~target:"/foo/bar/"
    ~expected:"foo/bar index";
  check_served "empty segments collapsed" ~target:"/foo//bar/"
    ~expected:"foo/bar index"

let test_files () =
  check_served "file" ~target:"/style.css" ~expected:"css body";
  check_not_found "missing file" ~target:"/nope.css";
  check_not_found "missing directory" ~target:"/nope/"

let test_content_type () =
  let response, _ = request "/style.css" in
  Alcotest.(check (option string))
    "css content-type" (Some "text/css")
    (Http.Header.get (Http.Response.headers response) "content-type");
  let response, _ = request "/foo/" in
  Alcotest.(check (option string))
    "index content-type is that of the page served, not the directory"
    (Some "text/html")
    (Http.Header.get (Http.Response.headers response) "content-type")

let test_traversal_clamped () =
  check_served "dot-dot" ~target:"/foo/../style.css" ~expected:"css body";
  check_served "dot-dot above root" ~target:"/../../style.css"
    ~expected:"css body";
  check_served "encoded slash" ~target:"/..%2f..%2fstyle.css"
    ~expected:"css body";
  check_served "fully encoded" ~target:"/%2e%2e%2f%2e%2e%2fstyle.css"
    ~expected:"css body";
  check_served "encoded slash into directory index" ~target:"/foo%2fbar%2f"
    ~expected:"foo/bar index"

(* Percent-decoding should happens exactly once. *)
let test_single_decode () =
  check_served "encoded space" ~target:"/my%20file.txt" ~expected:"spaced";
  check_not_found "double encoded slash" ~target:"/..%252f..%252fstyle.css";
  check_not_found "double encoded, fully" ~target:"/%252e%252e%252fstyle.css"

let test_request_fn () =
  let request_fn uri headers =
    Http.Header.add headers "x-request-uri" (Uri.path uri)
  in
  let response, body = request ~request_fn "/foo/" in
  Alcotest.(check string) "index still served" "foo index" body;
  Alcotest.(check (option string))
    "request_fn sees the request URI" (Some "/foo/")
    (Http.Header.get (Http.Response.headers response) "x-request-uri")

let () =
  Alcotest.run "cohttp-mirage static"
    [
      ( "paths",
        [
          ("index pages", `Quick, test_index_pages);
          ("files", `Quick, test_files);
          ("content types", `Quick, test_content_type);
          ("traversal clamped to root", `Quick, test_traversal_clamped);
          ("single percent-decode", `Quick, test_single_decode);
          ("request_fn", `Quick, test_request_fn);
        ] );
    ]

module type S = sig
  type 'a io
  type ic
  type oc
  type body

  type response_action =
    [ `Expert of Http.Response.t * (ic -> oc -> unit io)
    | `Response of Http.Response.t * body ]

  type spec = Http.Request.t -> body -> response_action io
  type async_test = unit -> unit io

  val response : Http.Response.t * body -> response_action
  val expert : ?rsp:Http.Response.t -> (ic -> oc -> unit io) -> spec
  val const : (Http.Response.t * body) io -> spec
  val response_sequence : spec list -> spec
  val temp_server : ?port:int -> spec -> (Uri.t -> 'a io) -> 'a io

  val test_server_s :
    ?port:int ->
    ?name:string ->
    spec ->
    (Uri.t -> (string * async_test) list) ->
    OUnit.test io

  val run_async_tests : OUnit.test io -> OUnit.test_results io
end

let port =
  Random.self_init ();
  ref (1024 + Random.int 40000)

let next_port () =
  let current_port = !port in
  incr port;
  current_port

let response_sequence fail responses =
  let xs = ref responses in
  fun req body ->
    match !xs with
    | x :: xs' ->
        xs := xs';
        x req body
    | [] -> fail "response_sequence: Server exhausted responses"

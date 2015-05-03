open Cohttp

module type S = sig
  type 'a io
  type body

  type spec = Request.t -> body -> (Response.t * body) io
  type async_test = unit -> unit io

  val response_sequence : (Response.t * body) io list -> spec
  val temp_server : ?port:int -> spec -> (Uri.t -> 'a io) -> 'a io
  val test_server_s : ?port:int -> ?name:string -> spec
  -> (Uri.t -> (string * async_test) list) -> OUnit.test io
  val run_async_tests : OUnit.test io -> OUnit.test_results io
end

let port = ref 9193

let next_port () =
  let current_port = !port in
  incr port;
current_port

let response_sequence fail responses =
  let xs = ref responses in
  fun _ _ ->
    match !xs with
  | x::xs' ->
      xs := xs';
    x
  | [] -> fail "response_sequence: Server exhausted responses"

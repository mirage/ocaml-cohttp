open Cohttp
module type S = sig
  type 'a io
  type body

  (** A server that is being tested must be defined by providing a spec *)
  type spec = Request.t -> body -> (Response.t * body) io

  type async_test = unit -> unit io

  (** A server that returns the list of responses in sequences and crashes
      on further requests *)
  val response_sequence : (Response.t * body) io list -> spec

  (** Create a temporary server according to spec that lives until the callback
      thread is determined. The uri provided in the callback should be the base
      uri for any requests made to the temp server *)
  val temp_server : ?port:int -> spec -> (Uri.t -> 'a io) -> 'a io

  (** Create a test suite against a server defined by spec. Tests
      run sequentially. *)
  val test_server_s : ?port:int -> ?name:string -> spec
  -> (Uri.t -> (string * async_test) list) -> OUnit.test io

  (** Run an async unit test and return and print the result *)
  val run_async_tests : OUnit.test io -> OUnit.test_results io
end

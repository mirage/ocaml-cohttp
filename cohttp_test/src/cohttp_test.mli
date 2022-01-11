module type S = sig
  type 'a io
  type ic
  type oc
  type body

  type response_action =
    [ `Expert of Http.Response.t * (ic -> oc -> unit io)
    | `Response of Http.Response.t * body ]

  type spec = Http.Request.t -> body -> response_action io
  (** A server that is being tested must be defined by providing a spec *)

  type async_test = unit -> unit io

  val response : Http.Response.t * body -> response_action
  val expert : ?rsp:Http.Response.t -> (ic -> oc -> unit io) -> spec

  val const : (Http.Response.t * body) io -> spec
  (** A constant handler that always returns its argument *)

  val response_sequence : spec list -> spec
  (** A server that process requests using the provided specs in sequence and
      crashes on further reqeusts *)

  val temp_server : ?port:int -> spec -> (Uri.t -> 'a io) -> 'a io
  (** Create a temporary server according to spec that lives until the callback
      thread is determined. The uri provided in the callback should be the base
      uri for any requests made to the temp server *)

  val test_server_s :
    ?port:int ->
    ?name:string ->
    spec ->
    (Uri.t -> (string * async_test) list) ->
    OUnit.test io
  (** Create a test suite against a server defined by spec. Tests run
      sequentially. *)

  val run_async_tests : OUnit.test io -> OUnit.test_results io
  (** Run an async unit test and return and print the result *)
end

val next_port : unit -> int
(** Internal API. Subject to breakage *)

val response_sequence :
  (string -> 'a) -> ('b -> 'c -> 'a) list -> 'b -> 'c -> 'a

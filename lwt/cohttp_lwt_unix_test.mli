(** Primitive testing framework for end to end tests *)
open Cohttp_lwt_unix

(** A server that is being tested must be defined by providing a spec *)
type spec = Request.t -> Cohttp_lwt_body.t
  -> (Response.t * Cohttp_lwt_body.t) Lwt.t

type async_test = unit -> unit Lwt.t

(** A server that returns the list of responses in sequences and crashes
    on further requests *)
val response_sequence : (Response.t * Cohttp_lwt_body.t) Lwt.t list -> spec

(** Create a temporary server according to spec that lives until the callback
    thread is determined. The uri provided in the callback should be the base
    uri for any requests made to the temp server *)
val temp_server : ?port:int -> spec -> (Uri.t -> 'a Lwt.t) -> 'a Lwt.t

(** Create a test suite against a server defined by spec. Tests
    run sequentially. *)
val test_server_s : ?port:int -> ?name:string -> spec
  -> (Uri.t -> (string * async_test) list) -> OUnit.test Lwt.t

(** Run an async unit test and return and print the result *)
val run_async_tests : OUnit.test Lwt.t -> OUnit.test_result list Lwt.t

(** High performance lwt server

    This module is an alternative to the server offered in
    [Cohttp_lwt_unix.Server]. It's a simplified implementation that has less
    functionality but offers more control and better performance. The
    differences are as follows:

      - Vastly improved performance due to optimized buffer handling
      - No dependency on conduit
      - No builtin logging

   An example server: {[
     open Lwt.Syntax

     let server_callback ctx =
       Lwt.join
         [
           Cohttp_server_lwt_unix.ontext.discard_body ctx;
           Cohttp_server_lwt_unix.ontext.respond ctx
            (Http.Response.make ())
            (Cohttp_server_lwt_unix.Body.string "hello world");
         ]

     let main () =
       let* _server =
         let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
         let server = Cohttp_server_lwt_unix.create server_callback in
         Lwt_io.establish_server_with_client_address ~backlog:10_000 listen_address
           (fun _addr ch -> Cohttp_server_lwt_unix.handle_connection server ch)
       in
       let forever, _ = Lwt.wait () in
       forever

     let () = ignore (Lwt_main.run (main ()))
   ]}
 *)

module Body : sig
  module Encoding : sig
    type t
    (** HTTP body encoding *)

    val fixed : int64 -> t
    val chunked : t
  end

  type t
  (** A response body *)

  val string : ?encoding:Encoding.t -> string -> t
  (** [string ?encoding s] respond with body [s].

      [?encoding] the encoding to use. by default this is [Encoding.fixed] *)

  module Substring : sig
    type t = { base : string; pos : int; len : int }
  end

  val stream : ?encoding:Encoding.t -> (unit -> Substring.t option Lwt.t) -> t
  (** [stream ?encoding f] respond with body generated by repeatedly applying
      [f]. When [f] returns [None], it will be considered terminated.

      [?encoding] is the encoding to use. By deafult this is [Encoding.chunked]. *)
end

module Context : sig
  type t
  (** A request context *)

  val request : t -> Http.Request.t
  (** [request t] returns the HTTP request *)

  val read_body : t -> string Lwt.t
  (** [read_body t] read the request body as a string *)

  val discard_body : t -> unit Lwt.t
  (** [discard_body t] discard the request body *)

  val respond : t -> Http.Response.t -> Body.t -> unit Lwt.t
  (** [respond t response body] respond to the request with [response] and
      [body] *)
end

type t
(** The type of an HTTP server able to handle requests. *)

val create : ?on_exn:(exn -> unit) -> (Context.t -> unit Lwt.t) -> t
(** [create ?on_exn f] creates an HTTP server that will handle every incoming
    request with [f] concurrently.

    [on_exn] will be called on exceptions not caught in [f] or raisedd by the
    server itself. If [on_exn] isn't provided [Lwt.async_exception_hook] will be
    used. *)

val handle_connection :
  t -> Lwt_io.input_channel * Lwt_io.output_channel -> unit Lwt.t
(** [handle_connection t (i, o)] will handle all HTTP requests incoming from [i]
    and write them to [o].

    This function should be used with
    [Lwt_io.establish_server_with_client_address] to setup a running HTTP
    server. *)

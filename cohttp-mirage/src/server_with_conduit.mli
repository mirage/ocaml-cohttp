
(** HTTP server with conduit. *)
include Cohttp_lwt.S.Server with type IO.conn = Conduit_mirage.flow

val connect :
  (_, 'flow) Conduit_mirage.protocol ->
  ('cfg, 't, 'flow) Conduit_mirage.Service.service ->
  ('cfg -> t -> unit Lwt.t) Lwt.t

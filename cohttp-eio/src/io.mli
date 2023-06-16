module IO :
  Cohttp.S.IO
    with type 'a t = 'a
     and type conn = Eio.Net.Sockaddr.stream
     and type ic = Eio.Buf_read.t
     and type oc = Eio.Buf_write.t

module Request :
  Cohttp.S.Http_io with type t := Http.Request.t and module IO := IO

module Response :
  Cohttp.S.Http_io with type t := Http.Response.t and module IO := IO

(* module Transfer : module type of Cohttp.Private.Transfer_io.Make (IO) *)

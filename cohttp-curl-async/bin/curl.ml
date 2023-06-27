open Cohttp
module Curl = Cohttp_curl_async
module Sexp = Sexplib0.Sexp
open Async_kernel
module Writer = Async_unix.Writer
module Time = Core.Time_float

let ( let* ) x f = Deferred.bind x ~f

let client uri meth' () =
  let meth = Cohttp.Code.method_of_string meth' in
  let reply =
    let context = Curl.Context.create () in
    let request =
      Curl.Request.create ~timeout:(Time.Span.of_ms 5000.) meth ~uri
        ~input:Curl.Source.empty ~output:Curl.Sink.string
    in
    Curl.submit context request
  in
  let* resp, response_body =
    Deferred.both (Curl.Response.response reply) (Curl.Response.body reply)
  in
  Format.eprintf "response:%a@.%!" Sexp.pp_hum (Response.sexp_of_t resp);
  let status = Response.status resp in
  (match Code.is_success (Code.code_of_status status) with
  | false -> prerr_endline (Code.string_of_status status)
  | true -> ());
  let output_body c =
    Writer.write c response_body;
    Writer.flushed c
  in
  output_body (Lazy.force Writer.stdout)

let _ =
  let open Async_command in
  async_spec ~summary:"Fetch URL and print it"
    Spec.(
      empty
      +> anon ("url" %: string)
      +> flag "-X" (optional_with_default "GET" string) ~doc:" Set HTTP method")
    client
  |> Command_unix.run

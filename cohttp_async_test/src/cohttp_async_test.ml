open Core
open Async
open OUnit
open Cohttp_async

type 'a io = 'a Deferred.t
type body = Body.t
type spec = Request.t -> body -> (Response.t * body) io
type async_test = unit -> unit io

let const = Cohttp_test.const

let response_sequence = Cohttp_test.response_sequence failwith

let temp_server ?port spec callback =
  let port = match port with
    | None -> Cohttp_test.next_port ()
    | Some p -> p in
  let uri = Uri.of_string ("http://0.0.0.0:" ^ (string_of_int port)) in
  let server = Server.create (Tcp.on_port port) (fun ~body _sock req -> spec req body) in
  server >>= fun server ->
  callback uri >>= fun res ->
  Server.close server >>| fun () ->
  res

let test_server_s ?port ?(name="Cohttp Server Test") spec f =
  temp_server ?port spec begin fun uri ->
    Logs.info (fun m -> m "Test %s running on %s" name (Uri.to_string uri));
    let tests = f uri in
    let results =
      tests
      |> Deferred.List.map ~how:`Sequential ~f:(fun (name, test) ->
        Logs.debug (fun m -> m "Running %s" name);
        let res =
          try_with test >>| function
          | Ok () -> `Ok
          | Error exn -> `Exn exn in
        res >>| (fun res -> (name, res))) in
    results >>| (fun results ->
      let ounit_tests =
        results
        |> List.map ~f:(fun (name, res) ->
          name >:: fun () ->
            match res with
            | `Ok -> ()
            | `Exn x -> raise x) in
      name >::: ounit_tests)
  end

let run_async_tests test =
  (* enable logging to stdout *)
  Fmt_tty.setup_std_outputs ();
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter (Logs_fmt.reporter ());
  test >>| (fun a -> a |> OUnit.run_test_tt_main)

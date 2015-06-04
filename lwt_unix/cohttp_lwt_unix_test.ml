open Lwt
open OUnit
open Cohttp
open Cohttp_lwt_unix

type 'a io = 'a Lwt.t
type body = Cohttp_lwt_body.t

type spec = Request.t -> body -> (Response.t * body) io

type async_test = unit -> unit Lwt.t

let const = Cohttp_test.const

let response_sequence = Cohttp_test.response_sequence Lwt.fail_with

let temp_server ?port spec callback =
  let port = match port with
    | None -> Cohttp_test.next_port ()
    | Some p -> p in
  let server = Server.make ~callback:(fun _ req body -> spec req body) () in
  let uri = Uri.of_string ("http://0.0.0.0:" ^ (string_of_int port)) in
  let server = Lwt.catch
                 (fun () -> Server.create ~mode:(`TCP (`Port port)) server)
                 (function
                   | Lwt.Canceled -> Lwt.return_unit
                   | x -> Lwt.fail x) in
  callback uri >|= fun res ->
  Lwt.cancel server;
  res

let test_server_s ?port ?(name="Cohttp Server Test") spec f =
  temp_server ?port spec begin fun uri ->
    Lwt_log.ign_info_f "Test %s running on %s" name (Uri.to_string uri);
    let tests = f uri in
    let results =
      tests
      |> Lwt_list.map_s (fun (name, test) ->
        Lwt_log.ign_debug_f "Running %s" name;
        let res = Lwt.try_bind test
                    (fun () -> return `Ok)
                    (fun exn -> return (`Exn exn)) in
        res >|= (fun res -> (name, res))) in
    results >|= (fun results ->
      let ounit_tests =
        results
        |> List.map (fun (name, res) ->
          name >:: fun () ->
            match res with
            | `Ok -> ()
            | `Exn x -> raise x) in
      name >::: ounit_tests)
  end

let run_async_tests test = test >|= OUnit.run_test_tt_main

(*{{{ Copyright (c) 2014 Andy Ray <andy.ray@ujamjar.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

(*

Description:

(1) Enter a username and click the button to query github and get a JSON
description of the users public repositories.

(2) Download the given file and show its size and the first 1K of date.

Build instructions:

$ ocamlfind c -package lwt.ppx -package js_of_ocaml.ppx \
    -package cohttp.js -linkpkg -o test_xhr.byte test_xhr.ml

This is done through oasis with --enable-tests

The next step is to convert to javascript.  I'm not sure how to automate this step
with oasis.

$ js_of_ocaml +weak.js test_xhr.byte -o lib_test/test_xhr.js

to load it in the browser run

$ ./test_net_lwt_server.native

and navigate to

http://localhost:8081/lib_test/index.html

(this is required for the download to be allowed under the same origin security policy)

*)

open Lwt
module Client = Cohttp_lwt_xhr.Client

(* test config
module Client = Cohttp_lwt_xhr.Make_client_async(struct
  let chunked_response = false
  let chunk_size = 0
  let convert_body_string = Js.to_string
end) *)

(* grab elements from the webpage *)
let get_element e =
    let d = Dom_html.document in
    Js.Opt.get
        (d##(getElementById (Js.string e)))
        (fun () -> assert false)
let get_input n =
    match Dom_html.tagged (get_element n) with
    | Dom_html.Input(x) -> x
    | _ -> failwith ("couldn't find text element" ^ n)
let value n = Js.to_string (get_input n)##.value

(* JSON object used for pretty printing result *)
type json_object
class type json = object
    method parse : Js.js_string Js.t -> json_object Js.t Js.meth
    method stringify : json_object Js.t -> unit Js.opt -> int -> Js.js_string Js.t Js.meth
end
let json : json Js.t = Js.Unsafe.variable "JSON"
let pretty str = json##(stringify (json##(parse (Js.string str))) (Js.null) (2))

let main _ =
  let counter = get_element "counter" in
  let output_response1 = get_element "output-response1" in
  let output_response2 = get_element "output-response2" in
  let list_repos = get_element "list-repos" in
  let download_blob = get_element "download-blob" in

  (* cohttp query to the JSON github API *)
  let run_query _ =
    Lwt.ignore_result (
      (Client.get Uri.(of_string ("https://api.github.com/users/" ^ value "input" ^ "/repos")))
      >>= fun (resp, body) ->
      (* show the response data *)
      let b = Buffer.create 1024 in
      let add s = Buffer.add_string b s; Buffer.add_string b "\n" in
      add Cohttp.(Code.(string_of_version resp.Response.version));
      add Cohttp.(Code.(string_of_status resp.Response.status));
      Cohttp.Header.iter (fun k v -> List.iter (fun v -> add (k ^ ": " ^ v)) v)
        resp.Cohttp.Response.headers;
      output_response1##.innerHTML := Js.string (Buffer.contents b);

      (* show the body as pretty printed json *)
      Cohttp_lwt_body.to_string body >>= fun body ->
      output_response2##.innerHTML := pretty body;
      Lwt.return ());
    Js._false
  in
  list_repos##.onclick := Dom_html.handler run_query;

  (* Download a file from test_net_lwt_server.native
   * There is an issue here with _build/lib_test/test_xhr.byte
   * where the file is ~8MB but we get ~13MB.
   *
   * This is happening I think in the js_string -> ocaml string
   * conversion (some utf issue?).  Not sure on the cohttp semantics
   * for the received data here - should we effecively treat all
   * data as binary and convert the string ourselves?  Or based on
   * some header perhaps?
   *
   * There is also quite a big pause which i *think* is due to the
   * string conversion.  I've stalled the transfer for a few
   * seconds in the server and that part remains responsive - it seems
   * to happen after the data is available.
   *
   * If you use the chunked encoding type (see body_chunked) and
   * dont touch the body this also doesn't happen. *)
  let run_download _ =
    Lwt.ignore_result (
      Client.get
        Uri.(of_string ("http://localhost:8081/" ^ value "input"))
      >>= fun (resp, body) ->

      let body = Cohttp_lwt_body.to_stream body in
      (* get total length, and 1st bit of data *)
      let rec read_stream length data =
        Lwt_stream.get body >>= function
        | None -> Lwt.return (length, data)
        | Some(s) ->
          let length  = length + String.length s in
          let data =  (* get 1st 1K *)
            match data with
            | None -> Some(try String.sub s 0 1024 with _ -> s)
            | Some(data) -> Some(data)
          in
          Lwt_js.yield () >>= fun () ->
          read_stream length data
      in
      read_stream 0 None >>= fun (length, data) ->
      let data = match data with None -> "" | Some(data) -> data in
      output_response1##.innerHTML := Js.string (Printf.sprintf "blob size = %i\n" length);
      output_response2##.innerHTML := Js.bytestring data;
      Lwt.return ());
    Js._false
  in

  download_blob##.onclick := Dom_html.handler run_download;

  (* run a quickly updating counter -
   * we want to avoid long pauses and this helps up see them *)
  let set_counter =
    let r = ref 0 in
    (fun () ->
      incr r; counter##.innerHTML := Js.string (string_of_int !r))
  in
  let rec f() = set_counter (); Lwt.( Lwt_js.sleep 0.1 >>= f ) in
  let _ = f() in
  Js._false


let _ = Dom_html.window##.onload := Dom_html.handler main


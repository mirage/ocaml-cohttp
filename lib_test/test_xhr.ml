(*

Description: 

Enter a username and click the button to query github and get a JSON
description of the users public repositories.

Build instructions:

$ ocamlfind c -syntax camlp4o -package lwt.syntax -package js_of_ocaml.syntax \
    -package cohttp.js -package yojson -linkpkg -o test_xhr.byte test_xhr.ml

This is done through oasis with --enable-tests

The next step is to convert to javscript.  I'm not sure how to automate this step
with oasis.

$ js_of_ocaml +weak test_xhr.byte -o lib_test/test_xhr.js

and load it in the browser.

$ chromium-browser lib_test/index.html

*)

(* grab elements from the webpage *)
let get_element e = 
    let d = Dom_html.document in
    Js.Opt.get 
        (d##getElementById (Js.string e))
        (fun () -> assert false)
let get_input n =
    match Dom_html.tagged (get_element n) with
    | Dom_html.Input(x) -> x
    | _ -> failwith ("couldn't find text element" ^ n)
let value n = Js.to_string (get_input n)##value

let main _ = 
  let output_list_repos = get_element "output-list-repos" in
  let list_repos = get_element "list-repos" in
  
  (* pretty print output *)
  let pretty s = Yojson.Basic.(pretty_to_string (from_string s)) in
  
  (* run the cohttp query to github *)
  let run_query _ = 
    Lwt.ignore_result (
      lwt resp, body = Cohttp_lwt_xhr.Client.get 
        Uri.(of_string ("https://api.github.com/users/" ^ value "username" ^ "/repos"))
      in
      lwt body = Cohttp_lwt_body.to_string body in
      output_list_repos##innerHTML <- Js.string (pretty body);
      Lwt.return ());
    Js._false
  in
  list_repos##onclick <- Dom_html.handler run_query;
  Js._false
  

let _ = Dom_html.window##onload <- Dom_html.handler main


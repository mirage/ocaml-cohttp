(* {[
   Copyright (c) 2003, Lars Nilsson, <lars@quantumchamaeleon.com>
   Copyright (c) 2009, ygrek, <ygrek@autistici.org>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
   ]} *)

module Cohttp_curl = Cohttp_curl.Private
module Sink = Cohttp_curl.Sink
module Source = Cohttp_curl.Source

module Context = struct
  type t = {
    mt : Curl.Multi.mt;
    wakeners : (Curl.t, Curl.curlCode Lwt.u) Hashtbl.t;
    all_events : (Unix.file_descr, Lwt_engine.event list) Hashtbl.t;
    mutable timer_event : Lwt_engine.event;
  }

  let create () =
    (* Most of this is taken from https://github.com/ygrek/ocurl/blob/master/curl_lwt.ml *)
    let t =
      {
        mt = Curl.Multi.create ();
        wakeners = Hashtbl.create 32;
        all_events = Hashtbl.create 32;
        timer_event = Lwt_engine.fake_event;
      }
    in
    let rec finished s =
      match Curl.Multi.remove_finished t.mt with
      | None -> ()
      | Some (h, code) ->
          (match Hashtbl.find_opt t.wakeners h with
          | None -> ()
          | Some w ->
              Hashtbl.remove t.wakeners h;
              Lwt.wakeup w code);
          finished s
    in
    let on_readable fd _ =
      let (_ : int) = Curl.Multi.action t.mt fd EV_IN in
      finished "on_readable"
    in
    let on_writable fd _ =
      let (_ : int) = Curl.Multi.action t.mt fd EV_OUT in
      finished "on_writable"
    in
    let on_timer _ =
      Lwt_engine.stop_event t.timer_event;
      Curl.Multi.action_timeout t.mt;
      finished "on_timer"
    in
    Curl.Multi.set_timer_function t.mt (fun timeout ->
        Lwt_engine.stop_event t.timer_event;
        t.timer_event <-
          Lwt_engine.on_timer (float_of_int timeout /. 1000.) false on_timer);
    Curl.Multi.set_socket_function t.mt (fun fd what ->
        (match Hashtbl.find_opt t.all_events fd with
        | None -> ()
        | Some events ->
            List.iter Lwt_engine.stop_event events;
            Hashtbl.remove t.all_events fd);
        let events =
          match what with
          | POLL_REMOVE | POLL_NONE -> []
          | POLL_IN -> [ Lwt_engine.on_readable fd (on_readable fd) ]
          | POLL_OUT -> [ Lwt_engine.on_writable fd (on_writable fd) ]
          | POLL_INOUT ->
              [
                Lwt_engine.on_readable fd (on_readable fd);
                Lwt_engine.on_writable fd (on_writable fd);
              ]
        in
        match events with [] -> () | _ -> Hashtbl.add t.all_events fd events);
    t

  let register t curl wk =
    Hashtbl.add t.wakeners curl wk;
    Curl.Multi.add t.mt curl

  let unregister t curl =
    Curl.Multi.remove t.mt curl;
    Hashtbl.remove t.wakeners curl
end

module Method = Http.Method
module Header = Http.Header

module Response = struct
  type 'a t = {
    curl : Curl.t;
    response : Http.Response.t Lwt.t;
    body : 'a Lwt.t;
  }

  let response t = t.response
  let body t = t.body

  module Expert = struct
    let curl t = t.curl
  end
end

module Request = struct
  type 'a t = {
    wk_body : Curl.curlCode Lwt.u;
    wt_body : Curl.curlCode Lwt.t;
    wt_response : Http.Response.t Lwt.t;
    base : 'a Cohttp_curl.Request.t;
  }

  module Expert = struct
    let curl t = Cohttp_curl.Request.curl t.base
  end

  let create (type a) ?timeout_ms ?headers method_ ~uri ~input
      ~(output : a Sink.t) : a t =
    let wt_response, wk_response = Lwt.wait () in
    let wt_body, wk_body = Lwt.wait () in
    let wt_response = Lwt.protected wt_response in
    let wt_body = Lwt.protected wt_body in
    let base =
      Cohttp_curl.Request.create ?timeout_ms ?headers method_ ~uri ~input
        ~output ~on_response:(Lwt.wakeup wk_response)
    in
    { base; wt_response; wk_body; wt_body }
end

let submit (type a) context (request : a Request.t) : a Response.t =
  let curl = Cohttp_curl.Request.curl request.base in
  let cancel = lazy (Context.unregister context curl) in
  Lwt.on_cancel request.wt_response (fun () -> Lazy.force cancel);
  Lwt.on_cancel request.wt_body (fun () -> Lazy.force cancel);
  Context.register context curl request.wk_body;
  let body : a Lwt.t =
    let open Lwt.Syntax in
    let+ (_ : Curl.curlCode) = request.wt_body in
    (Cohttp_curl.Request.body request.base : a)
  in
  { Response.body; response = request.wt_response; curl }

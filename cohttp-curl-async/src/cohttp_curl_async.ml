open Async_kernel
module Time = Core.Time_float
module Fd = Async_unix.Fd
module Clock = Async_unix.Clock

let ( let+ ) x f = Deferred.map x ~f

module Cohttp_curl = Cohttp_curl.Private
module Sink = Cohttp_curl.Sink
module Source = Cohttp_curl.Source

module Context = struct
  type fd_events = {
    fd : Fd.t;
    mutable read : unit Ivar.t option;
    mutable write : unit Ivar.t option;
  }

  type t = {
    mt : Curl.Multi.mt;
    wakeners : (Curl.t, Curl.curlCode Ivar.t) Hashtbl.t;
    all_events : (Unix.file_descr, fd_events) Hashtbl.t;
    mutable timeout : (unit, unit) Clock.Event.t option;
  }

  let create () =
    let t =
      {
        mt = Curl.Multi.create ();
        wakeners = Hashtbl.create 32;
        all_events = Hashtbl.create 32;
        timeout = None;
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
              Ivar.fill w code);
          finished s
    in
    let on_readable fd =
      let (_ : int) = Curl.Multi.action t.mt (Fd.file_descr_exn fd) EV_IN in
      finished "on_readable"
    in
    let on_writable fd =
      let (_ : int) = Curl.Multi.action t.mt (Fd.file_descr_exn fd) EV_OUT in
      finished "on_writable"
    in
    let on_timer () =
      Curl.Multi.action_timeout t.mt;
      finished "on_timer"
    in
    Curl.Multi.set_timer_function t.mt (fun timeout ->
        (match t.timeout with
        | None -> ()
        | Some event -> Clock.Event.abort_if_possible event ());
        let duration = Time.Span.of_ms (float_of_int timeout) in
        t.timeout <- Some (Clock.Event.run_after duration on_timer ()));
    let socket_function fd (what : Curl.Multi.poll) =
      let create_event fd what =
        let interrupt = Ivar.create () in
        let f () =
          match what with `Read -> on_readable fd | `Write -> on_writable fd
        in
        let event =
          Fd.interruptible_every_ready_to fd what
            ~interrupt:(Ivar.read interrupt)
            (fun () -> f ())
            ()
          |> Deferred.map ~f:(function
               | `Bad_fd | `Closed -> assert false
               | `Unsupported -> assert false
               | `Interrupted -> ())
          |> Deferred.ignore_m
        in
        don't_wait_for event;
        interrupt
      in
      let needs_read = what = POLL_IN || what = POLL_INOUT in
      let needs_write = what = POLL_OUT || what = POLL_INOUT in
      let+ current =
        match Hashtbl.find_opt t.all_events fd with
        | Some fd -> Deferred.return fd
        | None ->
            Deferred.return
              {
                fd =
                  Fd.create (Fd.Kind.Socket `Active) fd
                    (Base.Info.createf "curl");
                read = None;
                write = None;
              }
      in
      let update fd set_event set needs what =
        match (set, needs) with
        | None, false -> ()
        | Some _, true -> ()
        | None, true -> set_event (Some (create_event fd what))
        | Some ivar, false ->
            Ivar.fill ivar ();
            set_event None
      in
      update current.fd
        (fun ivar -> current.read <- ivar)
        current.read needs_read `Read;
      update current.fd
        (fun ivar -> current.write <- ivar)
        current.write needs_write `Write;
      Hashtbl.replace t.all_events fd current
    in
    Curl.Multi.set_socket_function t.mt (fun fd what ->
        don't_wait_for (socket_function fd what));
    t

  let unregister t curl =
    Curl.Multi.remove t.mt curl;
    Hashtbl.remove t.wakeners curl

  let register t curl wk =
    Hashtbl.add t.wakeners curl wk;
    Curl.Multi.add t.mt curl
end

module Method = Http.Method
module Header = Http.Header

module Response = struct
  type 'a t = {
    curl : Curl.t;
    response : Http.Response.t Deferred.t;
    body : 'a Deferred.t;
    context : Context.t;
  }

  let response t = t.response
  let body t = t.body
  let cancel t = Context.unregister t.context t.curl

  module Expert = struct
    let curl t = t.curl
  end
end

module Request = struct
  type 'a t = {
    body_ready : Curl.curlCode Ivar.t;
    response_ready : Http.Response.t Deferred.t;
    base : 'a Cohttp_curl.Request.t;
  }

  module Expert = struct
    let curl t = Cohttp_curl.Request.curl t.base
  end

  let create (type a) ?timeout ?headers method_ ~uri ~(input : Source.t)
      ~(output : a Sink.t) : a t =
    let response_ready = Ivar.create () in
    let body_ready = Ivar.create () in
    let base =
      let timeout_ms =
        Option.map
          (fun timeout -> Time.Span.to_ms timeout |> int_of_float)
          timeout
      in
      Cohttp_curl.Request.create ?timeout_ms ?headers method_ ~uri ~input
        ~output ~on_response:(Ivar.fill response_ready)
    in
    { base; response_ready = Ivar.read response_ready; body_ready }
end

let submit (type a) context (request : a Request.t) : a Response.t =
  let curl = Cohttp_curl.Request.curl request.base in
  Context.register context curl request.body_ready;
  let body : a Deferred.t =
    let+ (_ : Curl.curlCode) = Ivar.read request.body_ready in
    (Cohttp_curl.Request.body request.base : a)
  in
  { Response.body; context; response = request.response_ready; curl }

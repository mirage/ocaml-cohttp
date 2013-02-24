(*
 * Copyright (c) 2012-2013 Anil Madhavapeddy <anil@recoil.org>
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
 *)

open Core.Std
open Async.Std

(* IO implementation that uses Fable Cstruct Pipes. It still goes out
 * to string since that's all that the scanner supports, but when we
 * port ocaml-re to use string, that'll improve *)
module IO = struct

  type 'a t = 'a Deferred.t
  let (>>=) = Deferred.(>>=)
  let return = Deferred.return

  type ic = {
    mutable ibuf: Cstruct.t option; (* Queue of incoming buf *)
    rd: Cstruct.t Pipe.Reader.t;
  }

  type oc = {
    mutable obufq: Cstruct.t list;  (* Queue of completed writebuf *)
    mutable obuf: Cstruct.t option; (* Active write buffer *)
    mutable opos: int;              (* Position in active write buffer *)
    wr: Cstruct.t Pipe.Writer.t;
  }

  (* Initialise buffered input and output from a Pipe pair *)
  let create rd wr =
    { ibuf=None; rd }, { obufq=[]; obuf=None; opos=0; wr }

  (* Refill the input buffer from the Pipe *)
  let refill_input ic =
    Pipe.read ic.rd
    >>= function
    |`Eof -> 
      return false
    |`Ok buf -> 
      ic.ibuf <- Some buf;
      return true

  (* Get an input buffer, refilling if needed *)
  let rec get_input ic =
    match ic.ibuf with
    |None ->  begin
       refill_input ic
       >>= function
       |true -> get_input ic
       |false -> return None
    end
    |Some buf when Cstruct.len buf = 0 -> begin
      refill_input ic
       >>= function
       |true -> get_input ic
       |false -> return None
    end
    |Some buf ->
      return (Some buf)

  (* Read one character from the input channel *)
  let read_char ic =
    get_input ic
    >>= function
    |None -> return None
    |Some buf ->
      let c = Cstruct.get_char buf 0 in
      ic.ibuf <- Some (Cstruct.shift buf 1);
      return (Some c)

  (* Read a chunk and scan for a character.
   * TODO XXX this would be _way_ faster to do in cstruct *)
  let read_until ic ch =
    get_input ic 
    >>= function
    |None -> return `Eof
    |Some buf -> begin
      let len = Cstruct.len buf in
      let rec scan off =
        if off = len then None else begin
          if Cstruct.get_char buf off = ch then
            Some off else scan (off+1)
        end
      in
      match scan 0 with
      |None -> (* not found, return what we have until EOF *)
        ic.ibuf <- None;
        return (`Not_found buf)
      |Some off -> (* found, so split the buffer *)
        let hd = Cstruct.sub buf 0 off in
        ic.ibuf <- Some (Cstruct.shift buf (off+1));
        return (`Found hd)
    end

  (* This reads a line of input, which is terminated either by a CRLF
     sequence, or the end of the channel (which counts as a line). *)
  let read_line ic =
    let rec get acc =
      read_until ic '\n'
      >>= function
      |`Eof -> return None
      |`Not_found buf ->
         get (buf :: acc)
      |`Found buf -> begin
         (* chop the CR if present *)
         let vlen = Cstruct.len buf in
         let buf =
           if vlen > 0 && (Cstruct.get_char buf (vlen-1) = '\r') then
             Cstruct.sub buf 0 (vlen-1) 
           else buf in
         return (Some (buf :: acc))
      end
    in
    get []
    >>= fun l -> return (Option.map l (fun l -> Cstruct.copyv (List.rev l)))

  let read_buf ic len =
    get_input ic
    >>= function
    |None -> return None
    |Some buf ->
      let avail = Cstruct.len buf in
      if len < avail then begin
        let hd,tl = Cstruct.split buf len in
        ic.ibuf <- Some tl;
        return (Some hd)
    end else begin
      ic.ibuf <- None;
      return (Some buf)
    end

  let read ic len =
    read_buf ic len
    >>= function
    |None -> return ""
    |Some buf -> return (Cstruct.to_string buf)

  let read_exactly ic len =
    let rec get acc need =
      match need with
      |0 -> return (Some (Cstruct.copyv (List.rev acc)))
      |need -> begin
         read_buf ic need
         >>= function
         |None -> return None
         |Some buf -> get (buf::acc) (need - (Cstruct.len buf))
       end
     in
     get [] len     

  let write oc buf =
    Pipe.write oc.wr (Cstruct.of_string buf)

  let write_line oc buf =
    Pipe.write oc.wr (Cstruct.of_string buf)
    >>= fun () ->
    Pipe.write oc.wr (Cstruct.of_string "\r\n")

  let iter fn x =
    Deferred.List.iter x ~f:fn 

end

module Net = struct
  let connect ~uri ~f =
    let host = Option.value (Uri.host uri) ~default:"localhost" in
    match Uri_services.tcp_port_of_uri ~default:"http" uri with
    |None -> f `Unknown_service
    |Some port ->
      Tcp.with_connection (Tcp.to_host_and_port host port)
        (fun ic oc -> f (`Ok (ic,oc)))
end

module Response = Cohttp.Response.Make(IO)
module Request = Cohttp.Request.Make(IO)
module Client = struct
  include Cohttp.Client.Make(IO)(Request)(Response)

  let call ?headers ?(chunked=false) ?body meth uri =
    let ivar = Ivar.create () in
    let state = ref `Waiting_for_response in
    let signal_handler s =
      match !state,s with
      |`Waiting_for_response, `Response resp ->
        let rd,wr = Pipe.create () in
        state := `Getting_body wr;
        Ivar.fill ivar (resp, rd);
        return ()
      |`Getting_body wr, `Body buf ->
        Pipe.write_when_ready wr ~f:(fun wrfn -> wrfn buf)
        >>= (function
        |`Closed -> (* Junk rest of the body *)
          state := `Junking_body;
          return ()
        |`Ok _ -> return ())
      |`Getting_body wr, `Body_end ->
        state := `Complete;
        Pipe.close wr;
        return ()
      |`Junking_body, `Body _ -> return ()
      |`Junking_body, `Body_end ->
        state := `Complete;
        return ()
      |`Waiting_for_response, `Body _
      |`Waiting_for_response, `Body_end
      |_, `Failure
      |`Junking_body, `Response _
      |`Getting_body _, `Response _ ->
        (* TODO warning and non-fatal *)
        assert false
      |`Complete, _ -> return ()
    in 
    Net.connect ~uri ~f:(function
    |`Unknown_service -> return None
    |`Ok (ic,oc) ->
      (* Establish the remote HTTP connection *)
      call ?headers ~chunked ?body meth uri signal_handler ic oc
      >>= fun () ->
      Ivar.read ivar >>= fun x -> 
      return (Some x)
    )
end


(*
  OCaml HTTP - do it yourself (fully OCaml) HTTP daemon

  Copyright (C) <2002-2005> Stefano Zacchiroli <zack@cs.unibo.it>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as
  published by the Free Software Foundation, version 2.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  USA
*)

open Http_types;;
open Http_constants;;
open Http_common;;
open Http_daemon;;
open Printf;;

let status_line_RE = Pcre.regexp "^(HTTP/\\d\\.\\d) (\\d{3}) (.*)$"

let anyize = function
  | Some addr -> addr
  | None -> Unix.ADDR_INET (Unix.inet_addr_any, -1)

class response
  (* Warning: keep default values in sync with Http_daemon.respond function *)
  ?(body = "") ?(headers = []) ?(version = http_version)
  ?clisockaddr ?srvsockaddr (* optional because response have to be easily
                            buildable in callback functions *)
  ?(code = 200) ?status
  ()
  =

    (** if no address were supplied for client and/or server, use a foo address
    instead *)
  let (clisockaddr, srvsockaddr) = (anyize clisockaddr, anyize srvsockaddr) in

    (* "version code reason_phrase" *)
  object (self)

      (* note that response objects can't be created with a None version *)
    inherit
      Http_message.message
        ~body ~headers ~version:(Some version) ~clisockaddr ~srvsockaddr

    val mutable _code =
      match status with
      | None -> code
      | Some (s: Http_types.status) -> code_of_status s
    val mutable _reason: string option = None

    method private getRealVersion =
      match self#version with
      | None ->
          failwith ("Http_response.fstLineToString: " ^
            "can't serialize an HTTP response with no HTTP version defined")
      | Some v -> string_of_version v

    method code = _code
    method setCode c =
      ignore (status_of_code c);  (* sanity check on c *)
      _code <- c
    method status = status_of_code _code
    method setStatus (s: Http_types.status) = _code <- code_of_status s
    method reason =
      match _reason with
      | None -> Http_misc.reason_phrase_of_code _code
      | Some r -> r
    method setReason r = _reason <- Some r
    method statusLine =
      String.concat " "
        [self#getRealVersion; string_of_int self#code; self#reason]
    method setStatusLine s =
      try
        let subs = Pcre.extract ~rex:status_line_RE s in
        self#setVersion (version_of_string subs.(1));
        self#setCode (int_of_string subs.(2));
        self#setReason subs.(3)
      with Not_found ->
        raise (Invalid_status_line s)

    method isInformational = is_informational _code
    method isSuccess = is_success _code
    method isRedirection = is_redirection _code
    method isClientError = is_client_error _code
    method isServerError = is_server_error _code
    method isError = is_error _code

      (* FIXME duplication of code between this and send_basic_headers *)
    method addBasicHeaders =
      self#addHeader ~name:"Date" ~value:(Http_misc.date_822 ());
      self#addHeader ~name:"Server" ~value:server_string

    method contentType = self#header "Content-Type"
    method setContentType t = self#replaceHeader "Content-Type" t
    method contentEncoding = self#header "Content-Encoding"
    method setContentEncoding e = self#replaceHeader "Content-Encoding" e
    method date = self#header "Date"
    method setDate d = self#replaceHeader "Date" d
    method expires = self#header "Expires"
    method setExpires t = self#replaceHeader "Expires" t
    method server = self#header "Server"
    method setServer s = self#replaceHeader "Server" s

    method private fstLineToString =
      sprintf "%s %d %s" self#getRealVersion self#code self#reason

  end



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

(** Object Oriented representation of HTTP messages *)

open Http_types;;

  (** OO representation of an HTTP message
  @param entity body included in the message
  @param headers message headers shipped with the message *)
class virtual message:
  body: string -> headers: (string * string) list -> version: version option ->
  clisockaddr: Unix.sockaddr -> srvsockaddr: Unix.sockaddr ->
    object

        (** @return message HTTP version, it can be None because older version
        of the HTTP protocol don't require HTTP version to be told between
        message source and destination *)
      method version: version option

        (** set message HTTP version *)
      method setVersion: version -> unit

        (** @return message body *)
      method body: string

        (** set message body *)
      method setBody: string -> unit

        (** @return a Buffer.t connected to message body (Warning: changing this
        buffer will change message body too) *)
      method bodyBuf: Buffer.t

        (** set a new Buffer.t used to keep message body *)
      method setBodyBuf: Buffer.t -> unit

        (** append a string to message body *)
      method addBody: string -> unit

        (** append a whole buffer to message body *)
      method addBodyBuf: Buffer.t -> unit

      (** {i header name comparison are performed in a case-insensitive manner
      as required by RFC2616, actually the implementation works converting all
      header names in lowercase} *)

        (** add an HTTP header
        @param name header's name
        @param value header's value *)
      method addHeader: name:string -> value:string -> unit

        (** add a list of HTTP headers
        @param headers a list of pairs: header_name, header_value *)
      method addHeaders: (string * string) list -> unit

        (** like addHeader but replace previous definition of the same header *)
      method replaceHeader: name:string -> value:string -> unit

        (** like addHeaders but replace previous definition of headers that were
        already defined *)
      method replaceHeaders: (string * string) list -> unit

        (** remove _all_ occurences of an HTTP header from the message
        @param name name of the header to be removed *)
      method removeHeader: name:string -> unit

        (** @return true if given header exists in message, false otherwise *)
      method hasHeader: name:string -> bool

        (** @return value associated to a given header
        @param name name of the header to lookup
        @raise Header_not_found if given header wasn't defined in message *)
      method header: name:string -> string

        (** @return the full set of headers defined for this message, the value
        returned is an association list from headers name to headers value, an
        header may occurs more that once in the list *)
      method headers: (string * string) list


        (** @return client Unix.sockaddr *)
      method clientSockaddr: Unix.sockaddr

        (** @return client address pretty printed *)
      method clientAddr: string

        (** @return client port *)
      method clientPort: int

        (** @return server Unix.sockaddr *)
      method serverSockaddr: Unix.sockaddr

        (** @return server address pretty printed *)
      method serverAddr: string

        (** @return server port *)
      method serverPort: int


        (** @return for requests first request line, for responses first
        response line.
        User by derived requests and responses to implement toString method *)
      method private virtual fstLineToString: string

        (** @return a string representation of the message *)
      method toString: string

        (** serialize the message over an output channel *)
      method serialize: out_channel -> unit

    end


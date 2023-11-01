module Accept = Accept
module Auth = Auth
module Body = Body
module Conf = Conf
module Connection = Connection [@@deprecated "Connection.t values are useless."]
module Code = Code
module Cookie = Cookie
module Header = Header
module Link = Link
module Request = Request
module Response = Response
module S = S
module Path = Path
module Transfer = Transfer

module Generic = struct
  module Client = Client
  module Server = Server
end

module Private = struct
  module Transfer_io = Transfer_io
  module String_io = String_io
  module Header_io = Header_io
end

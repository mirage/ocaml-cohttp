module Body = Body
module Body_raw = Body [@@deprecated "Use Body"]
module Client = Client
module Io = Io [@@deprecated "This module is not for public consumption"]
module Request = Cohttp.Request [@@deprecated "Use Cohttp.Request directly"]
module Response = Cohttp.Response [@@deprecated "Use Cohttp.Response directly"]
module Server = Server

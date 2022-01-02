module Request (IO : S.IO) = struct
  include Cohttp.Request

  include (
    Private.Make (IO) : module type of Private.Make (IO) with type t := t)
  end

module Response (IO : S.IO) = struct
  include Cohttp.Response

  include (
    Private.Make (IO) : module type of Private.Make (IO) with type t := t)
  end

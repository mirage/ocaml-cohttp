val string_of_read_error : S.read_error -> string

module Make(IO : S.IO) : S.Http_io with module IO=IO

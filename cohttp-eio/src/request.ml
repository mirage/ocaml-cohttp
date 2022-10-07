type host = string * int option
type resource_path = string

type 'a Header.header +=
  | Host : host Header.header
  | User_agent : string Header.header

type t = {
  meth : Http.Method.t;
  version : Http.Version.t;
  resource_path : resource_path;
}

let make ?(meth = `GET) ?(version = `HTTP_1_1) _host resource_path =
  { meth; version; resource_path }

let meth t = t.meth
let version t = t.version
let resource_path t = t.resource_path

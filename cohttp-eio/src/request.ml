type host = string * int option
type resource_path = string

type 'a Header.header +=
  | Host : host Header.header
  | User_agent : string Header.header

module Key = struct
  type 'a t = 'a Header.header

  let equal : type a b. a t -> b t -> (a, b) Header.eq option =
   fun t t' ->
    match (t, t') with
    | Host, Host -> Some Eq
    | User_agent, User_agent -> Some Eq
    | _, _ -> None

  let decode = failwith "not implemented"
end

module Header = Header.Make (Key)

type t = {
  headers : Header.t;
  meth : Http.Method.t;
  version : Http.Version.t;
  resource_path : resource_path;
}

let make ?(meth = `GET) ?(version = `HTTP_1_1) _host resource_path =
  let headers = Header.empty in
  { headers; meth; version; resource_path }

let meth t = t.meth
let version t = t.version
let resource_path t = t.resource_path

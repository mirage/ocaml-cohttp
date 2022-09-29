type host = string * int option
type resource_path = string
type 'a Header.t += Host : host Header.t | User_agent : string Header.t

module Headers = struct
  module K = struct
    type nonrec 'a t = 'a Header.t

    let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t =
     fun t t' ->
      let open Gmap.Order in
      (* Ensures Host header will be the first item on iter, to_list, map etc. *)
      match (t, t') with
      | Host, Host -> Eq
      | Host, _ -> Lt
      | _, Host -> Gt
      | _ -> Header.compare t t'
  end

  include Gmap.Make (K)

  let name_value (B (hdr, v)) =
    match (hdr, v) with
    | Host, (hostname, port) ->
        let v =
          match port with
          | Some p -> hostname ^ ":" ^ string_of_int p
          | None -> hostname
        in
        ("Host", v)
    | User_agent, user_agent -> ("User-Agent", user_agent)
    | _ -> Header.name_value hdr v
end

type t = {
  meth : Http.Method.t;
  headers : Headers.t;
  version : Http.Version.t;
  resource_path : resource_path;
}

let make ?(meth = `GET) ?(version = `HTTP_1_1) ?(headers = Headers.empty) host
    resource_path =
  let headers = Headers.(add Host host headers) in
  { meth; headers; version; resource_path }

let meth t = t.meth
let headers t = t.headers
let version t = t.version
let resource_path t = t.resource_path

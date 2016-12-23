#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let opams =
    let lint_deps_excluding = Some ["mirage-conduit"] in
    [Pkg.opam_file ~lint_deps_excluding "opam"]
  in
  Pkg.describe ~opams "mirage-http" @@ fun c ->
  Ok [ Pkg.mllib "src/mirage-http.mllib" ]

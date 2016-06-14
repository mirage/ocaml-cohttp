#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  let opams =
    let lint_deps_excluding = Some ["mirage-types-lwt"; "mirage-conduit"] in
    [Pkg.opam_file ~lint_deps_excluding "opam"]
  in
  let licenses = [Pkg.std_file "LICENSE"] in
  Pkg.describe ~opams ~licenses "mirage-http" @@ fun c ->
  Ok [ Pkg.mllib "src/mirage-http.mllib" ]

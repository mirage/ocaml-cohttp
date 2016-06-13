#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe ~licenses:[Pkg.std_file "LICENSE"] "mirage-http" @@ fun c ->
  Ok [ Pkg.mllib "src/mirage-http.mllib" ]

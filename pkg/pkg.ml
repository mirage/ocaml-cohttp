#use "topfind"
#require "topkg-jbuilder"

open Topkg

let () =
  Topkg_jbuilder.describe
    ~name:"cohttp"
    ~change_logs:[ Pkg.std_file "CHANGES.md"
                 ; Pkg.std_file "mirage-http-CHANGES.md" ]
    ()

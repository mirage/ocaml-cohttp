#use "topfind"
#require "topkg-jbuilder"

open Topkg

let () =
  Topkg_jbuilder.describe
    ~change_logs:[ Pkg.std_file "CHANGES.md"
                 ; Pkg.std_file "cohttp-mirage-CHANGES.md" ]
    ()

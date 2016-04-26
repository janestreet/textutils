#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"textutils"
  [ oasis_lib "textutils"
  ; file "META" ~section:"lib"
  ]

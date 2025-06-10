open! Core
module Attr = Ansi_kernel.Attr

let list_sum l ~f = List.fold l ~init:0 ~f:(fun a b -> a + f b)
let list_max l ~f = List.fold l ~init:0 ~f:(fun a b -> max a (f b))

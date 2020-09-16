open! Core_kernel
open! Import
include Cell_intf

type t = Attr.t list * string list [@@deriving sexp_of]

let attr = fst
let to_tuple = Fn.id
let create attr str = attr, String.split_lines str
let width (_, lines) = list_max ~f:String.length lines

let height (_, lines) ~display_empty_rows ~width =
  let height =
    list_sum lines ~f:(fun s -> max ((String.length s + (width - 1)) / max width 1) 1)
  in
  if display_empty_rows then max height 1 else height
;;

let rec slices width lines =
  match lines with
  | [] -> []
  | line :: lines -> slices_split width lines line (String.length line) 0

and slices_split width lines line line_len pos =
  let chunk_len = min width (line_len - pos) in
  let completely_fits = Int.( = ) chunk_len (line_len - pos) in
  let chunk = String.sub line ~pos ~len:chunk_len in
  if completely_fits
  then chunk :: slices width lines
  else chunk :: slices_split width lines line line_len (pos + width)
;;

let wrap (_, lines) ~width = slices width lines
let is_empty (_, lines) = List.for_all lines ~f:String.is_empty

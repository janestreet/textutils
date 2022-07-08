open! Core
open! Import
include Cell_intf

type t = (Attr.t list * Utf8_text.t) list [@@deriving sexp_of]

let create entries =
  List.concat_map entries ~f:(fun (attrs, str) ->
    List.map (String.split_lines str) ~f:(fun line -> attrs, Utf8_text.of_string line))
;;

let width lines = list_max ~f:(fun (_, line) -> Utf8_text.width line) lines
let lines = Fn.id

let wrap_lines lines ~width ~prefer_split_on_spaces =
  List.concat_map lines ~f:(fun (attrs, line) ->
    let chunks = Utf8_text.chunks_of ~width ~prefer_split_on_spaces line in
    List.map chunks ~f:(fun chunk -> attrs, chunk))
;;

let height lines ~display_empty_rows ~width ~prefer_split_on_spaces =
  let height = wrap_lines lines ~width ~prefer_split_on_spaces |> List.length in
  if display_empty_rows then max height 1 else height
;;

let is_empty lines = List.for_all lines ~f:(fun (_, line) -> Utf8_text.is_empty line)

open! Core
open! Import
include Cell_intf

type t = (Attr.t list * String.Utf8.t) list [@@deriving sexp_of]

let create entries =
  List.concat_map entries ~f:(fun (attrs, str) ->
    List.map (String.split_lines str) ~f:(fun line -> attrs, String.Utf8.of_string line))
;;

let approx_display_width =
  (* See docs for [String.Utf8.length_in_uchars] on the limitations of assuming that the
     width per uchar is 1. *)
  String.Utf8.length_in_uchars
;;

let width lines = list_max ~f:(fun (_, line) -> approx_display_width line) lines
let lines = Fn.id

let wrap_lines lines ~width ~prefer_split_on_spaces =
  List.concat_map lines ~f:(fun (attrs, line) ->
    let chunks = Utf8_text_chunks.of_utf8 ~width ~prefer_split_on_spaces line in
    List.map chunks ~f:(fun chunk -> attrs, chunk))
;;

let height lines ~display_empty_rows ~width ~prefer_split_on_spaces =
  let height = wrap_lines lines ~width ~prefer_split_on_spaces |> List.length in
  if display_empty_rows then max height 1 else height
;;

let is_empty lines = List.for_all lines ~f:(fun (_, line) -> String.Utf8.is_empty line)

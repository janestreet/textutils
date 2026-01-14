open! Core
open! Import
include Ascii_table_kernel_intf
module Align = Column.Align
module Attr = Attr
module Column = Column
module Table_char = Table_char

module Display = struct
  type t = Grid.Display.t =
    | Short_box
    | Medium_box
    | Tall_box
    | Line
    | Blank
    | Column_titles
  [@@deriving compare ~localize, sexp_of]

  let short_box = Short_box
  let medium_box = Medium_box
  let tall_box = Tall_box
  let line = Line
  let blank = Blank
  let column_titles = Column_titles
end

module Screen = struct
  (* [Screen] is mostly private stuff, so we explicitly export the public bits instead of
     saying [Private] everywhere. *)

  type t = Screen.t

  let render = Screen.render
  let to_string = Screen.to_string
end

let draw
  ?(display = Display.short_box)
  ?(spacing = 1)
  ?(limit_width_to = 90)
  ?(header_attr = [])
  ?(display_empty_rows = false)
  ~prefer_split_on_spaces
  cols
  data
  =
  match cols with
  | [] -> None
  | _ :: _ ->
    Some
      (Grid.create
         ~spacing
         ~display
         ~max_width:limit_width_to
         ~header_attr
         cols
         data
         ~display_empty_rows
         ~prefer_split_on_spaces
       |> Grid.to_screen ~prefer_split_on_spaces)
;;

let to_string_noattr
  ?display
  ?spacing
  ?limit_width_to
  ?header_attr
  ?display_empty_rows
  ?(prefer_split_on_spaces = false)
  cols
  data
  ~bars
  =
  draw
    ?display
    ?spacing
    ?limit_width_to
    ?header_attr
    ?display_empty_rows
    cols
    data
    ~prefer_split_on_spaces
  |> Option.map ~f:(Screen.to_string ~bars ~string_with_attr:(fun _attr s -> s))
  |> Option.value ~default:""
;;

let cols_and_data_of_strings ?(index = false) ?(max_col_width = 90) cols data =
  let cols, data =
    if index
    then "#" :: cols, List.mapi data ~f:(fun i row -> Int.to_string (i + 1) :: row)
    else cols, data
  in
  let cols =
    List.mapi cols ~f:(fun i col ->
      let col, align =
        match String.chop_prefix col ~prefix:"-" with
        | None -> col, Align.Right
        | Some col -> col, Align.Left
      in
      Column.create ~max_width:max_col_width col (fun ls -> List.nth_exn ls i) ~align)
  in
  cols, data
;;

let simple_list_table_string
  ?index
  ?(display = Display.line)
  ?spacing
  ?(limit_width_to = 160)
  ?max_col_width
  ?header_attr
  ?(bars = `Unicode)
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  =
  let cols, data = cols_and_data_of_strings ?index ?max_col_width cols data in
  to_string_noattr
    ~display
    ?spacing
    ~limit_width_to
    ?header_attr
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
    ~bars
;;

module Private = struct
  module Utf8_text_chunks = Utf8_text_chunks
end

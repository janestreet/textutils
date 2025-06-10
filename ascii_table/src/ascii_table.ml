open Core
include Ascii_table_kernel

let output_screen ~oc ~screen ~bars =
  Ascii_table_kernel.Screen.render screen ~bars ~close:ignore ~output:(fun attr buf ->
    Console.Ansi.output_string attr oc (Buffer.contents buf);
    Buffer.clear buf)
;;

type ('row, 'rest) renderer =
  ?display:Display.t (* Default: short_box *)
  -> ?spacing:int (* Default: 1 *)
  -> ?limit_width_to:int (* defaults to 90 characters *)
  -> ?header_attr:Attr.t list
  -> ?bars:[ `Ascii | `Unicode ] (* defaults to [`Unicode] *)
  -> ?display_empty_rows:bool (* Default: false *)
  -> ?prefer_split_on_spaces:bool
  -> 'row Column.t list
  -> 'row list
  -> 'rest

let output
  ?display
  ?spacing
  ?limit_width_to
  ?header_attr
  ?(bars = `Unicode)
  ?display_empty_rows
  ?(prefer_split_on_spaces = false)
  cols
  data
  ~oc
  =
  Option.iter
    (Ascii_table_kernel.draw
       ?display
       ?spacing
       ?limit_width_to
       ?header_attr
       ?display_empty_rows
       ~prefer_split_on_spaces
       cols
       data)
    ~f:(fun screen -> output_screen ~screen ~bars ~oc)
;;

let to_string_gen
  ?display
  ?spacing
  ?limit_width_to
  ?header_attr
  ?(bars = `Unicode)
  ?display_empty_rows
  ?(prefer_split_on_spaces = false)
  cols
  data
  ~string_with_attr
  =
  match
    Ascii_table_kernel.draw
      ?display
      ?spacing
      ?limit_width_to
      ?header_attr
      ?display_empty_rows
      ~prefer_split_on_spaces
      cols
      data
  with
  | None -> ""
  | Some screen -> Screen.to_string screen ~bars ~string_with_attr
;;

let to_string_noattr
  ?display
  ?spacing
  ?limit_width_to
  ?header_attr
  ?bars
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  =
  to_string_gen
    ?display
    ?spacing
    ?limit_width_to
    ?header_attr
    ?bars
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
    ~string_with_attr:(fun _attrs str -> str)
;;

let to_string
  ?display
  ?spacing
  ?limit_width_to
  ?header_attr
  ?bars
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  =
  to_string_gen
    ?display
    ?spacing
    ?limit_width_to
    ?header_attr
    ?bars
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
    ~string_with_attr:Console.Ansi.string_with_attr
;;

let simple_list_table_internal
  ?index
  ?(display = Ascii_table_kernel.Display.line)
  ?spacing
  ?(limit_width_to = 160)
  ?(max_col_width = 90)
  ?header_attr
  ?bars
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  ~(f : (_, _) renderer)
  =
  let cols, data =
    let cols_and_data_of_strings =
      Ascii_table_kernel.cols_and_data_of_strings [@alert "-ascii_table_kernel_internal"]
    in
    cols_and_data_of_strings ?index ~max_col_width cols data
  in
  f
    ~display
    ?spacing
    ~limit_width_to
    ?header_attr
    ?bars
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
;;

let simple_list_table
  ?(oc = stdout)
  ?index
  ?display
  ?spacing
  ?limit_width_to
  ?max_col_width
  ?header_attr
  ?bars
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  =
  simple_list_table_internal
    ?index
    ?display
    ?spacing
    ?limit_width_to
    ?max_col_width
    ?header_attr
    ?bars
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
    ~f:(output ~oc)
;;

let simple_list_table_string
  ?index
  ?display
  ?spacing
  ?limit_width_to
  ?max_col_width
  ?header_attr
  ?bars
  ?display_empty_rows
  ?prefer_split_on_spaces
  cols
  data
  =
  simple_list_table_internal
    ?index
    ?display
    ?spacing
    ?limit_width_to
    ?max_col_width
    ?header_attr
    ?bars
    ?display_empty_rows
    ?prefer_split_on_spaces
    cols
    data
    ~f:to_string
;;

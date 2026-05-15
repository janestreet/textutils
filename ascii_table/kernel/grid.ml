open! Core
open! Import
include Grid_intf

type t =
  { data : Cell.t list list
  ; heights : int list
  ; widths : int list
  ; aligns : Column.Align.t list
  ; left_boundary : bool
  ; interior_boundaries : bool list
  ; right_boundary : bool
  ; spacing : int
  ; display : Display.t
  }
[@@deriving sexp_of]

let adjacent_pairs ts =
  let rec loop acc = function
    | left :: (right :: _ as rest) -> loop ((left, right) :: acc) rest
    | _ -> List.rev acc
  in
  loop [] ts
;;

let interior_boundaries_of_cols cols =
  List.map (adjacent_pairs cols) ~f:(fun (left, right) ->
    Column.Private.right_boundary left || Column.Private.left_boundary right)
;;

let create
  cols
  raw_data
  ~display
  ~display_empty_rows
  ~header_attr:h_attr
  ~max_width
  ~spacing
  ~prefer_split_on_spaces
  =
  let body =
    List.map raw_data ~f:(fun value -> List.map cols ~f:(Column.Private.to_cell ~value))
  in
  let empty =
    List.fold
      body
      ~init:(List.map cols ~f:(fun _ -> true))
      ~f:(List.map2_exn ~f:(fun is_empty element -> is_empty && Cell.is_empty element))
  in
  let keep =
    List.map2_exn cols empty ~f:(fun column is_empty ->
      match Column.show column with
      | `Yes -> true
      | `No -> false
      | `If_not_empty -> not is_empty)
  in
  let filter l = List.filter_opt (List.map2_exn keep l ~f:Option.some_if) in
  let cols = filter cols in
  let body = List.map body ~f:filter in
  (*=We subtract 1 from max_width because later we're going to add a line of
     '|'s to form the right wall of the table. *)
  let widths = Column.Private.layout cols raw_data ~spacing ~max_width:(max_width - 1) in
  let grid_data =
    List.map cols ~f:(fun column -> Cell.create [ h_attr, Column.header column ]) :: body
  in
  let heights =
    if [%compare.equal: Display.t] display Line
    then List.map grid_data ~f:(fun _ -> 1)
    else
      List.map grid_data ~f:(fun row ->
        assert (List.length widths = List.length row);
        List.map2_exn widths row ~f:(fun width element ->
          Cell.height element ~display_empty_rows ~width ~prefer_split_on_spaces)
        |> list_max ~f:Fn.id)
  in
  let aligns = List.map cols ~f:Column.align in
  let left_boundary =
    Option.value_map (List.hd cols) ~default:true ~f:Column.Private.left_boundary
  in
  let interior_boundaries = interior_boundaries_of_cols cols in
  let right_boundary =
    Option.value_map (List.last cols) ~default:true ~f:Column.Private.right_boundary
  in
  { data = grid_data
  ; heights
  ; widths
  ; aligns
  ; left_boundary
  ; interior_boundaries
  ; right_boundary
  ; spacing
  ; display
  }
;;

let is_empty t = List.is_empty t.widths

let to_screen t ~prefer_split_on_spaces =
  assert (List.length t.data = List.length t.heights);
  let mid_row =
    if [%compare.equal: Display.t] t.display Tall_box
       || [%compare.equal: Display.t] t.display Medium_box
    then 1
    else 0
  in
  (*=The total width of the table includes the '|'s to the left of elements, so we add 1
     and the spacing on either side when summing. *)
  let cols = list_sum t.widths ~f:(( + ) (1 + (t.spacing * 2))) + 1 in
  let rows = list_sum t.heights ~f:(( + ) mid_row) + 3 - (2 * mid_row) in
  let screen = Screen.create ~rows ~cols in
  let texel : Screen.Texel.t =
    if [%compare.equal: Display.t] t.display Column_titles then Blank else Line
  in
  Screen.hline screen texel ~row:0;
  Screen.hline screen texel ~row:(rows - 1);
  if not ([%compare.equal: Display.t] t.display Blank)
  then (
    if t.left_boundary then Screen.vline screen texel ~col:0;
    ignore
      (List.fold2_exn
         t.widths
         ((* This list represents whether to draw a right boundary for the given column,
             which is why we append [t.right_boundary] to the end. *)
          t.interior_boundaries
          @ [ t.right_boundary ])
         ~init:0
         ~f:(fun col width right_boundary ->
           let col = col + 1 + width + (t.spacing * 2) in
           if right_boundary then Screen.vline screen texel ~col;
           col)
       : int));
  ignore
    (List.fold2_exn t.data t.heights ~init:1 ~f:(fun row row_elements height ->
       let header_row = row = 1 in
       ignore
         (List.fold2_exn
            row_elements
            (List.zip_exn t.widths t.aligns)
            ~init:(1 + t.spacing)
            ~f:(fun col element (width, align) ->
              let lines = Cell.wrap_lines element ~width ~prefer_split_on_spaces in
              if [%compare.equal: Display.t] t.display Line
              then (
                match lines with
                | [] -> ()
                | [ (attr, line) ] ->
                  Screen.string screen align attr line ~row ~col ~width
                | (attr, line) :: _ ->
                  Screen.string screen align attr line ~row ~col ~width;
                  for col = col + max 0 (width - 3) to col + width - 1 do
                    Screen.char screen [] (Uchar.of_char '.') ~row ~col
                  done)
              else
                ignore
                  (List.fold lines ~init:row ~f:(fun row (attr, line) ->
                     Screen.string screen align attr line ~row ~col ~width;
                     row + 1)
                   : int);
              col + 1 + (t.spacing * 2) + width)
          : int);
       let row = row + height in
       if [%compare.equal: Display.t] t.display Tall_box || header_row
       then (
         if not ([%compare.equal: Display.t] t.display Blank)
         then Screen.hline screen Line ~row;
         row + 1)
       else if [%compare.equal: Display.t] t.display Medium_box
       then (
         let boundaries =
           (t.left_boundary :: t.interior_boundaries) @ [ t.right_boundary ]
         in
         ignore
           (List.fold2_exn
              t.widths
              (adjacent_pairs boundaries)
              ~init:0
              ~f:(fun col width (left_boundary, right_boundary) ->
                let width = width + (t.spacing * 2) in
                let write col = Screen.set_screen_point screen texel ~row ~col in
                if left_boundary
                then (
                  write col;
                  write (col + 1));
                if right_boundary then write (col + width);
                col + width + 1)
            : int);
         row + 1)
       else row)
     : int);
  screen
;;

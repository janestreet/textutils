open! Core
open! Import
include Column_intf

type 'a t =
  { max_width : int
  ; header : String.Utf8.t
  ; col_func : 'a -> Cell.t
  ; align : Align.t
  ; min_width : int option
  ; show : Show.t
  }
[@@deriving fields ~getters, sexp_of]

let lift t ~f = { t with col_func = (fun x -> t.col_func (f x)) }
let header t = String.Utf8.to_string t.header

let optional t =
  { t with
    col_func =
      (function
        | None -> Cell.create []
        | Some x -> t.col_func x)
  }
;;

let to_data t a =
  let tuples = Cell.lines (t.col_func a) in
  List.map tuples ~f:(fun (attrs, line) -> attrs, String.Utf8.to_string line)
;;

let create_attrs
  ?(align = Align.Left)
  ?min_width
  ?(max_width = 90)
  ?(show = `Yes)
  str
  parse_func
  =
  { max_width
  ; header = String.Utf8.of_string str
  ; col_func = (fun x -> Cell.create (parse_func x))
  ; align
  ; (*=We add one for the '|' on the left. *)
    min_width = Option.map min_width ~f:(( + ) 1)
  ; show
  }
;;

let create_attr ?align ?min_width ?max_width ?show str parse_func =
  create_attrs ?align ?min_width ?max_width ?show str (fun x -> [ parse_func x ])
;;

let create ?align ?min_width ?max_width ?show str parse_func =
  create_attrs ?align ?min_width ?max_width ?show str (fun x -> [ [], parse_func x ])
;;

let to_cell t ~value = t.col_func value
let update_header ~f t = { t with header = f (header t) |> String.Utf8.of_string }
let update_show ~f t = { t with show = f t.show }

let desired_width ~spacing data t =
  let column_data = List.map data ~f:t.col_func in
  let header_width =
    (* See docs for [String.Utf8.length_in_uchars] on the limitations of assuming that the
       width per uchar is 1. *)
    String.Utf8.split t.header ~on:(Uchar.of_char '\n')
    |> list_max ~f:String.Utf8.length_in_uchars
  in
  min
    (t.max_width - (2 * spacing))
    (max header_width (list_max column_data ~f:Cell.width))
;;

let layout ts data ~spacing ~max_width:table_width =
  let n = List.length ts in
  let extra = 1 (* left delimiter *) + (spacing * 2) in
  let max_width = table_width - (n * extra) in
  let requests =
    let%map.List t = ts in
    { Layout.Request.label = t.header
    ; desired_width = desired_width ~spacing data t
    ; min_width =
        (match t.min_width with
         | None -> Null
         | Some x -> This (x - extra))
    }
  in
  Layout.compute ~max_width requests
;;

module Of_field = struct
  let field ?align ?min_width ?max_width ?show ?header to_string record_field =
    create
      ?align
      ?min_width
      ?max_width
      ?show
      (Option.value header ~default:(Field.name record_field))
      (fun record -> to_string (Field.get record_field record))
  ;;

  let field_attr
    ?align
    ?min_width
    ?max_width
    ?show
    ?header
    to_string_and_attr
    record_field
    =
    create_attr
      ?align
      ?min_width
      ?max_width
      ?show
      (Option.value header ~default:(Field.name record_field))
      (fun record -> to_string_and_attr (Field.get record_field record))
  ;;

  let field_opt ?align ?min_width ?max_width ?show ?header to_string record_field =
    field
      ?align
      ?min_width
      ?max_width
      ?show
      ?header
      (function
        | None -> ""
        | Some x -> to_string x)
      record_field
  ;;

  let field_opt_attr
    ?align
    ?min_width
    ?max_width
    ?show
    ?header
    to_string_and_attr
    record_field
    =
    field_attr
      ?align
      ?min_width
      ?max_width
      ?show
      ?header
      (function
        | None -> [], ""
        | Some x -> to_string_and_attr x)
      record_field
  ;;
end

module Private = struct
  let layout = layout
  let to_cell = to_cell
end

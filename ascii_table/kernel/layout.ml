open! Core
open! Import

module Col = struct
  type t =
    { desired_width : int
    ; min_width : int
    }
  [@@deriving fields ~getters]

  let shrink_if_possible t =
    let { desired_width; min_width } = t in
    match desired_width > min_width with
    | true -> This { t with desired_width = desired_width - 1 }
    | false -> Null
  ;;
end

let rec reduce ~excess cols =
  (* Just reduce each col by 1 until we run out of excess. *)
  match excess <= 0 with
  | true -> List.map cols ~f:Col.desired_width
  | false ->
    let excess = ref excess in
    let cols =
      let%map.List col = cols in
      if !excess > 0
      then (
        match Col.shrink_if_possible col with
        | Null -> col
        | This col ->
          excess := !excess - 1;
          col)
      else col
    in
    reduce ~excess:!excess cols
;;

module Request = struct
  type t =
    { label : String.Utf8.t
    ; desired_width : int
    ; min_width : int or_null
    }
  [@@deriving sexp_of]

  let min t =
    match t.min_width with
    | Null -> 1
    | This x -> x
  ;;
end

type constraints =
  { total_width : int
  ; min_widths : (String.Utf8.t * int) list
  }
[@@deriving sexp_of]

exception Impossible_table_constraints of constraints [@@deriving sexp_of]

let compute ~max_width reqs =
  let minimum = list_sum reqs ~f:Request.min in
  if minimum > max_width
  then
    raise
      (Impossible_table_constraints
         { total_width = max_width
         ; min_widths =
             List.filter_map reqs ~f:(fun { label; min_width; _ } ->
               match min_width with
               | Null -> None
               | This x -> Some (label, x))
         });
  (* We know it's possible. Assign min widths to the unspecified columns, splitting up the
     available space evenly between them. *)
  let n_no_min = List.count reqs ~f:(fun { min_width; _ } -> Or_null.is_null min_width) in
  let requested_mins =
    list_sum reqs ~f:(fun { min_width; _ } -> Or_null.value ~default:0 min_width)
  in
  let remaining = max_width - requested_mins in
  let per_col_min = if n_no_min > 0 then remaining / n_no_min else 0 in
  let cols =
    List.map reqs ~f:(fun { desired_width; min_width; _ } ->
      match min_width with
      | This min_width -> { Col.desired_width = max desired_width min_width; min_width }
      | Null -> { Col.desired_width; min_width = per_col_min })
  in
  let desired = list_sum cols ~f:Col.desired_width in
  let excess = desired - max_width in
  reduce ~excess cols
;;

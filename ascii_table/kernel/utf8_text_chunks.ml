open Core

let assumed_width_per_uchar = 1
let space_uchar = Uchar.of_scalar_exn (Char.to_int ' ')

let fold_utf8_with_start_pos t ~init ~f =
  let require_uchar pos = function
    | `Malformed s -> raise_s [%message "Not UTF-8" ~_:(s : string) (pos : int)]
    | `Uchar uchar -> uchar
  in
  Uutf.String.fold_utf_8
    (fun init pos x -> f init pos (require_uchar pos x))
    init
    t [@nontail]
;;

let of_utf8 utf8 ~width ~prefer_split_on_spaces =
  let utf8 = String.Utf8.to_string utf8 in
  match utf8 with
  | "" -> [ String.Utf8.of_string "" ]
  | _ ->
    let uchar_ends_before_pos =
      Uutf.String.fold_utf_8 (fun acc start_pos _ -> start_pos :: acc) [] utf8
      |> List.cons (String.length utf8)
      |> List.rev
      |> List.tl_exn
    in
    (* We identify uchars by the byte positions after their last bytes *)
    let chunks =
      match prefer_split_on_spaces with
      | false ->
        assert (assumed_width_per_uchar = 1);
        uchar_ends_before_pos |> List.chunks_of ~length:width
      | true ->
        let get_num_uchars_in_chunk =
          let ends_of_spaces =
            fold_utf8_with_start_pos utf8 ~init:[] ~f:(fun acc start_pos uchar ->
              match Uchar.equal space_uchar uchar with
              | true -> (start_pos + assumed_width_per_uchar) :: acc
              | false -> acc)
            |> Set.of_list (module Int)
          in
          fun uchars_left ->
            List.take uchars_left width
            |> List.rev
            |> List.findi ~f:(fun _ pos -> Set.mem ends_of_spaces pos)
            |> Option.map ~f:(fun (uchars_after_last_space, _) ->
                 width - uchars_after_last_space)
            |> Option.value ~default:width
        in
        let rec chunks_split_on_spaces chunks num_uchars_left = function
          | [] -> List.rev chunks
          | _ :: _ as uchars_left ->
            (match num_uchars_left * assumed_width_per_uchar <= width with
             | true -> chunks_split_on_spaces (uchars_left :: chunks) 0 []
             | false ->
               let num_uchars_in_chunk = get_num_uchars_in_chunk uchars_left in
               let chunk, rest = List.split_n uchars_left num_uchars_in_chunk in
               let num_uchars_left = num_uchars_left - num_uchars_in_chunk in
               chunks_split_on_spaces (chunk :: chunks) num_uchars_left rest)
        in
        let num_uchars = List.length uchar_ends_before_pos in
        chunks_split_on_spaces [] num_uchars uchar_ends_before_pos
    in
    let chunk_ends_before_pos = chunks |> List.map ~f:List.last_exn |> Sequence.of_list in
    chunk_ends_before_pos
    |> Sequence.unfold_with ~init:0 ~f:(fun start_at end_before ->
         Yield
           { value =
               String.sub utf8 ~pos:start_at ~len:(end_before - start_at)
               |> String.Utf8.of_string
           ; state = end_before
           })
    |> Sequence.to_list
;;

open Core.Std

open Int.Replace_polymorphic_compare

type dims = { width : int; height : int }
  (* INVARIANT: width and height are both non-negative *)

type valign = [`Top | `Bottom | `Center]
type halign = [`Left | `Right | `Center]

type t =
  | Text of string
  | Fill of char option * dims
  | Hcat of t * t * dims
  | Flip of t * dims (* reflect across the line x = y *)

(* INVARIANTS:
    For each [Text x],
      [x] contains no newlines.
    For each [Hcat (t1, t2, {height = h; width = w})],
      h = height t1 = height t2, and
      w = width t1 + width t2
*)

let fill_generic ch ~width ~height =
  assert (width >= 0);
  assert (height >= 0);
  Fill (ch, {width; height})

let fill ch ~width ~height = fill_generic (Some ch) ~width ~height
let space   ~width ~height = fill_generic None      ~width ~height

let nil = space ~width:0 ~height:0

let hstrut width = space ~width ~height:0
let vstrut height = space ~height ~width:0

let height = function
  | Text _ -> 1
  | Fill (_, d) | Hcat (_, _, d) | Flip (_, d) -> d.height

let width = function
  | Text s -> String.length s
  | Fill (_, d) | Hcat (_, _, d) | Flip (_, d) -> d.width

let dims t = {width = width t; height = height t}

let flip_dims {width = w; height = h} = {width = h; height = w}

let flip = function
  | Flip (t, _) -> t
  | t -> Flip (t, flip_dims (dims t))

let halve n =
  let fst = n / 2 in
  let snd = fst + n mod 2 in
  (fst, snd)

let rec hpad t ~align delta =
  assert (delta >= 0);
  if delta = 0 then t else begin
    let height = height t in
    let pad = space ~height ~width:delta in
    match align with
    | `Left   -> Hcat (t, pad, {height; width = width t + delta})
    | `Right  -> Hcat (pad, t, {height; width = width t + delta})
    | `Center ->
      let (delta1, delta2) = halve delta in
      let t = hpad t ~align:`Left  delta1 in
      let t = hpad t ~align:`Right delta2 in
      t
  end

let vpad t ~align delta =
  let align =
    match align with
    | `Top    -> `Left
    | `Bottom -> `Right
    | `Center -> `Center
  in
  flip (hpad (flip t) ~align delta)

let max_height ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (height t))
let max_width  ts = List.fold ts ~init:0 ~f:(fun acc t -> Int.max acc (width  t))

let valign align ts =
  let h = max_height ts in
  List.map ts ~f:(fun t -> vpad ~align t (h - height t))

let halign align ts =
  let w = max_width ts in
  List.map ts ~f:(fun t -> hpad ~align t (w - width t))

let hcat ?(align = `Top) ?sep ts =
  let ts = Option.fold sep ~init:ts ~f:(fun ts sep -> List.intersperse ts ~sep) in
  let ts = valign align ts in
  match ts with
  | [] -> nil
  | t :: ts ->
    List.fold ~init:t ts ~f:(fun acc t ->
      assert (height acc = height t);
      Hcat (acc, t, {height = height acc; width = width acc + width t}))

let vcat ?(align = `Left) ?sep ts =
  let align =
    match align with
    | `Left   -> `Top
    | `Right  -> `Bottom
    | `Center -> `Center
  in
  let sep = Option.map sep ~f:flip in
  flip (hcat ~align ?sep (List.map ~f:flip ts))

let text ?(align = `Left) str =
  if String.mem str '\n' then
    String.split ~on:'\n' str
    |> List.map ~f:(fun line -> Text line)
    |> vcat ~align
  else
    Text str

let render_aux t ~write_direct ~line_length =
  for j = 0 to height t - 1 do write_direct '\n' (line_length j) j done;
  let write_flipped c j i = write_direct c i j in
  let rec aux t offset write_direct write_flipped =
    match t with
    | Text s ->
      for i = 0 to String.length s - 1 do
        write_direct s.[i] (i + offset.width) offset.height
      done
    | Fill (ch, d) ->
      Option.iter ch ~f:(fun ch ->
        for i = 0 to d.width - 1 do
          for j = 0 to d.height - 1 do
            write_direct ch
              (i + offset.width)
              (j + offset.height)
          done
        done)
    | Flip (t, _) ->
      aux t (flip_dims offset) write_flipped write_direct
    | Hcat (t1, t2, _) ->
      aux t1 offset write_direct write_flipped;
      let offset' =
        {height = offset.height; width = offset.width + width t1}
      in
      aux t2 offset' write_direct write_flipped
  in
  aux t {width = 0; height = 0} write_direct write_flipped

module Padded = struct
  let render t =
    let height = height t in
    let width  = width  t in
    let buf = String.make (height * (1 + width)) ' ' in
    let write_direct c i j = buf.[i + j * (1 + width)] <- c in
    let line_length _ = width in
    render_aux t ~write_direct ~line_length;
    buf
end

module Rstripped = struct
  let line_lengths t =
    let r = Array.create ~len:(height t) 0 in
    let write_direct c i j =
      if not (Char.is_whitespace c) then
        r.(j) <- Int.max r.(j) (i + 1)
    in
    let line_length _ = -1 (* doesn't matter *) in
    render_aux t ~write_direct ~line_length;
    r

  let render t =
    let line_lengths = line_lengths t in
    let (line_offsets, buflen) =
      let height = height t in
      let r = Array.create ~len:height 0 in
      let line_offset j = r.(j - 1) + line_lengths.(j - 1) + 1 in
      for j = 1 to height - 1 do r.(j) <- line_offset j done;
      (r, line_offset height)
    in
    let buf = String.make buflen ' ' in
    let write_direct c i j = buf.[i + line_offsets.(j)] <- c in
    let line_length j = line_lengths.(j) in
    render_aux t ~write_direct ~line_length;
    buf
end

let render ?rstrip t =
  match rstrip with
  | None    -> Padded.render    t
  | Some () -> Rstripped.render t

(* header compression *)

let rec cons x = function
  | [] -> [x]
  | y :: zs ->
    if height x < height y then
      x :: y :: zs
    else
      cons (hcat ~align:`Bottom [x; y]) zs

let compress_table_header ?(sep_width = 2) (`Cols cols) =
  let cols =
    List.map cols ~f:(fun (header, data, align) ->
      (header, Int.max 1 (max_width data), halign align data))
  in
  let header =
    hcat ~align:`Bottom begin
      List.fold_right cols ~init:[] ~f:(fun (header, max_width, _) stairs ->
        let rec loop stairs acc =
          let stop () = cons (vcat ~align:`Left [header; acc]) stairs in
          match stairs with
          | [] -> stop ()
          | x :: rest ->
            if width header + sep_width <= width acc then stop () else
              loop rest
                (hcat [
                  vcat ~align:`Left [
                    fill '|' ~width:1 ~height:(height x - height acc);
                    acc;
                  ];
                  x;
                ])
        in
        loop stairs
          (vcat ~align:`Left [
            text "|";
            hstrut (max_width + sep_width);
          ])
      )
    end
  in
  let rows =
    List.map cols ~f:(fun (_, _, data) -> data)
    |> List.transpose_exn
    |> List.map ~f:(fun row -> hcat row ~sep:(hstrut sep_width))
  in
  (`Header header, `Rows rows)

(* convenience definitions *)

let vsep = vstrut 1

let hsep = hstrut 1

let indent ?(n = 2) t = hcat [hstrut n; t]

let sexp sexp_of_a a = sexp_of_a a |> Sexp.to_string |> text

let textf fmt = ksprintf text fmt

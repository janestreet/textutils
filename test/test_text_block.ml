open! Core.Std
open! Async.Std
open! Import

open Text_block

let yoyoma : t list = [text "yo"; text "yo"; text "ma";]

let lines   = String.split  ~on:'\n'
let unlines = String.concat ~sep:"\n"

let test t =
  let s_padded   = render            t in
  let s_stripped = render ~rstrip:() t in
  begin
    match
      Or_error.try_with (fun () ->
        [%test_result: string] s_stripped
          ~expect:(lines s_padded |> List.map ~f:String.rstrip |> unlines))
    with
    | Ok () -> ()
    | Error e -> print_s ([%sexp_of: Error.t] (Error.tag e ~tag:"RAISED"))
  end;
  print_endline s_padded

let%expect_test _ =
  test (hcat yoyoma);
  [%expect {|
    yoyoma |}]

let%expect_test _ =
  test (hcat ~sep:(hstrut 1) yoyoma);
  [%expect {|
    yo yo ma |}]

let%expect_test _ =
  test (hcat ~sep:(hstrut 2) yoyoma);
  [%expect {|
    yo  yo  ma |}]

let%expect_test _ =
  test (vcat yoyoma);
  [%expect {|
    yo
    yo
    ma |}]

let%expect_test _ =
  test (vcat ~sep:(vstrut 1) yoyoma);
  [%expect {|
    yo

    yo

    ma |}]

let%expect_test _ =
  test (vcat ~sep:(vstrut 2) yoyoma);
  [%expect {|
    yo


    yo


    ma |}]

let sep = text "."

let%expect_test _ =
  test (hcat ~sep [vcat yoyoma; hcat yoyoma]);
  [%expect {|
    yo.yoyoma
    yo
    ma |}]

let%expect_test _ =
  test (hcat ~sep [hcat yoyoma; vcat yoyoma]);
  [%expect {|
    yoyoma.yo
           yo
           ma
    |}]

let%expect_test _ =
  test (vcat ~sep [vcat yoyoma; hcat yoyoma]);
  [%expect {|
    yo
    yo
    ma
    .
    yoyoma
    |}]

let%expect_test _ =
  test (vcat ~sep [hcat yoyoma; vcat yoyoma]);
  [%expect {|
    yoyoma
    .
    yo
    yo
    ma |}]

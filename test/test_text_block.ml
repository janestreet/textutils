open! Core.Std
open! Async.Std
open! Import

open Text_block

let yoyoma : t list = [text "yo"; text "yo"; text "ma";]

let test t =
  invariant t;
  print_endline (render t)

let%expect_test _ =
  test (hcat yoyoma);
  [%expect {|
    yoyoma
  |}]

let%expect_test _ =
  test (hcat ~sep:(hstrut 1) yoyoma);
  [%expect {|
    yo yo ma
  |}]

let%expect_test _ =
  test (hcat ~sep:(hstrut 2) yoyoma);
  [%expect {|
    yo  yo  ma
  |}]

let%expect_test _ =
  test (vcat yoyoma);
  [%expect {|
    yo
    yo
    ma
  |}]

let%expect_test _ =
  test (vcat ~sep:(vstrut 1) yoyoma);
  [%expect {|
    yo

    yo

    ma
  |}]

let%expect_test _ =
  test (vcat ~sep:(vstrut 2) yoyoma);
  [%expect {|
    yo


    yo


    ma
  |}]

let sep = text "."

let%expect_test _ =
  test (hcat ~sep [vcat yoyoma; hcat yoyoma]);
  [%expect {|
    yo.yoyoma
    yo
    ma
  |}]

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
    ma
  |}]

(* lines with trailing whitespace used to tickle a bug *)

let%expect_test _ =
  test
    (vcat [
       hcat [text "a"; text " "];
       hcat [text "b"]
     ]);
  [%expect {|
    a
    b
  |}]

let%expect_test _ =
  test
    (vcat [
       hcat [text "a"; text "    "];
       hcat [text "b"]
     ]);
  [%expect {|
    a
    b
  |}]

let yellow = ansi_escape ~prefix:"[33m" ~suffix:"[39m"

let%expect_test _ =
  test (yellow (vcat yoyoma));
  [%expect {|
    [33myo[39m
    [33myo[39m
    [33mma[39m
  |}]

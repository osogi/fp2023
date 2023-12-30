(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)
open SsaCheck

let test_ssa str =
  match Parser.Parsing.parse_program str with
  | Result.Error s -> Printf.printf "Parser error: \n %s" s
  | Result.Ok glob_lst ->
    (match run (ssa_glob_list glob_lst) (MapString.empty, MapString.empty) with
     | _, Result.Ok _ -> Printf.printf "Pass"
     | _, Result.Error s -> Printf.printf "SSA check failed: \n %s" s)
;;

let%expect_test _ =
  test_ssa "  %res = fcmp one float 4.0, 5.0";
  [%expect {|
    Parser error:
     : end_of_input |}]
;;

let%expect_test _ =
  test_ssa
    "@dd = global i32 0, align 4\n\
     @bb = global i32 32, align 4\n\
     @cc = global i32 32, align 4\n\
     @aa = global i32 32, align 4";
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa "@bb = global i32 0, align 4\n@bb = global i32 32, align 4\n";
  [%expect {|
    SSA check failed:
     Variable bb already was assignmented |}]
;;

let%expect_test _ =
  test_ssa
    "@bb = global i32 0, align 4\n\
     define i32 @bb(){\n\
    \  %1 = call i32 @ds()\n\
    \  %2 = call i32 @ds()\n\
    \  ret i32 0\n\
     }";
  [%expect {|
    SSA check failed:
     Variable bb already was assignmented |}]
;;

let%expect_test _ =
  test_ssa "@dd = global i32 0, align 4\ndefine i32 @bb(i32 %0, i32 %1){ ret i32 0 }";
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa
    "@dd = global i32 0, align 4\ndefine i32 @bb(i32 %0, i32 %1, i35 %0){ret i32 0}";
  [%expect {|
    SSA check failed:
     Variable 0 already was assignmented |}]
;;

let%expect_test _ =
  test_ssa
    "define i32 @ds() {\n\
    \      %1 = alloca i32, align 4\n\
    \      store i32 5, ptr %1, align 4\n\
    \      %2 = load i32, ptr %1, align 4\n\
    \      %3 = load i32, ptr @bb, align 4\n\
    \      %4 = add nsw i32 %3, %2\n\
    \      store i32 %4, ptr @bb, align 4\n\
    \      %5 = load i32, ptr @dd, align 4\n\
    \      store i32 %5, ptr %1, align 4\n\
    \      %6 = load i32, ptr %1, align 4\n\
    \      ret i32 %6\n\
    \    }";
  [%expect {|
    Pass |}]
;;

let%expect_test _ =
  test_ssa
    "define i32 @ds(i32 %6) {\n\
    \      %1 = alloca i32, align 4\n\
    \      store i32 5, ptr %1, align 4\n\
    \      %2 = load i32, ptr %1, align 4\n\
    \      %3 = load i32, ptr @bb, align 4\n\
    \      %4 = add nsw i32 %3, %2\n\
    \      store i32 %4, ptr @bb, align 4\n\
    \      %5 = load i32, ptr @dd, align 4\n\
    \      store i32 %5, ptr %1, align 4\n\
    \      %6 = load i32, ptr %1, align 4\n\
    \      ret i32 %6\n\
    \    }";
  [%expect {|
    SSA check failed:
     Variable 6 already was assignmented |}]
;;


let%expect_test _ =
  test_ssa
    "define i32 @d1(i32 %0) {\n\
    \      %1 = alloca i32, align 4\n\
    \      ret i32 %6\n\
    \    }
    
    define i32 @d2(i32 %0) {\n\
    \      %1 = alloca i32, align 4\n\
    \      ret i32 %6\n\
    \    }";
  [%expect {|
    Pass |}]
;;


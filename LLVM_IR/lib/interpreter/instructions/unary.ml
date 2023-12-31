(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State
open CommonInterpInstructions


(* 
let ifneg res_var tp value =  let* cnst = (get_const_from_value value) in
   


let execute_unary_operation : Ast.unary_operation -> (state, 'a) t = match op with
| Ast.Fneg (res_var, tp, value) -> (ifneg res_var tp value)

(* let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction {| %23 = fneg float 123.0   |};
  [%expect
    {|
    (Unary
       (Fneg ((LocalVar "23"), TFloat, (FromVariable ((LocalVar "val"), TFloat))
          ))) |}]
;; *) *)

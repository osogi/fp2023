(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)
open State

open CommonInterpInstructions

let real_shl tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.shift_left x (Int64.to_int y)) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at shl" in
    CVoid
;;

let real_lshr tp x y =
  match tp with
  | Ast.TInteger sz ->
    Common.IrInts.create (Int64.shift_right_logical x (Int64.to_int y)) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at lshr" in
    CVoid
;;

let real_ashr tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.shift_right x (Int64.to_int y)) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at ashr" in
    CVoid
;;

let real_and tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.logand x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at and" in
    CVoid
;;

let real_or tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.logor x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at or" in
    CVoid
;;

let real_xor tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.logxor x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at xor" in
    CVoid
;;

let launch_bitwise_operation : Ast.bitwise_binary_operation -> (state, instr_launch_res) t
  =
  fun instr ->
  (match instr with
   | Ast.Shl (var, tp, v1, v2) -> write_binop_res tp real_shl is_int v1 v2 var
   | Ast.Lshr (var, tp, v1, v2) -> write_binop_res tp real_lshr is_int v1 v2 var
   | Ast.Ashr (var, tp, v1, v2) -> write_binop_res tp real_ashr is_int v1 v2 var
   | Ast.And (var, tp, v1, v2) -> write_binop_res tp real_and is_int v1 v2 var
   | Ast.Or (var, tp, v1, v2) -> write_binop_res tp real_or is_int v1 v2 var
   | Ast.Xor (var, tp, v1, v2) -> write_binop_res tp real_xor is_int v1 v2 var)
  *> return None
;;

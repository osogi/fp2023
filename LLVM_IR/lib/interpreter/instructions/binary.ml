(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)
open State

open CommonInterpInstructions

let real_add tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.add x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at add" in
    CVoid
;;

let real_mul tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.mul x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at mul" in
    CVoid
;;

let real_sub tp x y =
  match tp with
  | Ast.TInteger sz -> Common.IrInts.create (Int64.sub x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at sub" in
    CVoid
;;

let real_udiv tp x y =
  match tp with
  | Ast.TInteger sz ->
    if Int64.equal 0L y
    then raise Division_by_zero
    else Common.IrInts.create (Int64.div x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at udiv" in
    CVoid
;;

let real_urem tp x y =
  match tp with
  | Ast.TInteger sz ->
    if Int64.equal 0L y
    then raise Division_by_zero
    else Common.IrInts.create (Int64.rem x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at urem" in
    CVoid
;;

let real_sdiv tp x y =
  match tp with
  | Ast.TInteger sz ->
    let x = Common.IrInts.sget x sz in
    let y = Common.IrInts.sget y sz in
    if Int64.equal 0L y
    then raise Division_by_zero
    else Common.IrInts.create (Int64.div x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at urem" in
    CVoid
;;

let real_srem tp x y =
  match tp with
  | Ast.TInteger sz ->
    let x = Common.IrInts.sget x sz in
    let y = Common.IrInts.sget y sz in
    if Int64.equal 0L y
    then raise Division_by_zero
    else Common.IrInts.create (Int64.rem x y) sz
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at urem" in
    CVoid
;;

let real_fadd _tp x y = Ast.CFloat (Float.add x y)
let real_fmul _tp x y = Ast.CFloat (Float.mul x y)
let real_fsub _tp x y = Ast.CFloat (Float.sub x y)
let real_fdiv _tp x y = Ast.CFloat (Float.div x y)
let real_frem _tp x y = Ast.CFloat (Float.rem x y)

let launch_binary_operation : Ast.binary_operation -> (state, instr_launch_res) t =
  fun instr ->
  (match instr with
   | Ast.Add (var, tp, v1, v2) -> write_binop_res tp real_add is_int v1 v2 var
   | Ast.Mul (var, tp, v1, v2) -> write_binop_res tp real_mul is_int v1 v2 var
   | Ast.Sub (var, tp, v1, v2) -> write_binop_res tp real_sub is_int v1 v2 var
   | Ast.Sdiv (var, tp, v1, v2) -> write_binop_res tp real_sdiv is_int v1 v2 var
   | Ast.Srem (var, tp, v1, v2) -> write_binop_res tp real_srem is_int v1 v2 var
   | Ast.Udiv (var, tp, v1, v2) -> write_binop_res tp real_udiv is_int v1 v2 var
   | Ast.Urem (var, tp, v1, v2) -> write_binop_res tp real_urem is_int v1 v2 var
   | Ast.Fadd (var, tp, v1, v2) -> write_binop_res tp real_fadd is_float v1 v2 var
   | Ast.Fmul (var, tp, v1, v2) -> write_binop_res tp real_fmul is_float v1 v2 var
   | Ast.Fdiv (var, tp, v1, v2) -> write_binop_res tp real_fdiv is_float v1 v2 var
   | Ast.Frem (var, tp, v1, v2) -> write_binop_res tp real_frem is_float v1 v2 var
   | Ast.Fsub (var, tp, v1, v2) -> write_binop_res tp real_fsub is_float v1 v2 var)
  *> return None
;;

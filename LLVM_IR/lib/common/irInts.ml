(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y
;;

let cut x n = modulo x (Int.shift_left 1 n)
let uget x n = cut x n

let sget x n =
  let mag = 1 lsl (n-1) in
  let neg = x land mag <> 0 in
  let value = cut x (n-1) in
  if neg then value-mag else value
;;

let create x n = Ast.CInteger (n, cut x n)

let bin_op int_op l r =
  match l, r with
  | Ast.CInteger (sz1, v1), Ast.CInteger (sz2, v2) when sz1 == sz2 -> Some (create(int_op (uget v1 sz1) (uget v2 sz2) sz1))
  | _ -> None
;;


let sbin_op int_op l r =
  match l, r with
  | Ast.CInteger (sz1, v1), Ast.CInteger (sz2, v2) when sz1 == sz2 -> Some (create(int_op (sget v1 sz1) (sget v2 sz2) sz1))
  | _ -> None
;;

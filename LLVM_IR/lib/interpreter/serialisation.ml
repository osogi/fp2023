(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

let div_up x y =
  let res = x / y in
  if y * res != x then res + 1 else res
;;

let get_n_byte x n = Char.chr ((x land (0xff lsl (8 * n))) lsr (8 * n))

let serialise_int sz value =
  let len = div_up sz 8 in
  List.init len (get_n_byte value)
;;


let serialise : Ast.const -> char list = function
  | Ast.CVoid -> []
  | Ast.CInteger (sz, value) -> serialise_int sz value
;;

let deserialise_int sz lst =
  List.fold_left
;;

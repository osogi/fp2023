(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val cut : int64 -> int -> int64
val uget : int64 -> int -> int64
val sget : int64 -> int -> int64
val create : int64 -> int -> Ast.const

val bin_op
  :  (int64 -> int64 -> int -> int64)
  -> Ast.const
  -> Ast.const
  -> (int -> Ast.const) option

val sbin_op
  :  (int64 -> int64 -> int -> int64)
  -> Ast.const
  -> Ast.const
  -> (int -> Ast.const) option

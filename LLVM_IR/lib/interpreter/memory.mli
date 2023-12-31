(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val put_cnst_in_heap : int -> Ast.const -> (State.state, unit) State.t
val take_cnst_in_heap : int -> Ast.tp -> (State.state, Ast.const) State.t
val alloc_stack : int -> (State.state, int) State.t
val free_stack : int -> (State.state, unit) State.t

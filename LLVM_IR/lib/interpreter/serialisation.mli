(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

val raw_date_len : Ast.tp -> int
val serialise : Ast.const -> char list
val deserialise : Ast.tp -> char list -> Ast.const

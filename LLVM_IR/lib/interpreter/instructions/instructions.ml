(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State
include CommonInterpInstructions

let launch_instruction: Ast.instruction -> (state, instr_launch_res) t = function 
| _ -> return None
(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State
include CommonInterpInstructions

let launch_instruction : Ast.instruction -> (state, instr_launch_res) t = function
  | Ast.Terminator inst -> Termainator.launch_terminator_instruction inst
  | Ast.Unary inst -> Unary.launch_unary_operation inst
  | Ast.Binary inst -> Binary.launch_binary_operation inst
  | Ast.BitwiseBinary inst -> Bitwise.launch_bitwise_operation inst
  | Ast.Vector inst -> Vector.launch_vector_instruction inst
  | Ast.Aggregate inst -> Aggregate.launch_aggregate_instruction inst
  | Ast.MemoryAddress inst -> MemoryAddress.launch_memory_address_operation inst
  | _ -> return None
;;

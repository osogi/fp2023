(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State
type instr_launch_res = 
| Ret of Ast.const
| Jmp of Ast.basic_block
| None 



let get_const_from_value : Ast.value -> (state, Ast.const) t = function
  | Ast.Const x -> return x
  | Ast.FromVariable (var, exp_tp) ->
    let* cnst = read_var var in
    let real_tp = Ast.const_to_tp cnst in
    if real_tp == exp_tp
    then return cnst
    else
      fail
        (Printf.sprintf
           "Variable %s is of type %s, but %s was expected"
           (Ast.show_variable var)  (Ast.show_tp real_tp)  (Ast.show_tp exp_tp))
;;

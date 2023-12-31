(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State

type instr_launch_res =
  | Ret of Ast.const
  | Jmp of Ast.basic_block
  | None

let err_type exp cnst =
  fail
    (Printf.sprintf
       "expected %s, but get %s\n"
       (Ast.show_tp exp)
       (Ast.show_tp (Ast.const_to_tp cnst)))
;;

let is_block  =
  fun cnst ->
  match cnst with
  | Ast.CLabel x -> return x
  | _ -> err_type Ast.TLabel cnst
;;

let is_bool =
  fun cnst ->
  match cnst with
  | Ast.CInteger (1, x) -> return (Int64.equal 1L x)
  | _ -> err_type (Ast.TInteger 1) cnst
;;

let is_int =
  fun cnst ->
  match cnst with
  | Ast.CInteger (_, x) -> return x
  | _ -> err_type (Ast.TInteger 0) cnst
;;

let is_float =
  fun cnst ->
  match cnst with
  | Ast.CFloat x -> return x
  | _ -> err_type (Ast.TFloat) cnst
;;

let is_vector is_elem = 
fun cnst ->
  match cnst with
  | Ast.CVector x -> map_list is_elem x
  | _ -> err_type (Ast.TVector (0, Ast.TVoid)) cnst


let get_const_from_value : Ast.value -> (state, Ast.const) t = function
  | Ast.Const x -> return x
  | Ast.FromVariable (var, exp_tp) ->
    let* cnst = read_var var in
    let real_tp = Ast.const_to_tp cnst in
    if Ast.tp_equal real_tp exp_tp
    then return cnst
    else
      fail
        (Printf.sprintf
           "Variable %s is of type %s, but %s was expected"
           (Ast.show_variable var)
           (Ast.show_tp real_tp)
           (Ast.show_tp exp_tp))
;;

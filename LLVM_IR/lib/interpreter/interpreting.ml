(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State

let glob_sect = 1024
(*  *)

let stack_sect = 0xcf000000
let empty_state : state = MapString.empty, MapString.empty, MapInt.empty, stack_sect

let assign_globs : Ast.glob_list -> (state, unit) t =
  fun glob_lst ->
  let count_addr addr glob =
    let tp, _, _, align = glob in
    match tp with
    | Ast.TFunc (_, _) -> addr, addr
    | _ ->
      let na = Memory.align_addr addr align true in
      let res = na + Serialisation.raw_date_len tp in
      res, na
  in
  let _, addrs = List.fold_left_map count_addr glob_sect glob_lst in
  let addrs_and_globs = List.combine addrs glob_lst in
  let assign_glob : int * (Ast.tp * Ast.variable * Ast.const * int) -> (state, unit) t =
    fun (addr, glob) ->
    let tp, var, value, _ = glob in
    let cnst =
      match tp with
      | Ast.TFunc (_, _) -> value
      | _ -> Ast.CPointer addr
    in
    write_var var cnst
  in
  map_list assign_glob addrs_and_globs *> return ()
;;

let allocate_globs : Ast.glob_list -> (state, unit) t =
  fun glob_lst ->
  let alloc_glob : Ast.tp * Ast.variable * Ast.const * int -> (state, unit) t =
    fun (_, var, cnst, _) ->
    let* ptr_cnst = read_var var in
    match ptr_cnst with
    | Ast.CPointer x -> Memory.put_cnst_in_heap x cnst
    | _ -> return ()
  in
  map_list alloc_glob glob_lst *> return ()
;;

let init_state : Ast.glob_list -> (state, unit) t =
  fun glob_lst -> assign_globs glob_lst *> allocate_globs glob_lst
;;

let rec launch_block : Ast.basic_block -> (state, Ast.const) t =
  fun bb ->
  let* instr_res = map_list Instructions.launch_instruction bb in
  let last_instr = List.nth instr_res (List.length instr_res - 1) in
  match last_instr with
  | Instructions.Jmp x -> launch_block x
  | Instructions.Ret x -> return x
  | Instructions.None ->
    fail "Impossible error: last instruction in block should have some result\n"
;;

let launch_function : Ast.func -> Ast.const list -> (state, Ast.const) t =
  fun fnc params_val ->
  let* old_loc, old_glb, old_heap, old_stack = read in
  write (MapString.empty, old_glb, old_heap, old_stack)
  *>
  let init_var (param, cnst) = write_var param cnst in
  let params_cnst = List.combine fnc.parameters params_val in
  map_list init_var params_cnst
  *> map_list init_var fnc.basic_blocks
  *>
  let _, fb = List.hd fnc.basic_blocks in
  let* fb = Instructions.is_block fb in
  launch_block fb
  <* Memory.free_stack old_stack
  <* let* _, glb, heap, stack = read in
     write (old_loc, glb, heap, stack)
;;

let interpritate_ast : Ast.glob_list -> (state, Ast.const) t =
  fun glb_lst ->
  init_state glb_lst
  *> let* main = read_var (Ast.GlobalVar "main") in
     match main with
     (* TODO: add check for main arguments*)
     | Ast.CFunc x -> launch_function x []
     | _ -> fail "Error: main is not function\n"
;;

let interp_test str =
  match Parser.Parsing.parse_program str with
  | Result.Ok x ->
    (match run (interpritate_ast x) empty_state with
     | _, Result.Ok x -> Printf.printf "%s\n" (Ast.show_const x)
     | _, Result.Error s -> Printf.printf "Error: %s!\n" s)
  | _ -> Printf.printf "Parser error\n"
;;

let%expect_test _ =
  interp_test
    {|  
; Function Attrs: noinline nounwind optnone uwtable
define <3 x float> @main(){
  %1 = fneg <3 x float> < float 1.2,  float -3.4,  float 5.6>
  br  label %3
  3:
  ret <3 x float> %1
}
      |};
  [%expect {|
    (CVector [(CFloat -1.2); (CFloat 3.4); (CFloat -5.6)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
; Function Attrs: noinline nounwind optnone uwtable
define <3 x float> @main(){
  %1 = fneg <3 x float> < float 1.2,  float -3.4,  float 5.6>
  %3 = fadd <3 x float> %1, < float 10.0,  float 8.0,  float 7.0>
  ret <3 x float> %3
}
      |};
  [%expect {|
    (CVector [(CFloat 8.8); (CFloat 11.4); (CFloat 1.4)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 8, 0
  %b = add i4 2, 0
  %c = sdiv i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| (CInteger (4, 12L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 9, 0
  %b = add i4 2, 0
  %c = srem i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| (CInteger (4, 15L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i4 @main(){
  %a = add i4 9, 0
  %b = add i4 0, 0
  %c = srem i4 %a, %b
  ret i4 %c
}
      |};
  [%expect {| Error: Runtime error: Division by 0! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define float @main(){
  %a = fadd float 0.0, 0.0
  %b = fadd float 2.0, 0.0
  %c = fdiv float %b, %a
  ret float %c
}
      |};
  [%expect {| (CFloat infinity) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <4 x i1> @main(){
  %a = xor <4 x i1> <i1 0, i1 0, i1 1, i1 1>, <i1 0, i1 1, i1 0, i1 1>
  ret <4 x i1> %a
}
      |};
  [%expect
    {|
      (CVector
         [(CInteger (1, 0L)); (CInteger (1, 1L)); (CInteger (1, 1L));
           (CInteger (1, 0L))]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define float @main(){
  %a = extractelement <3 x float> < float 1.2,  float -3.4,  float 5.6>, i32 1
  ret float %a
}
      |};
  [%expect {|
      (CFloat -3.4) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <3 x float> @main(){
  %a = insertelement <3 x float> < float 1.2,  float -3.4,  float 5.6>,  float 82.0, i32 1
  ret <3 x float> %a
}
      |};
  [%expect {|
      (CVector [(CFloat 1.2); (CFloat 82.); (CFloat 5.6)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define <2 x float> @main(){
  %a = shufflevector
   <3 x float> < float 1.2,  float -3.4,  float 5.6>,
    <3 x float>  < float 10.0,  float 8.0,  float 7.0>,
     <2 x i32> <i32 1, i32 5>
  ret <2 x float> %a
}
      |};
  [%expect {|
      (CVector [(CFloat -3.4); (CFloat 7.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %a = extractvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, 0, 1
  ret i32 %a
}
      |};
  [%expect {|
      (CInteger (32, 32L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %a = insertvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, i32 4, 0, 1
  ret {[3 x i32], float} %a
}
      |};
  [%expect
    {|
      (CStruct
         [(CArr [(CInteger (32, 21L)); (CInteger (32, 4L)); (CInteger (32, 43L))]);
           (CFloat 50.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %a = insertvalue {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.0}, i32 4, 1
  ret {[3 x i32], float} %a
}
      |};
  [%expect
    {|
      Error: Want to insert value with type (TInteger 32) instead of value with type TFloat! |}]
;;

let%expect_test _ =
  interp_test
    {|  
define i32 @main(){
  %a = alloca {[3 x i32], float}, align 4
  store {[3 x i32], float} {[3 x i32][i32 21, i32 32, i32 43], float 50.}, ptr %a, align 4
  %b = load {[3 x i32], float}, ptr %a, align 4
  ret {[3 x i32], float} %b
}
      |};
  [%expect
    {|
      (CStruct
         [(CArr [(CInteger (32, 21L)); (CInteger (32, 32L)); (CInteger (32, 43L))]);
           (CFloat 50.)]) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 4

define i32 @main(){
  %b = load i32, ptr @dd, align 4
  ret i32 %b
}
      |};
  [%expect {|
      (CInteger (32, 312312L)) |}]
;;

let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define ptr @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, ptr @dd, i32 1, i32 0, i32 2
  ret ptr %b
}
      |};
  [%expect {|
      Error: Runtime error: invalid getelementptr indices! |}]
;;


let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define ptr @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, ptr @dd, i32 1, i32 1, i32 2
  ret ptr %b
}
      |};
  [%expect {|
      (CPointer 2032) |}]
;;


let%expect_test _ =
  interp_test
    {|  
@dd = global i32 312312, align 1000

define < 2 x ptr> @main(){
  %b = getelementptr { i32, [3 x  i32], i32 }, < 2 x ptr> <ptr @dd, ptr @dd>, i32 0
  ret < 2 x ptr>  %b
}
      |};
  [%expect {|
      (CPointer 2032) |}]
;;
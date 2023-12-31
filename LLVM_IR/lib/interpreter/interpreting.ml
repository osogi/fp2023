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

type block_launch_res =
  | NextBlock of Ast.basic_block
  | Return of Ast.const

let launch_block : Ast.basic_block -> (state, block_launch_res) t =
  fun bb ->
  let* instr_res = map_list Instructions.launch_instruction bb in
  let last_instr = List.nth instr_res (List.length instr_res - 1) in
  match last_instr with
  | Instructions.Jmp x -> return (NextBlock x)
  | Instructions.Ret x -> return (Return x)
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
  map_list init_var params_cnst *> map_list init_var fnc.basic_blocks *> return Ast.CVoid
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

(*
   let%test _ =
let _ = 
  match Parser.Parsing.parse_program "@dd = global i32 0, align 1
  @bb = global i32 33, align 8
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define i32 @ds() {
    ret i32 %6
  }
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define i32 @main() {
    %2 = alloca i32, align 4
    %3 = alloca i32, align 4
    store i32 %0, ptr %3, align 4
    %4 = load i32, ptr %3, align 4
    %5 = icmp slt i32 %4, 1
    br i1 %5, label %6, label %7
  
  6:                                                ; preds = %1
    store i32 1, ptr %2, align 4
    br label %13
  
  7:                                                ; preds = %1
    %8 = load i32, ptr %3, align 4
    %9 = sub i32 %8, 1
    %10 = call i32 @fac(i32 %9)
    %11 = load i32, ptr %3, align 4
    %12 = mul i32 %10, %11
    store i32 %12, ptr %2, align 4
    br label %13
  
  13:                                               ; preds = %7, %6
    %14 = load i32, ptr %2, align 4
    ret i32 %14
  }
  " with
  | Result.Ok x ->
    (match run (interpritate_ast x) empty_state with
     | st, _ ->
       let loc, glob, heap, stack = st in
       (* Printf.printf "%s" (show_map_var glob)) *)
       Printf.printf "%s" (show_map_heap heap))

  | _ -> Printf.printf "Parser error\n"
     in 
     true
;;  *)

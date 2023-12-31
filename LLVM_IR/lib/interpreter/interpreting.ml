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

(* let%test _ =
  let _ =
    match
      Parser.Parsing.parse_program
        {|  
      @dd = global i32 0, align 4
@bb = global i32 32, align 4

; Function Attrs: noinline nounwind optnone uwtable
define i32 @ds() {
  %1 = alloca i32, align 4
  store i32 5, ptr %1, align 4
  %2 = load i32, ptr %1, align 4
  %3 = load i32, ptr @bb, align 4
  %4 = add nsw i32 %3, %2
  store i32 %4, ptr @bb, align 4
  %5 = load i32, ptr @dd, align 4
  store i32 %5, ptr %1, align 4
  %6 = load i32, ptr %1, align 4
  ret i32 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main(){
4:
  %1 = call i32 @ds()
  br label %4
3:
  %2 = call i32 @ds()
  ret i32 0
}

      |}
    with
    | Result.Ok x ->
      (match run (interpritate_ast x) empty_state with
       | st, Result.Ok x ->
         let loc, glob, heap, stack = st in
         (* Printf.printf "%s" (show_map_var glob)) *)
         Printf.printf "%s" (show_map_heap heap)
       | st, Result.Error s -> Printf.printf "Error: %s!\n" s)
    | _ -> Printf.printf "Parser error\n"
  in
  true
;; *)

(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open State

let glob_sect = 1024
  (*  *)
let stack_sect = 0xcf000000
let init_state : state = MapString.empty, MapString.empty, MapInt.empty, stack_sect

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

let allocate_globs : Ast.glob_list -> (state, unit) t = fun glob_lst -> 
  let alloc_glob : (Ast.tp * Ast.variable * Ast.const * int) -> (state, unit) t = fun (_, var, cnst, _) -> 
    let* ptr_cnst = read_var var in 
    match ptr_cnst with
    | Ast.CPointer x -> Memory.put_cnst_in_heap x cnst
      |_ -> return () 
    in
    map_list alloc_glob glob_lst *> return ()
let init_state : Ast.glob_list -> (state, unit) t = fun glob_lst -> assign_globs glob_lst *> allocate_globs glob_lst


(* let%test _ =
let _ = 
  match Parser.Parsing.parse_program "@dd = global i32 0, align 1
  @bb = global i32 33, align 8
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define i32 @ds() {
    ret i32 %6
  }
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define i32 @main(){
    %1 = call i32 @ds()
    %2 = call i32 @ds()
    ret i32 0
  }
  " with
  | Result.Ok x ->
    (match run (init_globs x) init_state with
     | st, _ ->
       let loc, glob, heap, stack = st in
       (* Printf.printf "%s" (show_map_var glob)) *)
       Printf.printf "%s" (show_map_heap heap))

  | _ -> Printf.printf "Parser error\n"
     in 
     true
;; *)

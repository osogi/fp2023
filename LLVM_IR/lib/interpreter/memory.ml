(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)
open State

let put_cnst_in_heap : int -> Ast.const -> (state, unit) t =
  fun addr cnst ->
  let lst = Serialisation.serialise cnst in
  let indxs =
    List.init (Serialisation.raw_date_len (Ast.const_to_tp cnst)) (fun x -> addr + x)
  in
  let bts = List.to_seq (List.combine indxs lst) in
  write_bytes bts
;;

let take_cnst_in_heap : int -> Ast.tp -> (state, Ast.const) t =
  fun addr tp ->
  let indxs = List.init (Serialisation.raw_date_len tp) (fun x -> addr + x) in
  let* bts = map_list read_byte indxs in
  return (Serialisation.deserialise tp bts)
;;

let alloc_stack : int -> (state, int) t =
  fun len ->
  let* old_local, old_global, old_heap, old_stack = read in
  let addr = old_stack - len - 1 in
  write (old_local, old_global, old_heap, addr) *> return addr
;;

let free_stack : int -> (state, unit) t =
  fun new_stack ->
  let* old_local, old_global, old_heap, old_stack = read in
  let new_heap =
    MapInt.filter
      (fun cur_addr _ -> cur_addr < new_stack && cur_addr >= old_stack)
      old_heap
  in
  write (old_local, old_global, new_heap, new_stack)
;;

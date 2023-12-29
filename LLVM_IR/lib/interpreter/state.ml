(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

module STATE_MONAD = struct
  type ('st, 'a) t = 'st -> 'st * ('a, string) Result.t

  let return x : _ = fun st -> st, Result.ok x
  let fail err st = st, Result.error err

  let ( >>= ) : 's 'a 'b. ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t =
    fun x f : _ ->
    fun st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error s -> st, Result.error s
  ;;

  let read : ('st, 'st) t = fun st -> st, Result.ok st
  let write : 'st -> ('st, unit) t = fun s _oldstate -> s, Result.ok ()
  let run : ('st, 'a) t -> 'st -> 'st * ('a, string) Result.t = fun f st -> f st
  let ( let* ) = ( >>= )
end

module MapString = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

module MapInt = struct
  include Map.Make (Int)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%d\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

type map_heap = char MapInt.t [@@deriving show { with_path = false }]
type map_var = Ast.const MapString.t [@@deriving show { with_path = false }]
type bytes = (int * char) Seq.t
type state = map_var * map_var * map_heap

include STATE_MONAD

let read_var : Ast.variable -> (state, Ast.const) t =
  let find_var name map =
    let value = MapString.find_opt name map in
    match value with
    | Some x -> return x
    | None -> fail (Printf.sprintf "Runtime error: Variable %s is not initialized" name)
  in
  fun variable ->
    let* local, glob, _ = read in
    match variable with
    | Ast.GlobalVar name -> find_var name glob
    | Ast.LocalVar name -> find_var name local
;;

let write_var : Ast.variable * Ast.const -> state -> (state, unit) t =
  fun (key, value) (old_local, old_global, old_heap) ->
  match key with
  | Ast.GlobalVar name -> write (old_local, MapString.add name value old_global, old_heap)
  | Ast.LocalVar name -> write (MapString.add name value old_local, old_global, old_heap)
;;

let read_byte : int -> (state, char) t =
  fun number ->
  let* _, _, heap = read in
  let bt = MapInt.find_opt number heap in
  match bt with
  | Some x -> return x
  | None ->
    fail (Printf.sprintf "Runtime error: Byte number %x is not initialized" number)
;;

let read_bytes : int -> int -> (state, char list) t =
  fun first len st -> 
   let init_el num = read_byte (first + num) in
  let buf_list = List.init len init_el in
  let f (res : (state, char) t) lst : (char list, string) Result.t =
    match lst, res st with
    | Result.Ok lst, (_, Result.Ok x) -> Result.ok (x :: lst)
    | _, (_, Result.Error s) -> Result.error s
    | Result.Error s, _ -> Result.error s
  in (st, List.fold_right f buf_list (Result.ok []))
  
;;

let write_bytes : bytes -> state -> (state, unit) t =
  fun bts (old_local, old_global, old_heap) ->
  write (old_local, old_global, MapInt.add_seq bts old_heap)
;;

(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom
open Common
open Values
open Types

let parse_instruction_result = parse_local_variable <* whitespaces <* char '='

let parse_terminator_instruction =
  let iret =
    word "ret" *> whitespaces *> parse_type_with_value
    >>| function
    | tp, value -> Ast.Ret (tp, value)
  and ibr =
    word "br" *> whitespaces *> type_with_value Ast.TLabel >>| fun value -> Ast.Br value
  and ibr_cond =
    word "br"
    *> lift3
         (fun b l1 l2 -> Ast.BrCond (b, l1, l2))
         (type_with_value (Ast.TInteger 1) <* comma)
         (type_with_value Ast.TLabel <* comma)
         (type_with_value Ast.TLabel)
  and iswitch =
    word "switch"
    *> let* value_type, switch_value = parse_type_with_value <* comma in
       let* default_dest = type_with_value Ast.TLabel in
       let* switch_list =
         whitespaces
         *> char '['
         *> whitespaces
         *> many
              (lift2
                 (fun x y -> x, y)
                 (type_with_value value_type <* comma)
                 (type_with_value Ast.TLabel))
         <* whitespaces
         <* char ']'
       in
       return (Ast.Switch (value_type, switch_value, default_dest, switch_list))
  and iindirectbr =
    word "indirectbr"
    *> let* destination = type_with_value Ast.TPointer <* comma in
       let* _ =
         whitespaces
         *> char '['
         *> whitespaces
         *> sep_by comma (type_with_value Ast.TLabel)
         <* whitespaces
         <* char ']'
       in
       return (Ast.Indirectbr destination)
  and iunreachable = 
  word "unreachable" *> return Ast.Unreachable
  in
  whitespaces *> choice [ iret; ibr; ibr_cond; iswitch; iindirectbr; iunreachable ]
;;

let parse_binary_operation =
  let help (mnem : string) (bin_op : Ast.binary_operation_body -> Ast.binary_operation) =
    parse_instruction_result
    >>= fun var ->
    whitespaces *> word mnem *> whitespaces *> parse_type_with_value2
    >>= function
    | tp, v1, v2 -> return (bin_op (var, tp, v1, v2))
  in
  whitespaces
  *> choice
       [ help "add" (fun x -> Ast.Add x)
       ; help "fadd" (fun x -> Ast.Fadd x)
       ; help "mul" (fun x -> Ast.Mul x)
       ; help "fmul" (fun x -> Ast.Fmul x)
       ; help "sub" (fun x -> Ast.Sub x)
       ; help "fsub" (fun x -> Ast.Fsub x)
       ; help "udiv" (fun x -> Ast.Udiv x)
       ; help "sdiv" (fun x -> Ast.Sdiv x)
       ; help "fdiv" (fun x -> Ast.Fdiv x)
       ; help "urem" (fun x -> Ast.Urem x)
       ; help "srem" (fun x -> Ast.Srem x)
       ; help "frem" (fun x -> Ast.Frem x)
       ]
;;

let parse_other_operation =
  let iicmp =
    lift3
      (fun var cond (tp, v1, v2) -> Ast.Icmp (var, cond, tp, v1, v2))
      parse_instruction_result
      (whitespaces *> word "icmp" *> whitespaces *> parse_word)
      (whitespaces *> parse_type_with_value2)
  and icall =
    lift4
      (fun var ret_tp vptr arg_lst -> Ast.Call (var, ret_tp, vptr, arg_lst))
      parse_instruction_result
      (whitespaces *> word "call" *> whitespaces *> parse_additional_type)
      (whitespaces *> parse_value Ast.TPointer)
      (whitespaces
       *> char '('
       *> sep_by
            comma
            (whitespaces *> parse_type_with_value
             >>= function
             | _, value -> return value)
       <* whitespaces
       <* char ')')
  in
  whitespaces *> choice [ iicmp; icall ]
;;

let parse_memory_instruction =
  let parse_align = comma *> word "align" *> whitespaces *> parse_integer <|> return 1 in
  let ialloca =
    lift4
      (fun var tp value align -> Ast.Alloca (var, tp, value, align))
      parse_instruction_result
      (whitespaces *> word "alloca" *> parse_main_type)
      (comma *> parse_type_with_value
       >>= (function
              | Ast.TInteger _, value -> return value
              | _ -> fail "Parser error: excepted integer type")
       <|> return (Ast.Const (Ast.CInteger (1, 1))))
      (whitespaces *> parse_align)
  and istore =
    lift3
      (fun (tp, value) vptr align -> Ast.Store (tp, value, vptr, align))
      (whitespaces *> word "store" *> whitespaces *> parse_type_with_value)
      (comma *> type_with_value Ast.TPointer)
      parse_align
  and iload =
    lift4
      (fun res tp vptr align -> Ast.Load (res, tp, vptr, align))
      parse_instruction_result
      (whitespaces *> word "load" *> parse_main_type)
      (comma *> type_with_value Ast.TPointer)
      parse_align
  in
  whitespaces *> choice [ ialloca; istore; iload ]
;;

let parse_instruction : Ast.instruction t =
  choice
    [ (parse_terminator_instruction >>| fun ins -> Ast.Terminator ins)
    ; (parse_binary_operation >>| fun ins -> Ast.Binary ins)
    ; (parse_other_operation >>| fun ins -> Ast.Other ins)
    ; (parse_memory_instruction >>| fun ins -> Ast.MemoryAddress ins)
    ]
;;

(* ##########################################################*)
(* ################ TERMINATOR ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "ret i32 %14";
  [%expect
    {|
    (Terminator
       (Ret ((TInteger 32), (FromVariable ((LocalVar "14"), (TInteger 32)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "br label %13";
  [%expect {|
    (Terminator (Br (FromVariable ((LocalVar "13"), TLabel)))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction " br i1 %5, label %6, label %7";
  [%expect
    {|
    (Terminator
       (BrCond ((FromVariable ((LocalVar "5"), (TInteger 1))),
          (FromVariable ((LocalVar "6"), TLabel)),
          (FromVariable ((LocalVar "7"), TLabel))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| switch i32 %val, label %otherwise [ i32 0, label %onzero
  i32 1, label %onone
  i32 2, label %ontwo ] |};
  [%expect
    {|
    (Terminator
       (Switch ((TInteger 32), (FromVariable ((LocalVar "val"), (TInteger 32))),
          (FromVariable ((LocalVar "otherwise"), TLabel)),
          [((Const (CInteger (32, 0))),
            (FromVariable ((LocalVar "onzero"), TLabel)));
            ((Const (CInteger (32, 1))),
             (FromVariable ((LocalVar "onone"), TLabel)));
            ((Const (CInteger (32, 2))),
             (FromVariable ((LocalVar "ontwo"), TLabel)))
            ]
          ))) |}]
;;


let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| indirectbr ptr %Addr, [ label %bb1, label %bb2, label %bb3 ] |};
  [%expect
    {|
    (Terminator (Indirectbr (FromVariable ((LocalVar "Addr"), TPointer)))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    {| unreachable |};
  [%expect
    {|
    (Terminator Unreachable) |}]
;;



(* ##########################################################*)
(* #################### BINARY ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %9 = sub i32 %8, 1";
  [%expect
    {|
    (Binary
       (Sub
          ((LocalVar "9"), (TInteger 32),
           (FromVariable ((LocalVar "8"), (TInteger 32))),
           (Const (CInteger (32, 1)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %12 = mul i32 %10, %11";
  [%expect
    {|
    (Binary
       (Mul
          ((LocalVar "12"), (TInteger 32),
           (FromVariable ((LocalVar "10"), (TInteger 32))),
           (FromVariable ((LocalVar "11"), (TInteger 32)))))) |}]
;;


(* ##########################################################*)
(* ##################### OTHER ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %10 = call i32 @fac(i32 %9)  ";
  [%expect
    {|
    (Other
       (Call ((LocalVar "10"), (TInteger 32),
          (Const (CPointer (PointerGlob (GlobalVar "fac")))),
          [(FromVariable ((LocalVar "9"), (TInteger 32)))]))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction " %5 = icmp slt i32 %4, 1";
  [%expect
    {|
    (Other
       (Icmp ((LocalVar "5"), "slt", (TInteger 32),
          (FromVariable ((LocalVar "4"), (TInteger 32))),
          (Const (CInteger (32, 1)))))) |}]
;;

(* ##########################################################*)
(* ################ MEMORY ADDRESS ##########################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %2 = alloca i32, align 4";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (1, 1))), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %2 = alloca i32";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (1, 1))), 1))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %2 = alloca i32, i32 4, align 4";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32), (Const (CInteger (32, 4))), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "   %2 = alloca i32, i32 %kakadu";
  [%expect
    {|
      (MemoryAddress
         (Alloca ((LocalVar "2"), (TInteger 32),
            (FromVariable ((LocalVar "kakadu"), (TInteger 32))), 1))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "%11 = load i32, ptr %3, align 4";
  [%expect
    {|
      (MemoryAddress
         (Load ((LocalVar "11"), (TInteger 32),
            (FromVariable ((LocalVar "3"), TPointer)), 4))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  store i32 %12, ptr %2, align 4";
  [%expect
    {|
      (MemoryAddress
         (Store ((TInteger 32), (FromVariable ((LocalVar "12"), (TInteger 32))),
            (FromVariable ((LocalVar "2"), TPointer)), 4))) |}]
;;

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
  and iunreachable = word "unreachable" *> return Ast.Unreachable in
  whitespaces *> choice [ iret; ibr; ibr_cond; iswitch; iindirectbr; iunreachable ]
;;

let parse_unary_operation =
  let ifneg =
    let* res_var = parse_instruction_result in
    let* tp, value = whitespaces *> word "fneg" *> whitespaces *> parse_type_with_value in
    return (Ast.Fneg (res_var, tp, value))
  in
  whitespaces *> choice [ ifneg ]
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

let parse_bitwise_binary_operation =
  let help
    (mnem : string)
    (bin_op : Ast.binary_operation_body -> Ast.bitwise_binary_operation)
    =
    parse_instruction_result
    >>= fun var ->
    whitespaces *> word mnem *> whitespaces *> parse_type_with_value2
    >>= function
    | tp, v1, v2 -> return (bin_op (var, tp, v1, v2))
  in
  whitespaces
  *> choice
       [ help "shl" (fun x -> Ast.Shl x)
       ; help "lshr" (fun x -> Ast.Lshr x)
       ; help "ashr" (fun x -> Ast.Ashr x)
       ; help "and" (fun x -> Ast.And x)
       ; help "or" (fun x -> Ast.Or x)
       ; help "xor" (fun x -> Ast.Xor x)
       ]
;;

let parse_vector_instruction =
  let iextractelement =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_val =
      whitespaces *> word "extractelement" *> whitespaces *> parse_type_with_value
      <* comma
    in
    let* int_tp, int_val = parse_type_with_value in
    return (Ast.Extractelement (res_var, vec_tp, vec_val, int_tp, int_val))
  and iinsertelement =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_val =
      whitespaces *> word "insertelement" *> whitespaces *> parse_type_with_value <* comma
    in
    let* elem_val =
      match vec_tp with
      | Ast.TVector (_, elem_tp) -> type_with_value elem_tp <* comma
      | _ -> fail "Parser error: insertelement's first value should be vector type"
    in
    let* int_tp, int_val = parse_type_with_value in
    return (Ast.Insertelement (res_var, vec_tp, vec_val, elem_val, int_tp, int_val))
  and ishufflevector =
    let* res_var = parse_instruction_result in
    let* vec_tp, vec_v1 =
      whitespaces *> word "shufflevector" *> whitespaces *> parse_type_with_value <* comma
    in
    let* vec_v2 = type_with_value vec_tp <* comma in
    let* mask_tp, mask_val = parse_type_with_value in
    match mask_tp, mask_val with
    | Ast.TVector (mask_size, Ast.TInteger 32), Ast.Const mask_const ->
      return (Ast.Shufflevector (res_var, vec_tp, vec_v1, vec_v2, mask_size, mask_const))
    | _ -> fail "Parser error: couldn't parse mask for shufflevector instruction"
  in
  whitespaces *> choice [ iextractelement; iinsertelement; ishufflevector ]
;;

let parse_aggregate_instruction =
  let iextractvalue =
    let* res_var = parse_instruction_result in
    let* agg_tp, agg_val =
      whitespaces *> word "extractvalue" *> whitespaces *> parse_type_with_value <* comma
    in
    let* indexes = sep_by1 comma parse_integer in
    return (Ast.Extractvalue (res_var, agg_tp, agg_val, indexes))
  and iinsertvalue =
    let* res_var = parse_instruction_result in
    let* agg_tp, agg_val =
      whitespaces *> word "insertvalue" *> whitespaces *> parse_type_with_value <* comma
    in
    let* value_tp, value_val = parse_type_with_value <* comma in
    let* indexes = sep_by1 comma parse_integer in
    return (Ast.Insertvalue (res_var, agg_tp, agg_val, value_tp, value_val, indexes))
  in
  whitespaces *> choice [ iextractvalue; iinsertvalue ]
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
    ; (parse_unary_operation >>| fun ins -> Ast.Unary ins)
    ; (parse_binary_operation >>| fun ins -> Ast.Binary ins)
    ; (parse_bitwise_binary_operation >>| fun ins -> Ast.BitwiseBinary ins)
    ; (parse_vector_instruction >>| fun ins -> Ast.Vector ins)
    ; (parse_aggregate_instruction >>| fun ins -> Ast.Aggregate ins)
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
  test_parse parse_instruction Ast.show_instruction {| unreachable |};
  [%expect {|
    (Terminator Unreachable) |}]
;;

(* ##########################################################*)
(* ##################### UNARY ##############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction {| %23 = fneg float %val   |};
  [%expect
    {|
    (Unary
       (Fneg ((LocalVar "23"), TFloat, (FromVariable ((LocalVar "val"), TFloat))
          ))) |}]
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
(* #################### BITWISE #############################*)
(* ########################################################## *)

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %res =  xor i32 0x1A, %var ";
  [%expect
    {|
      (BitwiseBinary
         (Xor
            ((LocalVar "res"), (TInteger 32), (Const (CInteger (32, 26))),
             (FromVariable ((LocalVar "var"), (TInteger 32)))))) |}]
;;

let%expect_test _ =
  test_parse parse_instruction Ast.show_instruction "  %res = ashr i8 -4, -0x23 ";
  [%expect
    {|
      (BitwiseBinary
         (Ashr
            ((LocalVar "res"), (TInteger 8), (Const (CInteger (8, 252))),
             (Const (CInteger (8, 221)))))) |}]
;;

(* ##########################################################*)
(* ##################### VECTOR #############################*)
(* ##########################################################*)

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = extractelement <4 x i32> %vec, i32 0 ";
  [%expect
    {|
      (Vector
         (Extractelement ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "vec"), (TVector (4, (TInteger 32))))),
            (TInteger 32), (Const (CInteger (32, 0)))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = insertelement <4 x i32> %vec, i32 1, i32 0    ; yields <4 x i32> ";
  [%expect
    {|
      (Vector
         (Insertelement ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "vec"), (TVector (4, (TInteger 32))))),
            (Const (CInteger (32, 1))), (TInteger 32), (Const (CInteger (32, 0)))))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = shufflevector <4 x i32> %v1, <4 x i32> %v2,\n\
    \    <8 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 5, i32 6, i32 7 > ";
  [%expect
    {|
      (Vector
         (Shufflevector ((LocalVar "result"), (TVector (4, (TInteger 32))),
            (FromVariable ((LocalVar "v1"), (TVector (4, (TInteger 32))))),
            (FromVariable ((LocalVar "v2"), (TVector (4, (TInteger 32))))), 8,
            (CVector
               [(CInteger (32, 0)); (CInteger (32, 1)); (CInteger (32, 2));
                 (CInteger (32, 3)); (CInteger (32, 4)); (CInteger (32, 5));
                 (CInteger (32, 6)); (CInteger (32, 7))])
            ))) |}]
;;

(* #############################################################*)
(* ##################### Aggregate #############################*)
(* #############################################################*)

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%result = extractvalue {i32, float} %agg, 0    ; yields i32";
  [%expect
    {|
      (Aggregate
         (Extractvalue ((LocalVar "result"), (TStruct [(TInteger 32); TFloat]),
            (FromVariable ((LocalVar "agg"), (TStruct [(TInteger 32); TFloat]))),
            [0]))) |}]
;;

let%expect_test _ =
  test_parse
    parse_instruction
    Ast.show_instruction
    "%agg3 = insertvalue {i32, {float}} %agg1, float %val, 1, 0 ";
  [%expect
    {|
      (Aggregate
         (Insertvalue ((LocalVar "agg3"),
            (TStruct [(TInteger 32); (TStruct [TFloat])]),
            (FromVariable ((LocalVar "agg1"),
               (TStruct [(TInteger 32); (TStruct [TFloat])]))),
            TFloat, (FromVariable ((LocalVar "val"), TFloat)), [1; 0]))) |}]
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

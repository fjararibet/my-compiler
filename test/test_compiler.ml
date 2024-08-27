open Lib.Compiler
open Alcotest

let exp : exp testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_exp e)) ( = )

let ins_list : instruction list testable =
  testable
    (fun oc ins_list -> Format.fprintf oc "%s" (asm_to_string ins_list))
    ( = )

let test_parse_int () = check exp "same exp" (parse (`Atom "5")) (Num 5L)

let test_parse_add1 () =
  check exp "same exp"
    (UnaryOp (Add1, Num 5L))
    (parse (`List [ `Atom "add1"; `Atom "5" ]))

let test_parse_sub1 () =
  check exp "same exp"
    (UnaryOp (Sub1, Num 5L))
    (parse (`List [ `Atom "sub1"; `Atom "5" ]))

let test_parse_double () =
  check exp "same exp"
    (UnaryOp (Double, Num 5L))
    (parse (`List [ `Atom "double"; `Atom "5" ]))

let test_parse_compound () =
  check exp "same exp"
    (UnaryOp (Double, UnaryOp (Add1, UnaryOp (Sub1, UnaryOp (Sub1, Num 5L)))))
    (parse
       (`List
         [
           `Atom "double";
           `List
             [
               `Atom "add1";
               `List [ `Atom "sub1"; `List [ `Atom "sub1"; `Atom "5" ] ];
             ];
         ]))

let test_parse_let () =
  check exp "same exp"
    (Let ("x", Num 5L, Id "x"))
    (parse (`List [ `Atom "let"; `List [ `Atom "x"; `Atom "5" ]; `Atom "x" ]))

let test_parse_nested_let () =
  check exp "same exp"
    (Let
       ( "a",
         Num 10L,
         Let
           ( "c",
             Let
               ( "b",
                 UnaryOp (Add1, Id "a"),
                 Let ("d", UnaryOp (Add1, Id "b"), UnaryOp (Add1, Id "b")) ),
             UnaryOp (Add1, Id "c") ) ))
    (parse
       (`List
         [
           `Atom "let";
           `List [ `Atom "a"; `Atom "10" ];
           `List
             [
               `Atom "let";
               `List
                 [
                   `Atom "c";
                   `List
                     [
                       `Atom "let";
                       `List [ `Atom "b"; `List [ `Atom "add1"; `Atom "a" ] ];
                       `List
                         [
                           `Atom "let";
                           `List
                             [ `Atom "d"; `List [ `Atom "add1"; `Atom "b" ] ];
                           `List [ `Atom "add1"; `Atom "b" ];
                         ];
                     ];
                 ];
               `List [ `Atom "add1"; `Atom "c" ];
             ];
         ]))

let test_compile_int () =
  check ins_list "same instruction list"
    [ IMov (Reg RAX, Const 5L) ]
    (compile (Num 5L) [])

let test_compile_add1 () =
  check ins_list "same instruction list"
    [ IMov (Reg RAX, Const 5L); IInc (Reg RAX) ]
    (compile (UnaryOp (Add1, Num 5L)) [])

let test_compile_sub1 () =
  check ins_list "same instruction list"
    [ IMov (Reg RAX, Const 5L); IDec (Reg RAX) ]
    (compile (UnaryOp (Sub1, Num 5L)) [])

let test_compile_double () =
  check ins_list "same instruction list"
    [ IMov (Reg RAX, Const 5L); IAdd (Reg RAX, Reg RAX) ]
    (compile (UnaryOp (Double, Num 5L)) [])

let test_compile_compound () =
  check ins_list "same instruction list"
    [
      IMov (Reg RAX, Const 5L);
      IDec (Reg RAX);
      IDec (Reg RAX);
      IInc (Reg RAX);
      IAdd (Reg RAX, Reg RAX);
    ]
    (compile
       (UnaryOp (Double, UnaryOp (Add1, UnaryOp (Sub1, UnaryOp (Sub1, Num 5L)))))
       [])

let test_compile_let () =
  check ins_list "same instruction list"
    [
      IMov (Reg RAX, Const 5L);
      IMov (RegOffset (RSP, 1), Reg RAX);
      IMov (Reg RAX, RegOffset (RSP, 1));
    ]
    (compile (Let ("x", Num 5L, Id "x")) [])

let test_compile_nested_let () =
  check ins_list "same instruction list"
    [
      IMov (Reg RAX, Const 10L);
      IMov (RegOffset (RSP, 1), Reg RAX);
      IMov (Reg RAX, RegOffset (RSP, 1));
      IInc (Reg RAX);
      IMov (RegOffset (RSP, 2), Reg RAX);
      IMov (Reg RAX, RegOffset (RSP, 2));
      IInc (Reg RAX);
      IMov (RegOffset (RSP, 3), Reg RAX);
      IMov (Reg RAX, RegOffset (RSP, 2));
      IInc (Reg RAX);
      IMov (RegOffset (RSP, 2), Reg RAX);
      IMov (Reg RAX, RegOffset (RSP, 2));
      IInc (Reg RAX);
    ]
    (compile
       (Let
          ( "a",
            Num 10L,
            Let
              ( "c",
                Let
                  ( "b",
                    UnaryOp (Add1, Id "a"),
                    Let ("d", UnaryOp (Add1, Id "b"), UnaryOp (Add1, Id "b")) ),
                UnaryOp (Add1, Id "c") ) ))
       [])

let test_asm_to_string_int () =
  check string "same asm string" "mov RAX, 5\n"
    (asm_to_string [ IMov (Reg RAX, Const 5L) ])

let test_asm_to_string_add1 () =
  check string "same asm string" "mov RAX, 5\ninc RAX\n"
    (asm_to_string [ IMov (Reg RAX, Const 5L); IInc (Reg RAX) ])

let test_asm_to_string_sub1 () =
  check string "same asm string" "mov RAX, 5\ndec RAX\n"
    (asm_to_string [ IMov (Reg RAX, Const 5L); IDec (Reg RAX) ])

let test_asm_to_string_double () =
  check string "same asm string" "mov RAX, 5\nadd RAX, RAX\n"
    (asm_to_string [ IMov (Reg RAX, Const 5L); IAdd (Reg RAX, Reg RAX) ])

let test_asm_to_string_compound () =
  check string "same asm string"
    "mov RAX, 5\ndec RAX\ndec RAX\ninc RAX\nadd RAX, RAX\n"
    (asm_to_string
       [
         IMov (Reg RAX, Const 5L);
         IDec (Reg RAX);
         IDec (Reg RAX);
         IInc (Reg RAX);
         IAdd (Reg RAX, Reg RAX);
       ])

let test_asm_to_string_let () =
  check string "same asm string"
    "mov RAX, 5\nmov [RSP - 8*1], RAX\nmov RAX, [RSP - 8*1]\n"
    (asm_to_string
       [
         IMov (Reg RAX, Const 5L);
         IMov (RegOffset (RSP, 1), Reg RAX);
         IMov (Reg RAX, RegOffset (RSP, 1));
       ])

let test_asm_to_string_nested_let () =
  check string "same asm string"
    "mov RAX, 10\n\
     mov [RSP - 8*1], RAX\n\
     mov RAX, [RSP - 8*1]\n\
     inc RAX\n\
     mov [RSP - 8*2], RAX\n\
     mov RAX, [RSP - 8*2]\n\
     inc RAX\n\
     mov [RSP - 8*3], RAX\n\
     mov RAX, [RSP - 8*2]\n\
     inc RAX\n\
     mov [RSP - 8*2], RAX\n\
     mov RAX, [RSP - 8*2]\n\
     inc RAX\n"
    (asm_to_string
       [
         IMov (Reg RAX, Const 10L);
         IMov (RegOffset (RSP, 1), Reg RAX);
         IMov (Reg RAX, RegOffset (RSP, 1));
         IInc (Reg RAX);
         IMov (RegOffset (RSP, 2), Reg RAX);
         IMov (Reg RAX, RegOffset (RSP, 2));
         IInc (Reg RAX);
         IMov (RegOffset (RSP, 3), Reg RAX);
         IMov (Reg RAX, RegOffset (RSP, 2));
         IInc (Reg RAX);
         IMov (RegOffset (RSP, 2), Reg RAX);
         IMov (Reg RAX, RegOffset (RSP, 2));
         IInc (Reg RAX);
       ])

let () =
  run "A compiler"
    [
      ( "parse",
        [
          test_case "A number" `Quick test_parse_int;
          test_case "An add1" `Quick test_parse_add1;
          test_case "A sub1" `Quick test_parse_sub1;
          test_case "A double" `Quick test_parse_double;
          test_case "A compound expression" `Quick test_parse_compound;
          test_case "A let binding" `Quick test_parse_let;
          test_case "A nested let binding" `Quick test_parse_nested_let;
        ] );
      ( "compile",
        [
          test_case "A number" `Quick test_compile_int;
          test_case "An add1" `Quick test_compile_add1;
          test_case "A sub1" `Quick test_compile_sub1;
          test_case "A double" `Quick test_compile_double;
          test_case "A compound expression" `Quick test_compile_compound;
          test_case "A let binding" `Quick test_compile_let;
          test_case "A nested let binding" `Quick test_compile_nested_let;
        ] );
      ( "asm to string",
        [
          test_case "A number" `Quick test_asm_to_string_int;
          test_case "An add1" `Quick test_asm_to_string_add1;
          test_case "A sub1" `Quick test_asm_to_string_sub1;
          test_case "A double" `Quick test_asm_to_string_double;
          test_case "A compound asm" `Quick test_asm_to_string_compound;
          test_case "A let binding" `Quick test_asm_to_string_let;
          test_case "A nested let binding" `Quick test_asm_to_string_nested_let;
        ] );
    ]

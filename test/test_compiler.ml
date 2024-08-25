open Compiler
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
    (parse (`List [ `Atom "add1"; `Atom "5" ]))
    (Add1 (Num 5L))

let test_parse_sub1 () =
  check exp "same exp"
    (parse (`List [ `Atom "sub1"; `Atom "5" ]))
    (Sub1 (Num 5L))

let test_parse_compound () =
  check exp "same exp"
    (parse
       (`List
         [
           `Atom "add1";
           `List [ `Atom "sub1"; `List [ `Atom "sub1"; `Atom "5" ] ];
         ]))
    (Add1 (Sub1 (Sub1 (Num 5L))))

let test_compile_exp_int () =
  check ins_list "same instruction list" (compile_exp (Num 5L))
    [ IMov (Reg RAX, Const 5L) ]

let test_compile_exp_add1 () =
  check ins_list "same instruction list"
    (compile_exp (Add1 (Num 5L)))
    [ IMov (Reg RAX, Const 5L); IAdd (Reg RAX, Const 1L) ]

let test_compile_exp_sub1 () =
  check ins_list "same instruction list"
    (compile_exp (Sub1 (Num 5L)))
    [ IMov (Reg RAX, Const 5L); IAdd (Reg RAX, Const (-1L)) ]

let () =
  run "A compiler"
    [
      ( "parse",
        [
          test_case "A number" `Quick test_parse_int;
          test_case "An add1" `Quick test_parse_add1;
          test_case "A sub1" `Quick test_parse_sub1;
          test_case "A compund expression" `Quick test_parse_compound;
        ] );
      ( "compile_exp",
        [
          test_case "A number" `Quick test_compile_exp_int;
          test_case "An add1" `Quick test_compile_exp_add1;
          test_case "An sub1" `Quick test_compile_exp_sub1;
        ] );
    ]

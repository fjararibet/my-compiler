open Compiler
open Alcotest

let exp : exp testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_exp e)) ( = )

let test_parse_int () = check exp "same exp" (parse (`Atom "5")) (Num 5L)

let () =
  run "A compiler" [ ("parse", [ test_case "A number" `Quick test_parse_int ]) ]

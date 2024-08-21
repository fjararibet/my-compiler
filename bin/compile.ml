open Compiler
let () =
  let sexp = sexp_from_file Sys.argv.(1) in
  let input_program = parse sexp in
  let program = compile input_program in
  Printf.printf "%s\n" program

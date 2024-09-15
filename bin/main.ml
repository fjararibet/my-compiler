open Lib.Parser
open Lib.Compiler

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (Format.sprintf "Unable to parse file %s: %s" filename msg)

let () =
  let sexp = sexp_from_file Sys.argv.(1) in
  let input_program = parse sexp in
  let program = compile_prog input_program in
  Printf.printf "%s\n" program

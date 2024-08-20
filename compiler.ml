open Printf

type expr = int64

type reg = 
  | RAX

type arg = 
  | Const of int64
  | Reg of reg

type instruction = 
  | IMov of arg * arg 

let asm_to_string (asm : instruction list) : string = 
  match asm with
  | (IMov (a1, a2)) :: rest -> sprintf "mov %s, %s" a1 a2

let compile_exp (e : expr) : instruction list = 
  [ IMov(Reg(RAX)), Const(e) ]

(* A very sophisticated compiler - insert the given integer into the mov
instruction at the correct place *)
let compile (program : int64) : string =
  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  mov RAX, %Ld
  ret\n" program

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let input_program = Int64.of_string (input_line input_file) in
  close_in input_file;
  let program = (compile input_program) in
  printf "%s\n" program;;

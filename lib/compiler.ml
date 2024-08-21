open CCSexp
open Printf

type exp = Num of int64 | Add1 of exp | Sub1 of exp
type reg = RAX
type arg = Const of int64 | Reg of reg
type instruction = IAdd of arg * arg | IMov of arg * arg | IRet

let rec string_of_exp (exp : exp) : string =
  match exp with
  | Num n -> Int64.to_string n
  | Add1 e -> sprintf "(add1 %s)" (string_of_exp e)
  | Sub1 e -> sprintf "(sub1 %s)" (string_of_exp e)

let string_of_reg (reg : reg) : string = match reg with RAX -> "RAX"

let string_of_arg (arg : arg) : string =
  match arg with Const n -> Int64.to_string n | Reg r -> string_of_reg r

let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | IMov (a1, a2) :: rest ->
      sprintf "mov %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IAdd (a1, a2) :: rest ->
      sprintf "add %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IRet :: rest -> "ret" ^ asm_to_string rest

let rec compile_exp (e : exp) : instruction list =
  match e with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Add1 e -> compile_exp e @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 e -> compile_exp e @ [ IAdd (Reg RAX, Const (-1L)) ]

(* A very sophisticated compiler - insert the given integer into the mov
   instruction at the correct place *)
let compile (e : exp) : string =
  let instructions = compile_exp e in
  let asm_string = asm_to_string (instructions @ [ IRet ]) in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  prelude ^ "\n" ^ asm_string

let rec parse (sexp : sexp) : exp =
  match sexp with
  | `Atom a -> Num (Int64.of_string a)
  | `List [ `Atom "add1"; exp ] -> Add1 (parse exp)
  | `List [ `Atom "sub1"; exp ] -> Sub1 (parse exp)
  | _ -> failwith "Not a valid exp"

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

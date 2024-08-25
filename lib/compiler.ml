open CCSexp
open Printf

type exp =
  | Num of int64
  | Add1 of exp
  | Sub1 of exp
  | Id of string
  | Let of string * exp * exp

type reg = RAX | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of
      reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction = IAdd of arg * arg | IMov of arg * arg | IRet
type env = (string * int) list

let rec lookup (name : string) (env : env) : int =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in the environment" name)
  | (x, i) :: rest -> if x = name then i else lookup name rest

let add (name : string) (env : env) : env * int =
  let slot = 1 + List.length env in
  ((name, slot) :: env, slot)

let rec string_of_exp (exp : exp) : string =
  match exp with
  | Num n -> Int64.to_string n
  | Id x -> x
  | Add1 e -> sprintf "(add1 %s)" (string_of_exp e)
  | Sub1 e -> sprintf "(sub1 %s)" (string_of_exp e)
  | Let (x, value, body) ->
      sprintf "(let (%s %s) %s)" x (string_of_exp value) (string_of_exp body)

let string_of_reg (reg : reg) : string =
  match reg with RAX -> "RAX" | RSP -> "RSP"

let string_of_arg (arg : arg) : string =
  match arg with
  | Const n -> Int64.to_string n
  | Reg r -> string_of_reg r
  | RegOffset (reg, offset) ->
      sprintf "[%s - 8*%s]" (string_of_reg reg) (string_of_int offset)

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

let rec compile_exp (exp : exp) (env : env) : instruction list =
  match exp with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Id x ->
      let slot = lookup x env in
      [ IMov (Reg RAX, RegOffset (RSP, slot)) ]
  | Add1 e -> compile_exp e env @ [ IAdd (Reg RAX, Const 1L) ]
  | Sub1 e -> compile_exp e env @ [ IAdd (Reg RAX, Const (-1L)) ]
  | Let (x, value, body) ->
      let env', slot = add x env in
      compile_exp value env
      @ [ IMov (RegOffset (RSP, slot), Reg RAX) ]
      @ compile_exp body env'

let compile (e : exp) : string =
  let instructions = compile_exp e [] in
  let asm_string = asm_to_string (instructions @ [ IRet ]) in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  prelude ^ "\n" ^ asm_string

let rec parse (sexp : sexp) : exp =
  match sexp with
  | `Atom a -> (
      match Int64.of_string_opt a with Some n -> Num n | None -> Id a)
  | `List [ `Atom "add1"; exp ] -> Add1 (parse exp)
  | `List [ `Atom "sub1"; exp ] -> Sub1 (parse exp)
  | `List [ `Atom "let"; `List [ `Atom x; value ]; body ] ->
      Let (x, parse value, parse body)
  | _ -> failwith "Not a valid exp"

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

open CCSexp
open Printf

type unary_op = Add1 | Sub1 | Double
type binary_op = Plus | Minus | Times

type exp =
  | Num of int64
  | UnaryOp of unary_op * exp
  | BinaryOp of binary_op * exp * exp
  | Id of string
  | Let of string * exp * exp
  | If of exp * exp * exp

type reg = RAX | RBX | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of
      reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

type instruction =
  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg
  | IMov of arg * arg
  | IInc of arg
  | IDec of arg
  | ILabel of string
  | IJmp of string
  | IJe of string
  | ICmp of arg * arg
  | IRet

type env = (string * int) list

let rec lookup (name : string) (env : env) : int =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in the environment" name)
  | (x, i) :: rest -> if x = name then i else lookup name rest

let add (name : string) (env : env) : env * int =
  let slot = 1 + List.length env in
  ((name, slot) :: env, slot)

let string_of_unary_op (op : unary_op) : string =
  match op with Add1 -> "add1" | Sub1 -> "sub1" | Double -> "double"

let string_of_binary_op (op : binary_op) : string =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*"

let rec string_of_exp (exp : exp) : string =
  match exp with
  | Num n -> Int64.to_string n
  | Id x -> x
  | UnaryOp (op, e) ->
      sprintf "(%s %s)" (string_of_unary_op op) (string_of_exp e)
  | BinaryOp (op, e1, e2) ->
      sprintf "(%s %s %s)" (string_of_binary_op op) (string_of_exp e1)
        (string_of_exp e2)
  | Let (x, value, body) ->
      sprintf "(let (%s %s) %s)" x (string_of_exp value) (string_of_exp body)
  | If (cond, true_path, false_path) -> sprintf "(if %s %s %s)" (string_of_exp cond) (string_of_exp true_path) (string_of_exp false_path)

let string_of_reg (reg : reg) : string =
  match reg with RAX -> "RAX" | RBX -> "RBX" | RSP -> "RSP"

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
  | ISub (a1, a2) :: rest ->
      sprintf "sub %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IMul a :: rest -> sprintf "mul %s\n" (string_of_arg a) ^ asm_to_string rest
  | IInc a :: rest -> sprintf "inc %s\n" (string_of_arg a) ^ asm_to_string rest
  | IDec a :: rest -> sprintf "dec %s\n" (string_of_arg a) ^ asm_to_string rest
  | ILabel label :: rest  -> sprintf "%s:\n" label ^ asm_to_string rest
  | ICmp (a1, a2) :: rest -> sprintf "cmp %s, %s\n" (string_of_arg a1) (string_of_arg a2) ^ asm_to_string rest
  | IJmp label :: rest -> sprintf "jmp %s\n" label ^ asm_to_string rest
  | IJe label :: rest -> sprintf "je %s\n" label ^ asm_to_string rest
  | IRet :: rest -> "ret" ^ asm_to_string rest

let gensym =
  let counter = ref 0 in
  fun (prefix : string) ->
    counter := !counter + 1;
    sprintf "%s%d" prefix !counter

let compile_unary (op : unary_op) =
  match op with
  | Add1 -> [ IInc (Reg RAX) ]
  | Sub1 -> [ IDec (Reg RAX) ]
  | Double -> [ IAdd (Reg RAX, Reg RAX) ]

let compile_binary (op : binary_op) (slot : int) =
  match op with
  | Plus -> [ IAdd (Reg RAX, RegOffset (RSP, slot)) ]
  | Minus -> [ ISub (Reg RAX, RegOffset (RSP, slot)) ]
  | Times -> [ IMov (Reg RBX, RegOffset (RSP, slot)) ] @ [ IMul (Reg RBX) ]

let rec compile (exp : exp) (env : env) : instruction list =
  match exp with
  | Num n -> [ IMov (Reg RAX, Const n) ]
  | Id x ->
      let slot = lookup x env in
      [ IMov (Reg RAX, RegOffset (RSP, slot)) ]
  | UnaryOp (op, e) -> compile e env @ compile_unary op
  | BinaryOp (op, e1, e2) ->
      let env', slot1 = add (gensym "#x#") env in
      let _, slot2 = add (gensym "#x#") env' in
      compile e1 env
      @ [ IMov (RegOffset (RSP, slot1), Reg RAX) ]
      @ compile e2 env'
      @ [ IMov (RegOffset (RSP, slot2), Reg RAX) ]
      @ [ IMov (Reg RAX, RegOffset (RSP, slot1)) ]
      @ compile_binary op slot2
  | Let (x, value, body) ->
      let env', slot = add x env in
      compile value env
      @ [ IMov (RegOffset (RSP, slot), Reg RAX) ]
      @ compile body env'
  | If (cond, true_path, false_path) ->
      let else_label = gensym "if_false" in
      let done_label = gensym "done" in
      compile cond env
      @ [ ICmp (Reg RAX, Const 0L) ]
      @ [ IJe else_label ]
      @ compile true_path env
      @ [ IJmp done_label]
      @ [ ILabel else_label ]
      @ compile false_path env
      @ [ ILabel done_label ]

let compile_prog (e : exp) : string =
  let instructions = compile e [] in
  let asm_string = asm_to_string (instructions @ [ IRet ]) in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  prelude ^ "\n" ^ asm_string

let rec parse (sexp : sexp) : exp =
  match sexp with
  | `Atom a -> (
      match Int64.of_string_opt a with Some n -> Num n | None -> Id a)
  | `List [ `Atom "add1"; exp ] -> UnaryOp (Add1, parse exp)
  | `List [ `Atom "sub1"; exp ] -> UnaryOp (Sub1, parse exp)
  | `List [ `Atom "double"; exp ] -> UnaryOp (Double, parse exp)
  | `List [ `Atom "+"; exp1; exp2 ] -> BinaryOp (Plus, parse exp1, parse exp2)
  | `List [ `Atom "-"; exp1; exp2 ] -> BinaryOp (Minus, parse exp1, parse exp2)
  | `List [ `Atom "*"; exp1; exp2 ] -> BinaryOp (Times, parse exp1, parse exp2)
  | `List [ `Atom "let"; `List [ `Atom x; value ]; body ] ->
      Let (x, parse value, parse body)
  | `List [ `Atom "if"; cond; true_path; false_path ] ->
      If (parse cond, parse true_path, parse false_path)
  | _ -> failwith "Not a valid exp"

let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

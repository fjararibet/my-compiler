open Ast
open Asm
open Printf

let gensym =
  let counter = ref 0 in
  fun (prefix : string) ->
    counter := !counter + 1;
    sprintf "%s%d" prefix !counter

let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let const_true = Int64.of_string "0x8000000000000001"
let const_false = Int64.of_string "0x0000000000000001"

let compile_unary (op : unary_op) =
  match op with
  | Add1 -> [ IAdd (Reg RAX, Const 2L) ]
  | Sub1 -> [ ISub (Reg RAX, Const 2L) ]
  | Not ->
      [
        IMov (Reg RBX, Const (Int64.of_string "0x8000000000000000"));
        IXor (Reg RAX, Reg RBX);
      ]
  | Double -> [ IAdd (Reg RAX, Reg RAX) ]

let compile_binary (op : binary_op) (slot : int) =
  match op with
  | Plus -> [ IAdd (Reg RAX, RegOffset (RSP, slot)) ]
  | Minus -> [ ISub (Reg RAX, RegOffset (RSP, slot)) ]
  | Times ->
      [ IMov (Reg RBX, RegOffset (RSP, slot)) ]
      @ [ IMul (Reg RBX) ]
      @ [ ISar (Reg RAX, Const 1L) ]
  | Lt ->
      let less_label = gensym "less" in
      [ IMov (Reg RBX, RegOffset (RSP, slot)) ]
      @ [ ICmp (Reg RBX, Reg RAX) ]
      @ [ IMov (Reg RAX, Const const_true) ]
      @ [ IJl less_label ]
      @ [ IMov (Reg RAX, Const const_false) ]
      @ [ ILabel less_label ]

let rec compile (exp : exp) (env : env) : instruction list =
  match exp with
  | Num n ->
      if n > max_int || n < min_int then
        failwith ("Integer Overflow: " ^ Int64.to_string n)
      else [ IMov (Reg RAX, Const (Int64.shift_left n 1)) ]
  | Bool true -> [ IMov (Reg RAX, Const const_true) ]
  | Bool false -> [ IMov (Reg RAX, Const const_false) ]
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
      @ compile_binary op slot1
  | Let (x, value, body) ->
      let env', slot = add x env in
      compile value env
      @ [ IMov (RegOffset (RSP, slot), Reg RAX) ]
      @ compile body env'
  | If (cond, true_path, false_path) ->
      let else_label = gensym "if_false" in
      let done_label = gensym "done" in
      compile cond env
      @ [ IMov (Reg RBX, Const const_false) ]
      @ [ ICmp (Reg RAX, Reg RBX) ]
      @ [ IJe else_label ] @ compile true_path env @ [ IJmp done_label ]
      @ [ ILabel else_label ] @ compile false_path env @ [ ILabel done_label ]

let compile_prog (e : exp) : string =
  let instructions = compile e [] in
  let asm_string = asm_to_string (instructions @ [ IRet ]) in
  let prelude =
    "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:"
  in
  prelude ^ "\n" ^ asm_string

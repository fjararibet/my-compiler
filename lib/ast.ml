open Printf

type unary_op = Add1 | Sub1 | Double | Not
type binary_op = Plus | Minus | Times

type exp =
  | Num of int64
  | Bool of bool
  | UnaryOp of unary_op * exp
  | BinaryOp of binary_op * exp * exp
  | Id of string
  | Let of string * exp * exp
  | If of exp * exp * exp

type env = (string * int) list

let rec lookup (name : string) (env : env) : int =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in the environment" name)
  | (x, i) :: rest -> if x = name then i else lookup name rest

let add (name : string) (env : env) : env * int =
  let slot = 1 + List.length env in
  ((name, slot) :: env, slot)

let string_of_unary_op (op : unary_op) : string =
  match op with Add1 -> "add1" | Sub1 -> "sub1" | Double -> "double" | Not -> "not"

let string_of_binary_op (op : binary_op) : string =
  match op with Plus -> "+" | Minus -> "-" | Times -> "*"

let rec string_of_exp (exp : exp) : string =
  match exp with
  | Num n -> Int64.to_string n
  | Bool b -> string_of_bool b
  | Id x -> x
  | UnaryOp (op, e) ->
      sprintf "(%s %s)" (string_of_unary_op op) (string_of_exp e)
  | BinaryOp (op, e1, e2) ->
      sprintf "(%s %s %s)" (string_of_binary_op op) (string_of_exp e1)
        (string_of_exp e2)
  | Let (x, value, body) ->
      sprintf "(let (%s %s) %s)" x (string_of_exp value) (string_of_exp body)
  | If (cond, true_path, false_path) ->
      sprintf "(if %s %s %s)" (string_of_exp cond) (string_of_exp true_path)
        (string_of_exp false_path)

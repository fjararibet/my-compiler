open Ast
open CCSexp

let rec parse (sexp : sexp) : exp =
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom a -> (
      match Int64.of_string_opt a with Some n -> Num n | None -> Id a)
  | `List [ `Atom "add1"; exp ] -> UnaryOp (Add1, parse exp)
  | `List [ `Atom "sub1"; exp ] -> UnaryOp (Sub1, parse exp)
  | `List [ `Atom "double"; exp ] -> UnaryOp (Double, parse exp)
  | `List [ `Atom "+"; exp1; exp2 ] -> BinaryOp (Plus, parse exp1, parse exp2)
  | `List [ `Atom "-"; exp1; exp2 ] -> BinaryOp (Minus, parse exp1, parse exp2)
  | `List [ `Atom "*"; exp1; exp2 ] -> BinaryOp (Times, parse exp1, parse exp2)
  | `List [ `Atom "and"; exp1; exp2 ] -> BinaryOp (And, parse exp1, parse exp2)
  | `List [ `Atom "or"; exp1; exp2 ] -> BinaryOp (Or, parse exp1, parse exp2)
  | `List [ `Atom "not"; exp ] -> UnaryOp (Not, parse exp)
  | `List [ `Atom "let"; `List [ `Atom x; value ]; body ] ->
      Let (x, parse value, parse body)
  | `List [ `Atom "if"; cond; true_path; false_path ] ->
      If (parse cond, parse true_path, parse false_path)
  | _ -> failwith "Not a valid exp"

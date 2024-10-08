open Ast
open CCSexp
open Printf

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
  | `List [ `Atom "and"; exp1; exp2 ] -> If (parse exp1, parse exp2, Bool false)
  | `List [ `Atom "or"; exp1; exp2 ] -> If (parse exp1, Bool true, parse exp2)
  | `List [ `Atom "<"; exp1; exp2 ] -> BinaryOp (Lt ,parse exp1, parse exp2)
  | `List [ `Atom "not"; exp ] -> UnaryOp (Not, parse exp)
  | `List [ `Atom "let"; `List [ `Atom x; value ]; body ] ->
      Let (x, parse value, parse body)
  | `List [ `Atom "if"; cond; true_path; false_path ] ->
      If (parse cond, parse true_path, parse false_path)
  | _ -> failwith "Not a valid exp"

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)

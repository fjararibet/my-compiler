open CCSexp
open Printf

let rec ccsexp_to_string (sexp : sexp) : string =
  match sexp with
  | `Atom a -> sprintf "`Atom \"%s\"" a
  | `List l ->
      let elements = List.map ccsexp_to_string l in
      sprintf "`List [%s]" (String.concat "; " elements)

let string_to_ccsexp_string s =
  let sexp =
    match parse_string s with
    | Ok s -> s
    | Error _ -> failwith "Not a valid sexp"
  in
  print_endline @@ ccsexp_to_string sexp

open Printf

type reg = RAX | RBX | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of
      reg * int (* RegOffset(reg, i) represents address [reg - 8*i] *)
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
  | ISar of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | IRet

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
      sprintf "  mov %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IAdd (a1, a2) :: rest ->
      sprintf "  add %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | ISub (a1, a2) :: rest ->
      sprintf "  sub %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IMul a :: rest -> sprintf "  mul %s\n" (string_of_arg a) ^ asm_to_string rest
  | IInc a :: rest -> sprintf "  inc %s\n" (string_of_arg a) ^ asm_to_string rest
  | IDec a :: rest -> sprintf "  dec %s\n" (string_of_arg a) ^ asm_to_string rest
  | ILabel label :: rest -> sprintf "%s:\n" label ^ asm_to_string rest
  | ICmp (a1, a2) :: rest ->
      sprintf "  cmp %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IJmp label :: rest -> sprintf "  jmp %s\n" label ^ asm_to_string rest
  | IJe label :: rest -> sprintf "  je %s\n" label ^ asm_to_string rest
  | ISar (a1, a2) :: rest ->
      sprintf "  sar %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IAnd (a1, a2) :: rest ->
      sprintf "  and %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IOr (a1, a2) :: rest ->
      sprintf "  or %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IXor (a1, a2) :: rest ->
      sprintf "  xor %s, %s\n" (string_of_arg a1) (string_of_arg a2)
      ^ asm_to_string rest
  | IRet :: rest -> "  ret" ^ asm_to_string rest


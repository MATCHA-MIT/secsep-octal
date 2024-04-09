(* ISA interface *)

module Isa = struct

  type label = string

  type register =
    |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15
    |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
    |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
    | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B
    (* | R0 *)

  let get_reg_idx (r: register) = 
    match r with
    | RAX | EAX | AX | AH | AL -> 0
    | RCX | ECX | CX | CH | CL -> 1
    | RDX | EDX | DX | DH | DL -> 2
    | RBX | EBX | BX | BH | BL -> 3
    | RSP | ESP | SP | SPL -> 4
    | RBP | EBP | BP | BPL -> 5
    | RSI | ESI | SI | SIL -> 6
    | RDI | EDI | DI | DIL -> 7
    | R8 | R8D | R8W | R8B -> 8
    | R9 | R9D | R9W | R9B -> 9
    | R10 | R10D | R10W | R10B -> 10
    | R11 | R11D | R11W | R11B -> 11
    | R12 | R12D | R12W | R12B -> 12
    | R13 | R13D | R13W | R13B -> 13
    | R14 | R14D | R14W | R14B -> 14
    | R15 | R15D | R15W | R15B -> 15
    (* | R0 -> 16 *)

  let total_reg_num : int = 16

  type immediate =
    | ImmNum of int
    | ImmLabel of int

  type scale = Scale1 | Scale2 | Scale4 | Scale8

  let scale_val (s: scale) : int =
    match s with
    | Scale1 -> 1
    | Scale2 -> 2
    | Scale4 -> 4
    | Scale8 -> 8

(*
  let scale_val = function
    | Scale1 -> 1
    | Scale2 -> 2
    | Scale4 -> 4
    | Scale8 -> 8;;
  val scale_val : scale -> int = <fun> *)

  type operand =
    | ImmOp of immediate
    | RegOp of register
    | MemOp of immediate * register * register * scale (* disp, base, index, scale *)
    | LdOp of immediate * register * register * scale
    | StOp of immediate * register * register * scale
    | LabelOp of label

  type branch_cond =
    | JNe | JE | JL | JLe | JG | JGe

  type instruction =
    | Mov of operand * operand
    | MovS of operand * operand
    | MovZ of operand * operand
    | Lea of operand * operand
    | Add of operand * operand * operand
    | Sub of operand * operand * operand
    | Sal of operand * operand * operand
    | Sar of operand * operand * operand
    | Xor of operand * operand * operand
    | Not of operand * operand
    | And of operand * operand * operand
    | Or of operand * operand * operand
    | Cmp of operand * operand
    | Jmp of label
    | Jcond of label * branch_cond
    | Ret
    | Nop

  type basic_block = {
    label: label;
    insts: instruction list;
  }

  type program = basic_block list

end

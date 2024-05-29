(* ISA interface *)

module Isa = struct
  (* TODO: Other exception on isa side *)
  exception IsaError of string

  let isa_error msg = raise (IsaError ("[Isa Error] " ^ msg))

  type label = string

  let ret_label = ".Ret"

  type imm_var_id = int

  module StrM = Map.Make(String)
  type imm_var_map = imm_var_id StrM.t

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

  let rsp_idx = get_reg_idx RSP

  let total_reg_num : int = 16

  let get_reg_size (r: register) : int64 =
    match r with
    |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15  -> 8L
    |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D -> 4L
    |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W -> 2L
    | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B -> 1L

  type immediate =
    | ImmNum of int64
    | ImmLabel of imm_var_id
    | ImmBExp of immediate * immediate

  let rec string_of_immediate (i: immediate) : string =
    match i with
    | ImmNum x -> Int64.to_string x
    | ImmLabel x -> "var " ^ (string_of_int x)
    | ImmBExp (i1, i2) -> "(" ^ (string_of_immediate i1) ^ ") + (" ^ (string_of_immediate i2) ^ ")"

  type scale = Scale1 | Scale2 | Scale4 | Scale8

  let scale_val (s: scale) : int64 =
    match s with
    | Scale1 -> 1L
    | Scale2 -> 2L
    | Scale4 -> 4L
    | Scale8 -> 8L

  type operand =
    | ImmOp of immediate
    | RegOp of register
    | MemOp of immediate option * register option * register option * scale option (* disp, base, index, scale *)
    | LdOp of immediate option * register option * register option * scale option * int64
    | StOp of immediate option * register option * register option * scale option * int64
    | LabelOp of label

  let get_reg_op_size (op_list: operand list) : int64 option =
    let helper (acc: int64 option) (op: operand) : int64 option =
      match op, acc with
      | RegOp _, Some size -> Some size (* Note: this result should not be used when operand size does not match!!! *)
        (* if get_reg_size r = size then acc else isa_error "reg size does not match" *)
      | RegOp r, None -> Some (get_reg_size r)
      | _, _ -> acc
    in
    List.fold_left helper None op_list

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
    | Shr of operand * operand * operand
    | Xor of operand * operand * operand
    | Not of operand * operand
    | And of operand * operand * operand
    | Or of operand * operand * operand
    | Cmp of operand * operand
    | Jmp of label
    | Jcond of label * branch_cond
    | Call of label
    | Push of operand
    | Pop of operand
    | Ret
    | Nop

  type basic_block = {
    label: label;
    func: label;
    rsp_offset: int64;
    insts: instruction list;
  }

  type program = {
    bbs: basic_block list;
    imm_var_map: imm_var_map;
  }

  let is_label_function_entry (l: label) = l.[0] <> '.'

  let inst_referring_label (m: string) : bool =
    match m with
    | "call"
    | "jmp" | "je" | "jne" | "jl" | "jle" | "jg" | "jge"
    | "jbe" | "jb" -> true
    | _ -> false

  let inst_is_uncond_jump (inst: instruction) : bool =
    match inst with
    | Jmp _ | Ret -> true
    | _ -> false

  let mnemonic_of_instruction (inst: instruction) : string =
    match inst with
    | Mov _ -> "mov"
    | MovS _ -> "movs"
    | MovZ _ -> "movz"
    | Lea _ -> "lea"
    | Add _ -> "add"
    | Sub _ -> "sub"
    | Sal _ -> "sal"
    | Sar _ -> "sar"
    | Shr _ -> "shr"
    | Xor _ -> "xor"
    | Not _ -> "not"
    | And _ -> "and"
    | Or _ -> "or"
    | Cmp _ -> "cmp"
    | Jmp _ -> "jmp"
    | Jcond (_, cond) ->
      begin
        match cond with
        | JNe -> "jne"
        | JE -> "je"
        | JL -> "jl"
        | JLe -> "jle"
        | JG -> "jg"
        | JGe -> "jge"
      end
    | Call _ -> "call"
    | Push _ -> "push"
    | Pop _ -> "pop"
    | Ret -> "ret"
    | Nop -> "nop"

  let get_op_list (inst: instruction) : operand list =
    match inst with
    | Mov (op0, op1) | MovS (op0, op1) | MovZ (op0, op1) 
    | Lea (op0, op1) | Not (op0, op1) | Cmp (op0, op1) -> [op0; op1]
    | Add (op0, op1, op2) | Sub (op0, op1, op2) | Sal (op0, op1, op2)
    | Sar (op0, op1, op2) | Shr (op0, op1, op2) | Xor (op0, op1, op2)
    | And (op0, op1, op2) | Or (op0, op1, op2) -> [op0; op1; op2]
    | Push op | Pop op -> [op]
    | _ -> []

end

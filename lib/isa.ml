(* ISA interface *)
open Pretty_print

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

  let fix_reg_size (r: register) (s: int64) : register =
    if s = 1L then
      match r with
      | RAX | EAX | AX | AL -> AL
      | RCX | ECX | CX | CL -> CL
      | RDX | EDX | DX | DL -> DL
      | RBX | EBX | BX | BL -> BL
      | RSP | ESP | SP | SPL -> SPL
      | RBP | EBP | BP | BPL -> BPL
      | RSI | ESI | SI | SIL -> SIL
      | RDI | EDI | DI | DIL -> DIL
      | R8 | R8D | R8W | R8B -> R8B
      | R9 | R9D | R9W | R9B -> R9B
      | R10 | R10D | R10W | R10B -> R10B
      | R11 | R11D | R11W | R11B -> R11B
      | R12 | R12D | R12W | R12B -> R12B
      | R13 | R13D | R13W | R13B -> R13B
      | R14 | R14D | R14W | R14B -> R14B
      | R15 | R15D | R15W | R15B -> R15B
      | _ -> isa_error "should not use high 1-byte reg"
    else if s = 2L then
      match r with
      | RAX | EAX | AX | AL -> AX
      | RCX | ECX | CX | CL -> CX
      | RDX | EDX | DX | DL -> DX
      | RBX | EBX | BX | BL -> BX
      | RSP | ESP | SP | SPL -> SP
      | RBP | EBP | BP | BPL -> BP
      | RSI | ESI | SI | SIL -> SI
      | RDI | EDI | DI | DIL -> DI
      | R8 | R8D | R8W | R8B -> R8W
      | R9 | R9D | R9W | R9B -> R9W
      | R10 | R10D | R10W | R10B -> R10W
      | R11 | R11D | R11W | R11B -> R11W
      | R12 | R12D | R12W | R12B -> R12W
      | R13 | R13D | R13W | R13B -> R13W
      | R14 | R14D | R14W | R14B -> R14W
      | R15 | R15D | R15W | R15B -> R15W
      | _ -> isa_error "should not use high 1-byte reg"
    else if s = 4L then
      match r with
      | RAX | EAX | AX | AL -> EAX
      | RCX | ECX | CX | CL -> ECX
      | RDX | EDX | DX | DL -> EDX
      | RBX | EBX | BX | BL -> EBX
      | RSP | ESP | SP | SPL -> ESP
      | RBP | EBP | BP | BPL -> EBP
      | RSI | ESI | SI | SIL -> ESI
      | RDI | EDI | DI | DIL -> EDI
      | R8 | R8D | R8W | R8B -> R8D
      | R9 | R9D | R9W | R9B -> R9D
      | R10 | R10D | R10W | R10B -> R10D
      | R11 | R11D | R11W | R11B -> R11D
      | R12 | R12D | R12W | R12B -> R12D
      | R13 | R13D | R13W | R13B -> R13D
      | R14 | R14D | R14W | R14B -> R14D
      | R15 | R15D | R15W | R15B -> R15D
      | _ -> isa_error "should not use high 1-byte reg"
    else if s = 8L then
      match r with
      | RAX | EAX | AX | AL -> RAX
      | RCX | ECX | CX | CL -> RCX
      | RDX | EDX | DX | DL -> RDX
      | RBX | EBX | BX | BL -> RBX
      | RSP | ESP | SP | SPL -> RSP
      | RBP | EBP | BP | BPL -> RBP
      | RSI | ESI | SI | SIL -> RSI
      | RDI | EDI | DI | DIL -> RDI
      | R8 | R8D | R8W | R8B -> R8
      | R9 | R9D | R9W | R9B -> R9
      | R10 | R10D | R10W | R10B -> R10
      | R11 | R11D | R11W | R11B -> R11
      | R12 | R12D | R12W | R12B -> R12
      | R13 | R13D | R13W | R13B -> R13
      | R14 | R14D | R14W | R14B -> R14
      | R15 | R15D | R15W | R15B -> R15
      | _ -> isa_error "should not use high 1-byte reg"
    else
      isa_error "invalid register size"

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

  let get_reg_offset_size (r: register) : int64 * int64 =
    match r with
    | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> (0L, 8L)
    | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D -> (0L, 4L)
    | AX | CX | DX | BX | SP | BP | SI | DI | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W -> (0L, 2L)
    | AH | CH | DH | BH -> (1L, 1L)
    | AL | CL | DL | BL | SPL | BPL | SIL | DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B -> (0L, 1L)

  let get_reg_full_size (r: register) : int64 =
    match r with
    | _ -> 8L (* Note: vector reg has different full sizes!!! *)

  let reg_name_list = [
    "rax"; "rcx"; "rdx"; "rbx";
    "rsp"; "rbp"; "rsi"; "rdi";
    "r8"; "r9"; "r10"; "r11";
    "r12"; "r13"; "r14"; "r15"
  ]

  let string_of_reg_idx (idx: int) : string =
    List.nth reg_name_list idx

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

  let get_op_size (op: operand) : int64 =
    match op with
    | RegOp r -> get_reg_size r
    | LdOp (_, _, _, _, size)
    | StOp (_, _, _, _, size) -> size
    | _ -> isa_error "cannot get size for the given op"

  let cmp_operand (op1: operand) (op2: operand) : bool = (* true for equal *)
    match op1, op2 with
    | ImmOp i1, ImmOp i2 -> i1 = i2
    | RegOp r1, RegOp r2 -> r1 = r2
    | MemOp (d1, b1, i1, s1), MemOp (d2, b2, i2, s2) ->
      d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2
    | LdOp (d1, b1, i1, s1, size1), LdOp (d2, b2, i2, s2, size2)
    | StOp (d1, b1, i1, s1, size1), StOp (d2, b2, i2, s2, size2) ->
        d1 = d2 && b1 = b2 && i1 = i2 && s1 = s2 && size1 = size2
    | LabelOp l1, LabelOp l2 -> l1 = l2
    | _ -> false

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
    | JB | JBe | JA | JAe

  type instruction =
    | Mov of operand * operand
    | MovS of operand * operand
    | MovZ of operand * operand
    | Xchg of operand * operand
    | Lea of operand * operand
    | Add of operand * operand * operand
    | Adc of operand * operand * operand (* Add and carry, our type checker does not calculate on this, just set dest as top *)
    | Sub of operand * operand * operand
    | Mul of operand * operand * operand
    | Imul of operand * operand * operand
    | Sal of operand * operand * operand
    | Sar of operand * operand * operand
    | Shr of operand * operand * operand
    | Rol of operand * operand * operand
    | Ror of operand * operand * operand
    | Xor of operand * operand * operand
    | Not of operand * operand
    | And of operand * operand * operand
    | Or of operand * operand * operand
    | Bswap of operand
    | RepStosq
    | RepMovsq
    | Cmp of operand * operand
    | Test of operand * operand
    | Jmp of label
    | Jcond of label * branch_cond
    | Call of label
    | Push of operand
    | Pop of operand
    | Ret
    | Nop
    | Syscall
    | Hlt

  type basic_block = {
    label: label;
    insts: instruction list;
  }

  type func = {
    name: label;
    body: basic_block list;
  }

  type prog = {
    funcs: func list;
    imm_var_map: imm_var_map;
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
    | "jbe" | "jb" | "jnb" -> true
    | "rep" -> true (* dirty impl *)
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
    | Xchg _ -> "xchg"
    | Lea _ -> "lea"
    | Add _ -> "add"
    | Adc _ -> "adc"
    | Sub _ -> "sub"
    | Mul _ -> "mul"
    | Imul _ -> "imul"
    | Sal _ -> "sal"
    | Sar _ -> "sar"
    | Shr _ -> "shr"
    | Rol _ -> "rol"
    | Ror _ -> "ror"
    | Xor _ -> "xor"
    | Not _ -> "not"
    | And _ -> "and"
    | Or _ -> "or"
    | Bswap _ -> "bswap"
    | RepStosq -> "rep stosq"
    | RepMovsq -> "rep movsq"
    | Cmp _ -> "cmp"
    | Test _ -> "test"
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
        | JB -> "jb"
        | JBe -> "jbe"
        | JA -> "ja"
        | JAe -> "jae"
      end
    | Call _ -> "call"
    | Push _ -> "push"
    | Pop _ -> "pop"
    | Ret -> "ret"
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"

  let get_op_list (inst: instruction) : operand list =
    match inst with
    | Mov (op0, op1) | MovS (op0, op1) | MovZ (op0, op1) 
    | Lea (op0, op1) | Not (op0, op1) | Cmp (op0, op1) | Test (op0, op1) -> [op0; op1]
    | Add (op0, op1, op2) | Sub (op0, op1, op2) | Sal (op0, op1, op2)
    | Sar (op0, op1, op2) | Shr (op0, op1, op2) | Xor (op0, op1, op2)
    | And (op0, op1, op2) | Or (op0, op1, op2) -> [op0; op1; op2]
    | Push op | Pop op -> [op]
    | _ -> []

  let pp_prog (lvl: int) (p: prog) =
    PP.print_lvl lvl "Prog\n";
    List.iteri (
      fun i func -> 
        PP.print_lvl lvl "<Func %d %s>\n" i func.name;
        List.iter (
          fun bb ->
            PP.print_lvl (lvl + 1) "%s\n" bb.label;
            List.iter (
              fun inst -> PP.print_lvl (lvl + 2) "%s\n" (mnemonic_of_instruction inst)
            ) bb.insts
        ) func.body
    ) p.funcs

end

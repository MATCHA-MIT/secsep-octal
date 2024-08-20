(* ISA interface *)
open Pretty_print

module Isa = struct
  (* TODO: Other exception on isa side *)
  exception IsaError of string

  let isa_error msg = raise (IsaError ("[Isa Error] " ^ msg))

  let string_is_sth (map: (string * 'a) list) (m: string) : bool =
    List.find_opt (fun (opcode, _) -> opcode = m) map != None

  let string_of_sth (map: (string * 'a) list) (op: 'a) : string option =
    List.find_map
      (fun (str, o) -> if o = op then Some str else None)
      map

  let string_to_sth (map: (string * 'a) list) (opcode: string) : 'a option =
    List.find_map
      (fun (str, o) -> if str = opcode then Some o else None)
      map

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

  let reg_map = [
    ("rax", RAX); ("eax", EAX); ("ax", AX); ("ah", AH); ("al", AL);
    ("rcx", RCX); ("ecx", ECX); ("cx", CX); ("ch", CH); ("cl", CL);
    ("rdx", RDX); ("edx", EDX); ("dx", DX); ("dh", DH); ("dl", DL);
    ("rbx", RBX); ("ebx", EBX); ("bx", BX); ("bh", BH); ("bl", BL);
    ("rsp", RSP); ("esp", ESP); ("sp", SP); ("spl", SPL);
    ("rbp", RBP); ("ebp", EBP); ("bp", BP); ("sbl", BPL);
    ("rsi", RSI); ("esi", ESI); ("si", SI); ("sil", SIL);
    ("rdi", RDI); ("edi", EDI); ("di", DI); ("dil", DIL);
    ("r8", R8); ("r8d", R8D); ("r8w", R8W); ("r8b", R8B);
    ("r9", R9); ("r9d", R9D); ("r9w", R9W); ("r9b", R9B);
    ("r10", R10); ("r10d", R10D); ("r10w", R10W); ("r10b", R10B);
    ("r11", R11); ("r11d", R11D); ("r11w", R11W); ("r11b", R11B);
    ("r12", R12); ("r12d", R12D); ("r12w", R12W); ("r12b", R12B);
    ("r13", R13); ("r13d", R13D); ("r13w", R13W); ("r13b", R13B);
    ("r14", R14); ("r14d", R14D); ("r14w", R14W); ("r14b", R14B);
    ("r15", R15); ("r15d", R15D); ("r15w", R15W); ("r15b", R15B);
  ]

  let string_of_reg (r: register) : string = Option.get (string_of_sth reg_map r)
  let string_to_reg = string_to_sth reg_map

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

  let scale_to_string (s: scale) : string = Int64.to_string (scale_val s)

  type operand =
    | ImmOp of immediate
    | RegOp of register
    | MemOp of immediate option * register option * register option * scale option (* disp, base, index, scale *)
    | LdOp of immediate option * register option * register option * scale option * int64
    | StOp of immediate option * register option * register option * scale option * int64
    | LabelOp of label

  let string_of_option (to_string: 'a -> string) (x: 'a option) : string =
    match x with
    | Some x -> to_string x
    | None -> ""

  let rec string_of_operand (op: operand): string =
    match op with
    | ImmOp imm -> string_of_immediate imm
    | RegOp r -> string_of_reg r
    | MemOp (disp, base, index, scale) ->
      let disp_str = string_of_option string_of_immediate disp in
      let base_str = string_of_option string_of_reg base in
      let index_str = string_of_option string_of_reg index in
      let scale_str = string_of_option scale_to_string (scale) in
      Printf.sprintf "%s(%s,%s,%s)" disp_str base_str index_str scale_str
    | LdOp (disp, base, index, scale, size) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale)) in
      Printf.sprintf "Ld(%s,%s)" addr_str (Int64.to_string size)
    | StOp (disp, base, index, scale, size) ->
      let addr_str = string_of_operand (MemOp (disp, base, index, scale)) in
      Printf.sprintf "St(%s,%s)" addr_str (Int64.to_string size)
    | LabelOp label -> label

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

  let common_opcode_list = [
    "mov"; "movabs"; "movs"; "movz"; "lea"; 
    "xchg";
    "add"; "adc"; "sub"; "mul"; "imul"; 
    "sal"; "sar"; "shl"; "shr"; "rol"; "ror";
    "xor"; "and"; "or"; "not"; "bswap";
    "cmp"; "test";
    "push"; "pop"
  ]

  type bop =
    | Add | Adc | Sub
    | Mul (* unsigned multiply *) | Imul (* signed multiply *)
    | Sal | Sar | Shl | Shr (* Sal = Shl, Sar is signed, Shr is unsigned*)
    | Rol | Ror
    | Xor | And | Or

  let bop_opcode_map = [
    ("add", Add); ("adc", Adc); ("sub", Sub);
    ("mul", Mul); ("imul", Imul);
    ("sal", Sal); ("sar", Sar); ("shl", Shl); ("shr", Shr);
    ("rol", Rol); ("ror", Ror);
    ("xor", Xor); ("and", And); ("or", Or)
  ]
  
  type uop =
    | Mov | MovS | MovZ
    | Lea
    | Not | Bswap

  let uop_opcode_map = [
    ("mov", Mov); ("movabs", Mov);
    ("movs", MovS); ("movz", MovZ);
    ("lea", Lea);
    ("not", Not); ("bswap", Bswap)
  ]

  type branch_cond =
    | JNe | JE | JL | JLe | JG | JGe
    | JB | JBe | JA | JAe | JOther

  let cond_jump_opcode_map = [
    ("jne", JNe); ("jnz", JNe);               (* != *)
    ("je", JE); ("jz", JE);                   (* = *)
    ("jl", JL); ("jnge", JL);                 (* < signed *)
    ("jle", JLe); ("jng", JLe);               (* <= signed *)
    ("jg", JG); ("jnle", JG);                 (* > signed *)
    ("jge", JGe); ("jnl", JGe);               (* >= signed *)
    ("jb", JB); ("jnae", JB); ("jc", JB);     (* < unsigned *)
    ("jbe", JBe); ("jna", JBe);               (* <= unsigned*)
    ("ja", JA); ("jnbe", JA);                 (* > unsigned *)
    ("jae", JAe); ("jnb", JAe); ("jnc", JAe); (* >= unsigned *)
    ("jother", JOther); (* Dirty implementation for print *)
    ("jo", JOther); ("jno", JOther); ("js", JOther); ("jns", JOther);
    ("jp", JOther); ("jpe", JOther); ("jnp", JOther); ("jpo", JOther);
    ("jcxz", JOther); ("jecxz", JOther);
  ]

  let opcode_is_binst = string_is_sth bop_opcode_map
  let opcode_is_uinst = string_is_sth uop_opcode_map
  let opcode_is_cond_jump = string_is_sth cond_jump_opcode_map
  
  let opcode_of_binst = string_of_sth bop_opcode_map
  let opcode_of_uinst = string_of_sth uop_opcode_map
  let opcode_of_cond_jump = string_of_sth cond_jump_opcode_map
  
  let op_of_binst = string_to_sth bop_opcode_map
  let op_of_uinst = string_to_sth uop_opcode_map
  let op_of_cond_jump = string_to_sth cond_jump_opcode_map

  type instruction =
    (* | Mov of operand * operand
    | MovS of operand * operand
    | MovZ of operand * operand
    | Lea of operand * operand *)
    | BInst of bop * operand * operand * operand
    | UInst of uop * operand * operand
    | Xchg of operand * operand * operand * operand
    | Cmp of operand * operand
    | Test of operand * operand
    | Push of operand
    | Pop of operand
    | RepStosq
    | RepMovsq
    | Jmp of label
    | Jcond of branch_cond * label
    | Call of label
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
    | "jmp" -> true
    | "rep" -> true (* dirty impl *)
    | _ -> (* Check if conditional branch opcode*)
      opcode_is_cond_jump m

  let inst_is_uncond_jump (inst: instruction) : bool =
    match inst with
    | Jmp _ -> true
    | _ -> false

  let mnemonic_of_instruction (inst: instruction) : string =
    match inst with
    (* | Mov _ -> "mov"
    | MovS _ -> "movs"
    | MovZ _ -> "movz"
    | Lea _ -> "lea" *)
    | BInst (bop, _, _, _) ->
      begin match opcode_of_binst bop with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a bop"
      end
    | UInst (uop, _, _) ->
      begin match opcode_of_uinst uop with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a uop"
      end
    | Cmp _ -> "cmp"
    | Test _ -> "test"
    | Push _ -> "push"
    | Pop _ -> "pop"
    | Xchg _ -> "xchg"
    | RepStosq -> "rep stosq"
    | RepMovsq -> "rep movsq"
    | Jmp _ -> "jmp"
    | Jcond (cond, _) ->
      begin match opcode_of_cond_jump cond with
      | Some s -> s
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call _ -> "call"
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"

  let string_of_instruction (inst: instruction) : string =
    let get_tab (opcode: string): string =
      if String.length opcode <= 3 then "\t\t" else "\t"
    in
    match inst with
    | BInst (bop, dst, src2, src1) ->
      begin match opcode_of_binst bop with
      | Some opcode -> 
        Printf.sprintf 
          "%s%s%s\t<-\t%s,\t%s" 
          opcode (get_tab opcode) (string_of_operand dst) (string_of_operand src2) (string_of_operand src1)
      | None -> isa_error "cannot find opcode for a bop"
      end
    | UInst (uop, dst, src) ->
      begin match opcode_of_uinst uop with
      | Some opcode ->
        Printf.sprintf 
            "%s%s%s\t<-\t%s" 
            opcode (get_tab opcode) (string_of_operand dst) (string_of_operand src)
      | None -> isa_error "cannot find opcode for a uop"
      end
    | Cmp (src2, src1) ->
      Printf.sprintf "cmp\t\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Test (src2, src1) ->
      Printf.sprintf "test\t%s,\t%s" (string_of_operand src2) (string_of_operand src1)
    | Push src -> Printf.sprintf "push\t%s" (string_of_operand src)
    | Pop src -> Printf.sprintf "pop\t\t%s" (string_of_operand src)
    | Xchg (dst2, dst1, _, _) -> 
      Printf.sprintf "xchg\t%s,\t%s" (string_of_operand dst2) (string_of_operand dst1)
    | RepStosq -> "rep stosq"
    | RepMovsq -> "rep movsq"
    | Jmp label -> Printf.sprintf "jmp\t\t%s" label
    | Jcond (cond, label) ->
      begin match opcode_of_cond_jump cond with
      | Some opcode -> Printf.sprintf "%s\t\t%s" opcode label
      | None -> isa_error "cannot find opcode for a cond jump"
      end
    | Call label -> Printf.sprintf "call\t\t%s" label
    | Nop -> "nop"
    | Syscall -> "syscall"
    | Hlt -> "hlt"

  (* let get_op_list (inst: instruction) : operand list =
    match inst with
    | Mov (op0, op1) | MovS (op0, op1) | MovZ (op0, op1) 
    | Lea (op0, op1) | Not (op0, op1) | Cmp (op0, op1) | Test (op0, op1) -> [op0; op1]
    | Add (op0, op1, op2) | Sub (op0, op1, op2) | Sal (op0, op1, op2)
    | Sar (op0, op1, op2) | Shr (op0, op1, op2) | Xor (op0, op1, op2)
    | And (op0, op1, op2) | Or (op0, op1, op2) -> [op0; op1; op2]
    | Push op | Pop op -> [op]
    | _ -> [] *)

  let pp_prog (lvl: int) (p: prog) =
    PP.print_lvl lvl "Prog\n";
    List.iteri (
      fun i func -> 
        PP.print_lvl lvl "<Func %d %s>\n" i func.name;
        List.iter (
          fun bb ->
            PP.print_lvl (lvl + 1) "%s\n" bb.label;
            List.iter (
              fun inst -> PP.print_lvl (lvl + 2) "%s\n" (string_of_instruction inst)
            ) bb.insts
        ) func.body
    ) p.funcs

end

open Sexplib
open Sexplib.Std

module IsaBasic = struct
  (* TODO: Other exception on isa side *)
  exception IsaError of string

  let isa_error msg = raise (IsaError ("[Isa Error] " ^ msg))

  let string_is_sth (map: (string * 'a) list) (m: string) : bool =
    List.find_opt (fun (opcode, _) -> opcode = m) map <> None

  let string_of_sth (map: (string * 'a) list) (op: 'a) : string option =
    List.find_map
      (fun (str, o) -> if o = op then Some str else None)
      map

  let string_to_sth (map: (string * 'a) list) (opcode: string) : 'a option =
    List.find_map
      (fun (str, o) -> if str = opcode then Some o else None)
      map

  type label = string
  [@@deriving sexp]

  module StringSet = Set.Make(String)

  let ret_label = ".Ret"

  type imm_var_id = int
  [@@deriving sexp]

  module StrM = Map.Make(String)
  module IntM = Map.Make(Int)
  type imm_var_map = imm_var_id StrM.t
  type imm_var_rev_map = string IntM.t
  (* TODO: Fix this dirty implementation later!!! *)
  type list_t = (string * imm_var_id) list
  [@@deriving sexp]
  let imm_var_map_of_sexp (s_exp: Sexp.t) : imm_var_map =
    StrM.of_list (list_t_of_sexp s_exp)
  let sexp_of_imm_var_map (s: imm_var_map) : Sexp.t =
    sexp_of_list_t (StrM.to_list s)

  type register =
    |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15
    |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
    |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
    | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B
    | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7
    (* | R0 *)
  [@@deriving sexp]

  let reg_map = [
    ("rax", RAX); ("eax", EAX); ("ax", AX); ("ah", AH); ("al", AL);
    ("rcx", RCX); ("ecx", ECX); ("cx", CX); ("ch", CH); ("cl", CL);
    ("rdx", RDX); ("edx", EDX); ("dx", DX); ("dh", DH); ("dl", DL);
    ("rbx", RBX); ("ebx", EBX); ("bx", BX); ("bh", BH); ("bl", BL);
    ("rsp", RSP); ("esp", ESP); ("sp", SP); ("spl", SPL);
    ("rbp", RBP); ("ebp", EBP); ("bp", BP); ("bpl", BPL);
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
    ("xmm0", XMM0); ("xmm1", XMM1); ("xmm2", XMM2); ("xmm3", XMM3); 
    ("xmm4", XMM4); ("xmm5", XMM5); ("xmm6", XMM6); ("xmm7", XMM7); 
  ]

  let string_of_reg (r: register) : string = Option.get (string_of_sth reg_map r)

  let ocaml_string_of_reg (r: register) : string =
    (* make uppercase *)
    let s = string_of_reg r in
    String.uppercase_ascii s

  let ocaml_string_of_reg_op (r: register) : string =
    Printf.sprintf "RegOp %s" (ocaml_string_of_reg r)

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
      (* TODO: Check *)
      | XMM0 -> XMM0
      | XMM1 -> XMM1
      | XMM2 -> XMM2
      | XMM3 -> XMM3
      | XMM4 -> XMM4
      | XMM5 -> XMM5
      | XMM6 -> XMM6
      | XMM7 -> XMM7
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
      | XMM0 -> XMM0
      | XMM1 -> XMM1
      | XMM2 -> XMM2
      | XMM3 -> XMM3
      | XMM4 -> XMM4
      | XMM5 -> XMM5
      | XMM6 -> XMM6
      | XMM7 -> XMM7
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
      | XMM0 -> XMM0
      | XMM1 -> XMM1
      | XMM2 -> XMM2
      | XMM3 -> XMM3
      | XMM4 -> XMM4
      | XMM5 -> XMM5
      | XMM6 -> XMM6
      | XMM7 -> XMM7
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
      | XMM0 -> XMM0
      | XMM1 -> XMM1
      | XMM2 -> XMM2
      | XMM3 -> XMM3
      | XMM4 -> XMM4
      | XMM5 -> XMM5
      | XMM6 -> XMM6
      | XMM7 -> XMM7
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
    | XMM0 -> 16
    | XMM1 -> 17
    | XMM2 -> 18
    | XMM3 -> 19
    | XMM4 -> 20
    | XMM5 -> 21
    | XMM6 -> 22
    | XMM7 -> 23
    (* | R0 -> 16 *)

  let get_full_reg_by_idx (idx: int) : register =
    match idx with
    | 0 -> RAX
    | 1 -> RCX
    | 2 -> RDX
    | 3 -> RBX
    | 4 -> RSP
    | 5 -> RBP
    | 6 -> RSI
    | 7 -> RDI
    | 8 -> R8
    | 9 -> R9
    | 10 -> R10
    | 11 -> R11
    | 12 -> R12
    | 13 -> R13
    | 14 -> R14
    | 15 -> R15
    | 16 -> XMM0
    | 17 -> XMM1
    | 18 -> XMM2
    | 19 -> XMM3
    | 20 -> XMM4
    | 21 -> XMM5
    | 22 -> XMM6
    | 23 -> XMM7
    | _ -> isa_error "get_full_reg_by_idx: invalid idx"

  let get_reg_offset_size (r: register) : int64 * int64 =
    match r with
    | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> (0L, 8L)
    | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D -> (0L, 4L)
    | AX | CX | DX | BX | SP | BP | SI | DI | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W -> (0L, 2L)
    | AH | CH | DH | BH -> (1L, 1L)
    | AL | CL | DL | BL | SPL | BPL | SIL | DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B -> (0L, 1L)
    | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 -> isa_error "should not get_reg_offset_size for xmm registers"

  let get_reg_full_size (r: register) : int64 =
    match r with
    | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 -> 16L
    | _ -> 8L (* Note: vector reg has different full sizes!!! *)
  
  let get_gpr_full_size () : int64 = get_reg_full_size RAX

  let reg_name_list = [
    "rax"; "rcx"; "rdx"; "rbx";
    "rsp"; "rbp"; "rsi"; "rdi";
    "r8"; "r9"; "r10"; "r11";
    "r12"; "r13"; "r14"; "r15";
    "xmm0"; "xmm1"; "xmm2"; "xmm3";
    "xmm4"; "xmm5"; "xmm6"; "xmm7";
  ]

  let string_of_reg_idx (idx: int) : string =
    List.nth reg_name_list idx

  let rsp_idx = get_reg_idx RSP

  let total_reg_num : int = 24

  let get_reg_size (r: register) : int64 =
    match r with
    |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15  -> 8L
    |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D -> 4L
    |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W -> 2L
    | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B -> 1L
    | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 -> 16L

  let is_reg_idx_callee_saved (r_idx: int) : bool =
    List.mem r_idx [
      3; (* RBX *)
      4; (* RSP *)
      5; (* RBP *)
      12; 13; 14; 15
    ]

  let is_reg_callee_saved (r: register) : bool =
    is_reg_idx_callee_saved (get_reg_idx r)

  let is_xmm (r: register) : bool =
    match r with
    | XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 -> true
    | _ -> false

  type immediate =
    | ImmNum of int64
    | ImmLabel of imm_var_id
    | ImmBExp of immediate * immediate
  [@@deriving sexp]

  let rec string_of_immediate (i: immediate) : string =
    match i with
    | ImmNum x -> Int64.to_string x
    | ImmLabel x -> "var " ^ (string_of_int x)
    | ImmBExp (i1, i2) -> "(" ^ (string_of_immediate i1) ^ ") + (" ^ (string_of_immediate i2) ^ ")"

  let ocaml_string_of_int (x: int) : string =
    Printf.sprintf (if x >= 0 then "%d" else "(%d)") x

  let ocaml_string_of_int64 (x: int64) : string =
    Printf.sprintf (if x >= 0L then "%LdL" else "(%LdL)") x

  let rec ocaml_string_of_immediate (i: immediate) : string =
    match i with
    | ImmNum x -> Printf.sprintf "ImmNum %s" (ocaml_string_of_int64 x)
    | ImmLabel x -> Printf.sprintf "ImmLabel %s" (ocaml_string_of_int x)
    | ImmBExp (i1, i2) -> Printf.sprintf "ImmBExp (%s, %s)" (ocaml_string_of_immediate i1) (ocaml_string_of_immediate i2)

  let ocaml_string_of_immediate_op (i: immediate) : string =
    Printf.sprintf "ImmOp (%s)" (ocaml_string_of_immediate i)

  type scale = Scale1 | Scale2 | Scale4 | Scale8
  [@@deriving sexp]

  let scale_val (s: scale) : int64 =
    match s with
    | Scale1 -> 1L
    | Scale2 -> 2L
    | Scale4 -> 4L
    | Scale8 -> 8L

  let scale_to_string (s: scale) : string = Int64.to_string (scale_val s)
  
  let ocaml_scale_to_string (s: scale) : string =
    match s with
    | Scale1 -> "Scale1"
    | Scale2 -> "Scale2"
    | Scale4 -> "Scale4"
    | Scale8 -> "Scale8"

  let string_of_option (to_string: 'a -> string) (x: 'a option) : string =
    match x with
    | Some x -> to_string x
    | None -> ""
  
  let ocaml_string_of_option (to_ocaml_string: 'a -> string) (x: 'a option) : string =
    match x with
    | Some x -> Printf.sprintf "Some (%s)" (to_ocaml_string x)
    | None -> "None"

  let common_opcode_list = [
    "xorps"; (* For the parser to work, this has to appear before xor *)
    "movabs"; "mov"; "movz"; "lea";
    "xchg";
    "add"; "adc"; "sub"; "mul"; "imul";
    "shrd";
    "sal"; "sar"; "shl"; "shr"; "rol"; "ror";
    "xor"; "and"; "or"; "not"; "bswap"; "neg"; "inc"; "dec";
    "cmp"; "test";
    "push"; "pop";
    "punpck"; "packus";
    "pxor"; "pand"; "por";
    "psll"; "psrl";
    "pshufd"; "pshuflw"; "pshufhw";
  ]

  let rep_opcode_list = [
    "movs"; "lods"; "stos";
  ]

  type bop =
    | Add | Adc | Sub
    | Mul (* unsigned multiply *) | Imul (* signed multiply *)
    | Sal | Sar | Shl | Shr (* Sal = Shl, Sar is signed, Shr is unsigned*)
    | Rol | Ror
    | Xor | And | Or
    | Punpck | Packus
    | Pxor | Pand | Por
    | Psll | Psrl
    | Xorps
  [@@deriving sexp]

  let bop_opcode_map = [
    ("add", Add); ("adc", Adc); ("sub", Sub);
    ("mul", Mul); ("imul", Imul);
    ("sal", Sal); ("sar", Sar); ("shl", Shl); ("shr", Shr);
    ("rol", Rol); ("ror", Ror);
    ("xor", Xor); ("and", And); ("or", Or);
    ("punpck", Punpck); ("packus", Packus);
    ("pxor", Pxor); ("pand", Pand); ("por", Por);
    ("psll", Psll); ("psrl", Psrl);
    ("xorps", Xorps);
  ]

  let bop_opcode_ocaml_str_map = [
    ("Add", Add); ("Adc", Adc); ("Sub", Sub);
    ("Mul", Mul); ("Imul", Imul);
    ("Sal", Sal); ("Sar", Sar); ("Shl", Shl); ("Shr", Shr);
    ("Rol", Rol); ("Ror", Ror);
    ("Xor", Xor); ("And", And); ("Or", Or);
    ("Punpck", Punpck); ("Packus", Packus);
    ("Pxor", Pxor); ("Pand", Pand); ("Por", Por);
    ("Psll", Psll); ("Psrl", Psrl);
    ("Xorps", Xorps);
  ]
  
  type uop =
    | Mov 
    (* | MovS *) 
    (* NOTE: MovS is not simply mov sign, it also updates rsi and rdi. Removed and may need to be handled later*)
    | MovZ
    | Lea
    | Not | Bswap
    | Neg
    | Inc |Dec
  [@@deriving sexp]

  let uop_set_flag (uop: uop) : bool =
    match uop with
    | Inc | Dec -> true
    | _ -> false

  let uop_opcode_map = [
    ("mov", Mov); ("movabs", Mov);
    (* ("movs", MovS);  *)
    ("movz", MovZ);
    ("lea", Lea);
    ("not", Not); ("bswap", Bswap);
    ("neg", Neg);
    ("inc", Inc); ("dec", Dec);
  ]

  let uop_opcode_ocaml_str_map = [
    ("Mov", Mov);
    (* ("MovS", MovS);  *)
    ("MovZ", MovZ);
    ("Lea", Lea);
    ("Not", Not); ("Bswap", Bswap);
    ("Neg", Neg);
    ("Inc", Inc); ("Dec", Dec);
  ]

  type top =
    | Shrd
    | Pshufd | Pshuflw | Pshufhw
  [@@deriving sexp]

  let top_opcode_map = [
    ("shrd", Shrd);
    ("pshufd", Pshufd);
    ("pshuflw", Pshuflw);
    ("pshufhw", Pshufhw);
  ]

  let top_opcode_ocaml_str_map = [
    ("Shrd", Shrd);
    ("Pshufd", Pshufd);
    ("Pshuflw", Pshuflw);
    ("Pshufhw", Pshufhw);
  ]

  type branch_cond =
    | JNe | JE | JL | JLe | JG | JGe
    | JB | JBe | JA | JAe | JOther
  [@@deriving sexp]

  let cond_jump_opcode_map = [
    ("jne", JNe); ("jnz", JNe);               (* <> *)
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

  let cond_jump_opcode_ocaml_str_map = [
    ("JNe", JNe);
    ("JE", JE);
    ("JL", JL);
    ("JLe", JLe);
    ("JG", JG);
    ("JGe", JGe);
    ("JB", JB);
    ("JBe", JBe);
    ("JA", JA);
    ("JAe", JAe);
    ("JOther", JOther); (* Dirty implementation for print *)
  ]

  let opcode_is_binst = string_is_sth bop_opcode_map
  let opcode_is_uinst = string_is_sth uop_opcode_map
  let opcode_is_tinst = string_is_sth top_opcode_map
  let opcode_is_cond_jump = string_is_sth cond_jump_opcode_map
  
  let opcode_of_binst = string_of_sth bop_opcode_map
  let opcode_of_uinst = string_of_sth uop_opcode_map
  let opcode_of_tinst = string_of_sth top_opcode_map
  let opcode_of_cond_jump = string_of_sth cond_jump_opcode_map

  let ocaml_opcode_of_binst = string_of_sth bop_opcode_ocaml_str_map
  let ocaml_opcode_of_uinst = string_of_sth uop_opcode_ocaml_str_map
  let ocaml_opcode_of_tinst = string_of_sth top_opcode_ocaml_str_map
  let ocaml_opcode_of_cond_jump = string_of_sth cond_jump_opcode_ocaml_str_map
  
  let op_of_binst = string_to_sth bop_opcode_map
  let op_of_uinst = string_to_sth uop_opcode_map
  let op_of_tinst = string_to_sth top_opcode_map
  let op_of_cond_jump = string_to_sth cond_jump_opcode_map

  let is_label_function_entry (l: label) = l.[0] <> '.'

  let inst_referring_label (m: string) : bool =
    match m with
    | "call" | "callq"
    | "jmp" -> true
    | "rep" -> true (* dirty impl *)
    | _ -> (* Check if conditional branch opcode*)
      opcode_is_cond_jump m

end

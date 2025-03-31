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

  let sentry_func_label = "__sentry_func__"
  let ret_label = ".Ret"

  type imm_var_id = int
  [@@deriving sexp]

  let imm_var_unset: imm_var_id = -1

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

  let get_rev_imm_var_map (global_var_map: imm_var_map) : imm_var_rev_map =
    let kv_list = StrM.to_list global_var_map in
    let vk_map = List.fold_left (fun acc_map (k, v) ->
      if IntM.mem v acc_map then
        isa_error "Unexpected duplicated value in imm_var_map";
      IntM.add v k acc_map
    ) IntM.empty kv_list in
    vk_map


  (* size in bytes *)
  type data_size = int64
  [@@deriving sexp]

  (* size and offset in bytes *)
  type data_off_size = int64 * int64
  [@@deriving sexp]

  let gpr_full_off_size: data_off_size = (0L, 8L)
  let xmm_full_off_size: data_off_size = (0L, 16L)

  let data_off_size_to_size (os_opt: data_off_size option) : data_size option =
    match os_opt with
    | None -> None
    | Some (_, sz) -> Some sz
  
  let data_size_to_off_size (sz_opt: data_size option) : data_off_size option =
    match sz_opt with
    | None -> None
    | Some sz -> Some (0L, sz)

  let string_of_data_off_size (os: data_off_size option) =
    match os with
    | None -> "[]"
    | Some (off, size) ->
      Printf.sprintf "[%s:%s]"
        (off |> Int64.add size |> Int64.add (-1L) |> Int64.to_string)
        (off |> Int64.to_string)

  let string_of_data_size (sz: data_size option) =
    match sz with
    | None -> "(sz=unknown)"
    | Some size -> Printf.sprintf "(sz=%s)" (size |> Int64.to_string)


  type register =
    |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15
    |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
    |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
    | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B
    | XMM of (int * data_off_size option)
    (* | R0 *)
  [@@deriving sexp]

  let is_xmm (r: register) : bool =
    match r with
    | XMM _ -> true
    | _ -> false

  let get_xmm_off_size (r: register) : data_off_size option =
    match r with
    | XMM (_, os) -> os
    | _ -> isa_error "get_xmm_off_size: not xmm register"

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
    ("xmm0", XMM (0, None)); ("xmm1", XMM (1, None)); ("xmm2", XMM (2, None)); ("xmm3", XMM (3, None)); 
    ("xmm4", XMM (4, None)); ("xmm5", XMM (5, None)); ("xmm6", XMM (6, None)); ("xmm7", XMM (7, None)); 
    ("xmm8", XMM (80, None)); ("xmm9", XMM (9, None)); ("xmm10", XMM (10, None)); ("xmm11", XMM (11, None)); 
    ("xmm12", XMM (12, None)); ("xmm13", XMM (13, None)); ("xmm14", XMM (14, None)); ("xmm15", XMM (15, None)); 
  ]

  let string_of_reg (r: register) : string =
    if not (is_xmm r) then
      r |> string_of_sth reg_map |> Option.get
    else begin
      match r with
      | XMM (idx, os) -> "xmm" ^ (string_of_int idx) ^ (string_of_data_off_size os)
      | _ -> isa_error "expecting xmm register"
    end

  let string_of_reg_plain (r: register) : string =
    if not (is_xmm r) then
      r |> string_of_sth reg_map |> Option.get
    else begin
      match r with
      | XMM (idx, os) ->
        let _ = match os with
        | None -> ()
        | Some os ->
          if os <> xmm_full_off_size then
            isa_error (Printf.sprintf "expecting xmm register with full size, got %s" (string_of_data_off_size (Some os)))
        in
        "xmm" ^ (string_of_int idx)
      | _ -> isa_error "expecting xmm register"
    end

  (* parser uses this to convert reg name to register *)
  (* XMM's offset & size is initialized to None *)
  let string_to_reg = string_to_sth reg_map

  let ocaml_string_of_reg (r: register) : string =
    (* make uppercase *)
    let s = string_of_reg r in
    String.uppercase_ascii s

  let ocaml_string_of_reg_op (r: register) : string =
    Printf.sprintf "RegOp %s" (ocaml_string_of_reg r)

  (* aliased registers share the same index, but they have unique offset & size *)
  (* see get_reg_off_size for details *)
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
    | XMM (idx, _) -> 16 + idx
    (* | R0 -> 16 *)

  let total_reg_num : int = 32

  let rsp_idx = get_reg_idx RSP

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
    | _ ->
      if idx < 32 then XMM (idx - 16, Some xmm_full_off_size)
      else isa_error "get_full_reg_by_idx: invalid idx"

  let get_reg_offset_size_opt (r: register) : data_off_size option =
    match r with
    | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> Some (0L, 8L)
    | EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D -> Some (0L, 4L)
    | AX | CX | DX | BX | SP | BP | SI | DI | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W -> Some (0L, 2L)
    | AH | CH | DH | BH -> Some (1L, 1L)
    | AL | CL | DL | BL | SPL | BPL | SIL | DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B -> Some (0L, 1L)
    | XMM (_, os) -> os
  
  let fill_os_for_reg (r: register) (os: data_off_size) : register =
    match r with
    | XMM (idx, _) -> XMM (idx, Some os)
    | _ -> r

  let get_reg_offset_size (r: register) : data_off_size = get_reg_offset_size_opt r |> Option.get

  let get_reg_size (r: register) : data_size = r |> get_reg_offset_size |> snd

  (* full size of the physical register that r belongs to *)
  let get_reg_full_size (r: register) : data_size =
    let os = match r with
    | XMM _ -> xmm_full_off_size
    | _ -> gpr_full_off_size
    in
    os |> snd
  
  let get_gpr_full_size () : data_size = gpr_full_off_size |> snd

  (* physical registers *)
  let reg_name_list = [
    "rax"; "rcx"; "rdx"; "rbx";
    "rsp"; "rbp"; "rsi"; "rdi";
    "r8"; "r9"; "r10"; "r11";
    "r12"; "r13"; "r14"; "r15";
    "xmm0"; "xmm1"; "xmm2"; "xmm3";
    "xmm4"; "xmm5"; "xmm6"; "xmm7";
    "xmm8"; "xmm9"; "xmm10"; "xmm11";
    "xmm12"; "xmm13"; "xmm14"; "xmm15";
  ]

  (* physical registers *)
  let string_of_reg_idx (idx: int) : string =
    List.nth reg_name_list idx

  let callee_saved_reg_idx = [
    3; (* RBX *)
    4; (* RSP *)
    5; (* RBP *)
    12; 13; 14; 15
  ]

  let is_reg_idx_callee_saved (r_idx: int) : bool =
    List.mem r_idx callee_saved_reg_idx

  let is_reg_callee_saved (r: register) : bool =
    is_reg_idx_callee_saved (get_reg_idx r)

  let is_partial_update_reg_set_default_zero (r: register) : bool =
    not (is_xmm r) && get_reg_size r = 4L


  type flag =
    | CF | PF | AF | ZF | SF | OF
  [@@deriving sexp]

  let total_flag_num = 6

  let get_flag_idx (f: flag) : int =
    match f with
    | CF -> 0
    | PF -> 1
    | AF -> 2
    | ZF -> 3
    | SF -> 4
    | OF -> 5

  let is_valid_flag_idx (f_idx: int) : bool =
    f_idx >= 0 && f_idx < total_flag_num

  let string_of_flag (f: flag) : string =
    match f with
    | CF -> "CF"
    | PF -> "PF"
    | AF -> "AF"
    | ZF -> "ZF"
    | SF -> "SF"
    | OF -> "OF"

  type immediate =
    | ImmNum of int64 * data_size option
    | ImmLabel of imm_var_id * data_size option
    | ImmBExp of (immediate * immediate) (* sum of two immediates *) * data_size option
  [@@deriving sexp]

  let get_imm_size (i: immediate) : data_size option =
    match i with
    | ImmNum (_, size) -> size
    | ImmLabel (_, size) -> size
    | ImmBExp (_, size) -> size

  let set_imm_size (i: immediate) (size: data_size option) : immediate =
    match i with
    | ImmNum (imm, _) -> ImmNum (imm, size)
    | ImmLabel (imm, _) -> ImmLabel (imm, size)
    | ImmBExp (imm, _) -> ImmBExp (imm, size)
  
  let rec simplify_imm (i: immediate) : immediate =
    match i with
    | ImmBExp ((l, r), size_opt) -> begin
        let l = simplify_imm l in
        let r = simplify_imm r in
        match l, r with
        | ImmNum (l_val, _), ImmNum (r_val, _) -> ImmNum (Int64.add l_val r_val, size_opt)
        | _ -> i
      end
    | _ -> i

  let string_of_immediate (i: immediate) : string =
    Sexplib.Sexp.to_string_hum (sexp_of_immediate i)

  let ocaml_string_of_int (x: int) : string =
    Printf.sprintf (if x >= 0 then "%d" else "(%d)") x

  let ocaml_string_of_int64 (x: int64) : string =
    Printf.sprintf (if x >= 0L then "%LdL" else "(%LdL)") x

  let ocaml_string_of_immediate (i: immediate) : string =
    string_of_immediate i (* no longer ocaml format *)

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

  (*
    # Current supported instructions that does not have single-letter size suffix
  
    mov[sz](bq|wq|lq|bl|wl|bw): move with signed/zero extension

    c(btw|wtl|ltq): size conversion

    xorp[sd]: xor packed
  
    p(xor|andn|and|or): logical operations, bitwise, no suffix 

    pshufb: packed shuffle, 2 operands
    pshuf[wd]: packed shuffle, 3 operands
    pshuf[lh]w: 3 operands

    punpck[hl](bw|wd|dq|qdq)

    pack[us]s(dw|wb)

    mov[ua]p[sd]
    movdq[ua]

    # Future TODOs:

    (* ps[lr]ldq : shift double quadword logical, no suffix *)
    "pslldq"; "psrldq";

    "movss";  (* movss *)
  *)

  (* mnemonics that has a single-letter size suffix *)
  (* other cases are pre-handled in `decode_operand_size` *)
  let common_opcode_list = [
    (* packed integer operations *)
    "padd"; "psub"; "psll"; "psrl";
    
    (* shift double *)
    "shld"; "shrd";
    (* shift *)
    "sal"; "sar"; "shl"; "shr";

    "movabs"; "mov";
    "add"; "adc"; "sub"; "sbb";
    "mul"; "imul";
    "lea";
    "rol"; "ror";
    "xor"; "and"; "or"; "not"; "bswap"; "neg"; "inc"; "dec";
    "bt";
    "cmove"; (* TODO: add better support for parsing cmovxx *)

    "xchg";
    "push"; "pop";
    "cmp"; "test";
  ]

  let rep_opcode_list = [
    "movs"; "lods"; "stos";
  ]

  type bop =
    | Add | Adc | Sub | Sbb
    | Mul (* unsigned multiply *) | Imul (* signed multiply *)
    | Sal | Sar | Shl | Shr (* Sal = Shl, Sar is signed, Shr is unsigned*)
    | Rol | Ror
    | Xor | And | Or
    | CmovEq
    | Bt
    | Punpck | Packxs
    | Pshuf
    | Padd | Psub | Psll | Psrl
    | Pxor | Pandn | Pand | Por
    | Xorp
  [@@deriving sexp]

  let bop_opcode_map = [
    ("add", Add); ("adc", Adc); ("sub", Sub); ("sbb", Sbb);
    ("mul", Mul); ("imul", Imul);
    ("sal", Sal); ("sar", Sar); ("shl", Shl); ("shr", Shr);
    ("rol", Rol); ("ror", Ror);
    ("xor", Xor); ("and", And); ("or", Or);
    ("cmove", CmovEq);
    ("bt", Bt);
    ("punpckh", Punpck); ("punpckl", Punpck);
    ("packus", Packxs); ("packss", Packxs);
    ("padd", Padd); ("psub", Psub); ("pxor", Pxor); ("pandn", Pandn); ("pand", Pand); ("por", Por);
    ("psll", Psll); ("psrl", Psrl);
    ("xorp", Xorp);
    ("pshuf", Pshuf)
  ]

  let bop_opcode_ocaml_str_map = [
    ("Add", Add); ("Adc", Adc); ("Sub", Sub); ("Sbb", Sbb);
    ("Mul", Mul); ("Imul", Imul);
    ("Sal", Sal); ("Sar", Sar); ("Shl", Shl); ("Shr", Shr);
    ("Rol", Rol); ("Ror", Ror);
    ("Xor", Xor); ("And", And); ("Or", Or);
    ("CmovEq", CmovEq);
    ("Bt", Bt);
    ("Punpck", Punpck);
    ("Packxs", Packxs);
    ("Padd", Padd); ("Psub", Psub); ("Pxor", Pxor); ("Pandn", Pandn); ("Pand", Pand); ("Por", Por);
    ("Psll", Psll); ("Psrl", Psrl);
    ("Xorp", Xorp);
    ("Pshuf", Pshuf)
  ]
  
  type uop =
    | Mov 
    | MovS | MovZ
    | Lea
    | Not | Bswap
    | Neg
    | Inc | Dec
  [@@deriving sexp]

  let uop_opcode_map = [
    ("mov", Mov); ("movabs", Mov);
    ("movs", MovS); ("movz", MovZ);
    ("lea", Lea);
    ("not", Not); ("bswap", Bswap);
    ("neg", Neg);
    ("inc", Inc); ("dec", Dec);
  ]

  let uop_opcode_ocaml_str_map = [
    ("Mov", Mov);
    ("MovS", MovS); ("MovZ", MovZ);
    ("Lea", Lea);
    ("Not", Not); ("Bswap", Bswap);
    ("Neg", Neg);
    ("Inc", Inc); ("Dec", Dec);
  ]

  type top =
    | Shld
    | Shrd
  [@@deriving sexp]

  let top_opcode_map = [
    ("shld", Shld);
    ("shrd", Shrd);
  ]

  let top_opcode_ocaml_str_map = [
    ("Shld", Shld);
    ("Shrd", Shrd);
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

  let line_is_directive (line: string) : bool =
    let line = String.trim line in
    let pattern = Str.regexp {|^\.\([A-Za-z_][A-Za-z_0-9]*\)\b\([^:].*\)?$|} in
    Str.string_match pattern line 0
  
  let line_is_label (line: string) : bool =
    let line = String.trim line in
    let pattern = Str.regexp {|^\([\.A-Za-z_][\.A-Za-z_0-9]*\):$|} in
    Str.string_match pattern line 0

  let get_label_in_line (line: string) : string option =
    match line_is_label line with
    | false -> None
    | true ->
      let l = Str.group_beginning 1 in
      let r = Str.group_end 1 in
      Some (String.sub line l (r - l))
  
  let line_is_global_label (line: string) : bool =
    let line = String.trim line in
    let pattern = Str.regexp {|^\([A-Za-z_][\.A-Za-z_0-9]*\):$|} in
    Str.string_match pattern line 0

  let get_global_label_in_line (line: string) : string option =
    match line_is_global_label line with
    | false -> None
    | true ->
      let l = Str.group_beginning 1 in
      let r = Str.group_end 1 in
      Some (String.sub line l (r - l))

  let is_label_function_entry (l: label) = l.[0] <> '.'

  let is_compiler_gen_rom (l: label) = l.[0] = '.'

  (* instruction that has a single label as the operand *)
  (* We assume function label does not start with "."! *)
  let inst_with_single_label (m: string) : bool =
    match m with
    | "call" | "callq"
    | "jmp" -> true
    | "rep" -> true (* dirty impl: we treat the mnemonic after rep as a label, and handle it specially later *)
    | _ -> (* Check if conditional branch opcode *)
      opcode_is_cond_jump m

end

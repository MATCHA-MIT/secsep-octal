open Isa_basic
open Isa
open Full_mem_anno

module Parser = struct

  exception LexicalError of string
  exception ParseError of string

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))
  let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

  let parse_label (line: string) : Isa.label =
    let label_name = String.sub line 0 ((String.length line) - 1) in
    label_name

  let parse_register (name: string) : Isa.register option =
    if name = "rip" then None (* Dirty support for rip *)
    else
      match IsaBasic.string_to_reg name with
      | Some r -> Some r
      | None -> parse_error ("parse_register: invalid register name " ^ name)
    (* match name with
    |     "rax" -> Some RAX |     "rcx" -> Some RCX |     "rdx" -> Some RDX |     "rbx" -> Some RBX | "rsp" -> Some RSP  | "rbp" -> Some RBP  | "rsi" -> Some RSI  | "rdi" -> Some RDI  | "r8" -> Some R8  | "r9" -> Some R9  | "r10" -> Some R10  | "r11" -> Some R11  | "r12" -> Some R12  | "r13" -> Some R13  | "r14" -> Some R14  | "r15" -> Some R15
    |     "eax" -> Some EAX |     "ecx" -> Some ECX |     "edx" -> Some EDX |     "ebx" -> Some EBX | "esp" -> Some ESP  | "ebp" -> Some EBP  | "esi" -> Some ESI  | "edi" -> Some EDI  | "r8d" -> Some R8D | "r9d" -> Some R9D | "r10d" -> Some R10D | "r11d" -> Some R11D | "r12d" -> Some R12D | "r13d" -> Some R13D | "r14d" -> Some R14D | "r15d" -> Some R15D
    |      "ax" -> Some AX |      "cx" -> Some CX |      "dx" -> Some DX |      "bx" -> Some BX |  "sp" -> Some SP  |  "bp" -> Some BP  |  "si" -> Some SI  |  "di" -> Some DI  | "r8w" -> Some R8W | "r9w" -> Some R9W | "r10w" -> Some R10W | "r11w" -> Some R11W | "r12w" -> Some R12W | "r13w" -> Some R13W | "r14w" -> Some R14W | "r15w" -> Some R15W
    | "ah" -> Some AH | "al" -> Some AL | "ch" -> Some CH | "cl" -> Some CL | "dh" -> Some DH | "dl" -> Some DL | "bh" -> Some BH | "bl" -> Some BL |  "spl" -> Some SPL |  "bpl" -> Some BPL |  "sil" -> Some SIL |  "dil" -> Some DIL | "r8b" -> Some R8B | "r9b" -> Some R9B | "r10b" -> Some R10B | "r11b" -> Some R11B | "r12b" -> Some R12B | "r13b" -> Some R13B | "r14b" -> Some R14B | "r15b" -> Some R15B
    | "rip" -> None (* Dirty support for rip *)
    | _ -> parse_error ("parse_register: invalid register name " ^ name) *)

  let string_of_register (r: Isa.register) : string = IsaBasic.string_of_reg r
    (* match r with
    |     RAX -> "rax" |     RCX -> "rcx" |     RDX -> "rdx" |     RBX -> "rbx" | RSP -> "rsp"  | RBP -> "rbp"  | RSI -> "rsi"  | RDI -> "rdi"  | R8 -> "r8"  | R9 -> "r9"  | R10 -> "r10"  | R11 -> "r11"  | R12 -> "r12"  | R13 -> "r13"  | R14 -> "r14"  | R15 -> "r15"
    |     EAX -> "eax" |     ECX -> "ecx" |     EDX -> "edx" |     EBX -> "ebx" | ESP -> "esp"  | EBP -> "ebp"  | ESI -> "esi"  | EDI -> "edi"  | R8D -> "r8d" | R9D -> "r9d" | R10D -> "r10d" | R11D -> "r11d" | R12D -> "r12d" | R13D -> "r13d" | R14D -> "r14d" | R15D -> "r15d"
    |      AX -> "ax" |      CX -> "cx" |      DX -> "dx" |      BX -> "bx" |  SP -> "sp"  |  BP -> "bp"  |  SI -> "si"  |  DI -> "di"  | R8W -> "r8w" | R9W -> "r9w" | R10W -> "r10w" | R11W -> "r11w" | R12W -> "r12w" | R13W -> "r13w" | R14W -> "r14w" | R15W -> "r15w"
    | AH -> "ah" | AL -> "al" | CH -> "ch" | CL -> "cl" | DH -> "dh" | DL -> "dl" | BH -> "bh" | BL -> "bl" |  SPL -> "spl" |  BPL -> "bpl" |  SIL -> "sil" |  DIL -> "dil" | R8B -> "r8b" | R9B -> "r9b" | R10B -> "r10b" | R11B -> "r11b" | R12B -> "r12b" | R13B -> "r13b" | R14B -> "r14b" | R15B -> "r15b" *)
    
  
  type token =
    | MneTok of string
    | RegTok of Isa.register
    | RipTok
    | ImmTok of (IsaBasic.immediate * (string option) list)
    | Comma
    | LParen
    | RParen
    | Raw of string

  let imm_var_unset : IsaBasic.imm_var_id = -1


  let rec string_of_imm_token (i: IsaBasic.immediate) (name_list: (string option) list) : string =
    match i, name_list with
    | ImmNum n, None :: [] -> Int64.to_string n
    | ImmLabel v, Some name :: [] -> "(var " ^ (string_of_int v) ^ ": " ^  name ^ ")"
    | ImmBExp (i1, i2), hd :: tl ->
      (string_of_imm_token i1 [hd]) ^ " + " ^ (string_of_imm_token i2 tl)
    | _ -> parse_error "string_of_imm_token" 


  let string_of_token (t: token) : string =
    match t with
    | MneTok s -> "MneTok " ^ s
    | RegTok r -> "RegTok " ^ (string_of_register r)
    | RipTok -> "RipTok"
    (* | ImmTok (ImmNum n, _) -> "ImmTok " ^ (Int64.to_string n)
    | ImmTok (ImmLabel v, name) -> "ImmTok (var " ^ (string_of_int v) ^ ": " ^ Option.get name ^ ")"
    | ImmTok (exp, _) -> "ImmTok (" ^ (Isa.string_of_immediate exp) ^ ")"  *)
    | ImmTok (i, name_list) -> "ImmTok " ^ (string_of_imm_token i name_list)
    | Comma -> "Comma"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | Raw s -> "Raw " ^ s
  
  let is_char_of_name_start (c: char) : bool =
    Char.code c >= 65 && Char.code c <= 90
    || Char.code c >= 97 && Char.code c <= 122
    || c = '_' || c = '.'

  let is_char_of_name (c: char) : bool =
    Char.code c >= 48 && Char.code c <= 57 
    || Char.code c >= 65 && Char.code c <= 90
    || Char.code c >= 97 && Char.code c <= 122
    || c = '_' || c = '.' || c = '@'

  let consume_name (cs: char list) : string * char list =
    let rec go (cs: char list) (acc: char list) =
      match cs with
      | [] -> (List.rev acc |> List.to_seq |> String.of_seq, [])
      | c :: cs ->
        if is_char_of_name c then
          go cs (c :: acc)
        else
          (List.rev acc |> List.to_seq |> String.of_seq, c :: cs)
    in
    if cs = [] then
      lexical_error "consume_name: unexpected end of input"
    else if not (is_char_of_name_start (List.hd cs)) then
      lexical_error ("consume_name: invalid start of name: " ^ (String.concat "" (List.map (String.make 1) cs)))
    else
      go cs []

  let consume_int (cs: char list) : int64 * char list =
    let rec go (cs: char list) (acc: char list) =
      match cs with
      | [] -> (List.rev acc |> List.to_seq |> String.of_seq |> Int64.of_string, [])
      | c :: cs ->
        if c = '-' || c = 'x' || c = 'X'
          || (Char.code c >= 48 && Char.code c <= 57)
          || (Char.code c >= 65 && Char.code c <= 70)
          || (Char.code c >= 97 && Char.code c <= 102)
        then
          go cs (c :: acc)
        else
          (List.rev acc |> List.to_seq |> String.of_seq |> Int64.of_string, c :: cs)
    in
    if cs = [] then
      lexical_error "consume_int: unexpected end of input"
    else
      go cs []

  let rec consume_immediate (cs: char list) : IsaBasic.immediate * (string option) list * char list =
    let exp, name_list, remained_cs =
    begin if cs = [] then
      lexical_error "consume_immediate: unexpected end of input"
    else if is_char_of_name_start (List.hd cs) then (
      let (name, cs) = consume_name cs in
      (Isa.ImmLabel imm_var_unset, [ Some name ], cs)
    )
    else (
      let (num, cs) = consume_int cs in
      (Isa.ImmNum num, [ None ], cs)
    ) end 
    in
    match remained_cs with
    | '+' :: remained_cs_tl ->
      let other_exp, other_name_list, other_remained_cs = consume_immediate remained_cs_tl in
      (Isa.ImmBExp (exp, other_exp), name_list @ other_name_list, other_remained_cs)
    | '-' :: _ ->
      let other_exp, other_name_list, other_remained_cs = consume_immediate remained_cs in
      (Isa.ImmBExp (exp, other_exp), name_list @ other_name_list, other_remained_cs)
    | _ -> (exp, name_list, remained_cs)


  let expect (t: token) (ts: token list) : token list =
    match ts with
    | [] -> parse_error "expect: unexpected end of input"
    | t' :: ts ->
      if t = t' then ts
      else parse_error ("expect: expecting " ^ (string_of_token t) ^ ", found " ^ (string_of_token t'))
    
  let consume_lparen (ts: token list) : token list =
    match ts with
    | LParen :: ts -> ts
    | _ -> parse_error "consume_lparen"
  
  let consume_rparen (ts: token list) : token list =
    match ts with
    | RParen :: ts -> ts
    | _ -> parse_error "consume_rparen"
  
  let consume_comma (ts: token list) : token list =
    match ts with
    | Comma :: ts -> ts
    | _ -> parse_error "consume_comma"

  let is_mne_compiler_annotation (mne_str: string) =
    String.sub mne_str 0 1 = "." && mne_str.[(String.length mne_str) - 1] != ':'
  
  let tokenize_line (line: string) : token list =
    let rec go (cs: char list) (acc: token list) =
      match cs with
      | [] -> List.rev acc
      | ' ' :: cs | '\t' :: cs -> go cs acc
      | ',' :: cs -> go cs (Comma :: acc)
      | '(' :: cs -> go cs (LParen :: acc)
      | ')' :: cs -> go cs (RParen :: acc)
      | '%' :: cs ->
          let (name, cs) = consume_name cs in
          begin match parse_register name with
          | Some r -> go cs (RegTok r :: acc)
          | None -> go cs (RipTok :: acc)
          end
          (* go cs (RegTok (parse_register name) :: acc) *)
      | '$' :: cs ->
          go cs acc
      | _ ->
          let (imm, name, cs) = consume_immediate cs in
          go cs (ImmTok (imm, name) :: acc)
      
    in
    let cs = line |> String.to_seq |> List.of_seq in
    let (mne_str, cs) = consume_name cs in
    if is_mne_compiler_annotation mne_str then
      [Raw line]
    else
      go cs [MneTok mne_str]

  let imm_to_scale (imm: IsaBasic.immediate) : Isa.scale =
    match imm with
    | ImmNum 1L -> Scale1
    | ImmNum 2L -> Scale2
    | ImmNum 4L -> Scale4
    | ImmNum 8L -> Scale8
    | _ -> parse_error "imm_to_scale"

  let parse_memory_operand (ts: token list) : Isa.operand * token list =
    match ts with
    | LParen :: RegTok base :: RParen :: ts -> (* (base) *)
      (MemOp (None, Some base, None, None), ts)
    | LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts -> (* (base, index) *)
      (MemOp (None, Some base, Some index, None), ts)
    | LParen :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* (, index, scale) *)
      (MemOp (None, None, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: RParen :: ts -> (* disp(base) *)
      (MemOp (Some disp, Some base, None, None), ts)
    | LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* (base, index, scale) *)
      (MemOp (None, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts ->  (* disp(base, index) *)
      (MemOp (Some disp, Some base, Some index, None), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(base, index, scale) *)
      (MemOp (Some disp, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(, index, scale) *)
      (MemOp (Some disp, None, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: Comma :: RegTok index :: RParen :: ts ->  (* disp(, index) *)
      (MemOp (Some disp, None, Some index, Some Scale1), ts)
    | ImmTok (disp, _) :: LParen :: RipTok :: RParen :: ts -> (* disp(rip) *)
      (MemOp (Some disp, None, None, None), ts)
    | _ -> parse_error ("parse_memory_operand: " ^ (String.concat ", " (List.map string_of_token ts)))

  let parse_operand (ts: token list) : Isa.operand * token list =
    match ts with
    | RegTok reg :: ts -> (RegOp reg, ts)
    | ImmTok _ :: LParen :: _ -> parse_memory_operand ts
    | ImmTok (imm, _) :: ts -> (ImmOp imm, ts)
    | LParen :: _ -> parse_memory_operand ts
    | _ -> parse_error "parse_operand"
  
  let parse_operands (ts: token list) : Isa.operand list =
    let rec go ts acc =
      match ts with
      | [] -> List.rev acc
      | Comma :: ts -> go ts acc
      | _ ->
        let (opr, ts) = parse_operand ts in
        go ts (opr :: acc)
    in
    go ts []

  let convert_imm_to_label (ts: token list) : Isa.operand =
    match ts with
    | ImmTok (ImmLabel _, name) :: [] -> 
      begin match name with
      | Some l :: [] -> LabelOp l
      | None :: [] -> parse_error "convert_imm_to_label no label"
      | _ -> parse_error "convert_imm_to_label too many labels"
      end
    | _ -> parse_error "convert_imm_to_label"

  let split_opcode_size (opcode_list: string list) (mnemonic: string) (dirty_op_size: int64 option) : string * (int64 option) =
    let suffix_to_size (opcode: string) (s: string) : int64 option =
      match s with
      | "" -> dirty_op_size
      | "b" -> Some 1L
      | "w" -> Some 2L
      | "l" -> Some 4L
      | "d" -> Some 4L
      | "q" -> Some 8L
      (* Very dirty orzzz *)
      | "ups" | "aps" | "dqu" | "dqa" | "dq" -> dirty_op_size
      | _ -> 
        begin match opcode with
        | "punpck"
        | "packus" -> dirty_op_size
        | _ -> parse_error (Printf.sprintf "split_opcode_size: invalid size %s mnemonic %s" s mnemonic)
        end
    in
    let helper (opcode: string) : (string * (int64 option)) option =
      if String.starts_with mnemonic ~prefix:opcode then
        Some (opcode, suffix_to_size opcode (String.sub mnemonic (String.length opcode) (String.length mnemonic - String.length opcode)))
      else None
    in
    match List.find_map helper opcode_list with
    | Some result -> result
    | None -> parse_error ("split_opcode_size cannot parse " ^ mnemonic)

  let get_operand_size 
      (mnemonic: string) 
      (operands: Isa.operand list) : string * ((Isa.operand * (int64 option)) list) * (int64 option) =
    let dirty_op_size = Isa.get_reg_op_size operands in
    let list_merge_helper = List.map2 (fun x y -> (x, y)) in
    match mnemonic with
    (* Handle special cases where operand sizes are not the same first *)
    | "movsbq" -> "mov", (list_merge_helper operands [Some 1L; Some 8L]), None
    | "movswq" -> "mov", (list_merge_helper operands [Some 2L; Some 8L]), None
    | "movslq" -> "mov", (list_merge_helper operands [Some 4L; Some 8L]), None
    | "movsbl" -> "mov", (list_merge_helper operands [Some 1L; Some 4L]), None
    | "movswl" -> "mov", (list_merge_helper operands [Some 2L; Some 4L]), None
    | "movsbw" -> "mov", (list_merge_helper operands [Some 1L; Some 2L]), None
    | "cltq" -> "mov", (list_merge_helper [RegOp EAX; RegOp RAX] [Some 4L; Some 8L]), None
    | "movzbq" -> "movz", (list_merge_helper operands [Some 1L; Some 8L]), None
    | "movzwq" -> "movz", (list_merge_helper operands [Some 2L; Some 8L]), None
    | "movzlq" -> "movz", (list_merge_helper operands [Some 4L; Some 8L]), None
    | "movzbl" -> "movz", (list_merge_helper operands [Some 1L; Some 4L]), None
    | "movzwl" -> "movz", (list_merge_helper operands [Some 2L; Some 4L]), None
    | "movzbw" -> "movz", (list_merge_helper operands [Some 1L; Some 2L]), None
    | _ ->
      let opcode, op_size = split_opcode_size Isa.common_opcode_list mnemonic dirty_op_size in
      opcode, (List.map (fun x -> (x, op_size)) operands), op_size

  let get_default_operand (op_size: int64 option) : Isa.operand * (int64 option) =
    match op_size with
    | Some 1L -> RegOp AL, op_size
    | Some 2L -> RegOp AX, op_size
    | Some 4L -> RegOp EAX, op_size
    | Some 8L -> RegOp RAX, op_size
    | Some x -> parse_error ("cannot get default op with size " ^ (Int64.to_string x))
    | None -> parse_error "cannot get default op when size is unknown"

  let src (opr_size: Isa.operand * int64 option) : Isa.operand =
    match opr_size with
    | MemOp (disp, base, index, scale), Some size -> LdOp (disp, base, index, scale, size, MemAnno.make_empty ())
    | MemOp _, None -> parse_error "no size for mem op"
    | StOp _, _ -> parse_error "src"
    | LdOp _, _ -> parse_error "LdOp not expected"
    (* | LdOp (disp, base, index, scale, _, mem_anno), Some size -> LdOp (disp, base, index, scale, size, mem_anno) *)
    (* | LdOp _, None -> parse_error "no size for ld op" *)
    (* | RegOp r -> if Isa.get_reg_size r = size then opr else parse_error "src reg size does not match opcode size" *)
    | opr, _ -> opr

  let dst (opr_size: Isa.operand * int64 option) : Isa.operand =
    match opr_size with
    | MemOp (disp, base, index, scale), Some size -> StOp (disp, base, index, scale, size, MemAnno.make_empty ())
    | MemOp _, None -> parse_error "no size for mem op"
    | LdOp _, _ -> parse_error "dst"
    | StOp _, _ -> parse_error "StOp not expected"
    (* | StOp (disp, base, index, scale, _, taint), Some size -> StOp (disp, base, index, scale, size, taint) *)
    (* | StOp _, None -> parse_error "no size for st op" *)
    (* | RegOp r -> if Isa.get_reg_size r = size then opr else parse_error "dst reg size does not match opcode size" *)
    | opr, _ -> opr

  let get_binst_with_one_operand (op: Isa.bop) (opr: Isa.operand * (int64 option)) (default_size: int64 option) : Isa.instruction =
    match op with
    | Sal | Sar | Shr | Shl | Rol | Ror -> BInst (op, dst opr, src opr, src (ImmOp (ImmNum 1L), default_size))
    | Mul | Imul ->
      let default_opr : Isa.operand * (int64 option) = begin match default_size with
      | Some _ -> parse_error "<TODO> not implemented yet"
      (* | Some 1L -> RegOp AL, default_size
      | Some 2L -> RegOp AX, default_size
      | Some 4L -> RegOp EAX, default_size
      | Some 8L -> RegOp RAX, default_size
      | Some x -> parse_error ("cannot get default op with size " ^ (Int64.to_string x)) *)
      | None -> parse_error "cannot get default op when size is unknown"
      end in
      BInst (op, dst default_opr, src default_opr, src opr)
    | _ -> parse_error (Printf.sprintf "Cannot get default op for op %s" (Sexplib.Sexp.to_string (Isa.sexp_of_bop op)))

  let get_binst (op: Isa.bop) (operands: (Isa.operand * (int64 option)) list) (default_size: int64 option) : Isa.instruction =
    match operands with
    | [opr1] -> 
      get_binst_with_one_operand op opr1 default_size
      (* let default_opr = get_default_operand default_size in
      BInst (op, dst default_opr, src default_opr, src opr1) *)
    | [opr1; opr2] ->
      BInst (op, dst opr2, src opr2, src opr1)
    | [opr0; opr1; opr2] ->
      BInst (op, dst opr2, src opr1, src opr0)
    | _ -> parse_error ("get_binst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let get_uinst (op: Isa.uop) (operands: (Isa.operand * (int64 option)) list) : Isa.instruction =
    match op, operands with
    | Lea, [(opr1, _); opr2] -> UInst (op, dst opr2, opr1)
    | _, [opr1; opr2] -> UInst (op, dst opr2, src opr1)
    | _, [opr] -> UInst (op, dst opr, src opr)
    | _ -> parse_error ("get_uinst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let get_tinst (op: Isa.top) (operands: (Isa.operand * (int64 option)) list) : Isa.instruction =
    let num_operands = List.length operands in
    let rev_operands = List.rev operands in
    if num_operands >= 3 then
      TInst (op, dst (List.hd rev_operands), List.map src rev_operands)
    else
      parse_error ("get_tinst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let parse_tokens (imm_var_map: Isa.imm_var_map) (ts: token list) : (Isa.imm_var_map * (IsaBasic.imm_var_id list)) * (Isa.instruction * string (* mnemonic *)) =
    let anno_opt = match ts with
    | Raw raw_line :: [] ->
      Some (imm_var_map, (Isa.Annotation raw_line, raw_line)) (* mnemonic is the whole line *)
    | _ -> None
    in
    if Option.is_some anno_opt then begin
      let imm_var_map, (inst, mnemonic) = Option.get anno_opt in
      (imm_var_map, []), (inst, mnemonic)
    end else

    let (mnemonic, ts) = match ts with
    | MneTok mnemonic :: ts -> (mnemonic, ts)
    | _ -> parse_error ("parse_tokens: unexpected end of input: " ^ (String.concat ", " (List.map string_of_token ts)))
    in
    let imm_var_map, symbols, operands = if Isa.inst_referring_label mnemonic
      then imm_var_map, [], [convert_imm_to_label ts]
      else begin 
        let rec fill_id_helper 
            (info: Isa.imm_var_map * (IsaBasic.imm_var_id list)) (imm_name: IsaBasic.immediate * (string option) list) :
            (IsaBasic.imm_var_map * (IsaBasic.imm_var_id list)) * IsaBasic.immediate =
          let imm_var_map, symbols = info in
          match imm_name with
          | ImmNum n, None :: [] -> ((imm_var_map, symbols), ImmNum n)
          | ImmLabel v, (Some name) :: [] ->
            if v <> imm_var_unset then ((imm_var_map, v :: symbols), ImmLabel v)
            else begin
              if Isa.StrM.mem name imm_var_map
              then begin
                let id = Isa.StrM.find name imm_var_map in
                ((imm_var_map, id :: symbols), ImmLabel id)
              end else begin
                (* let id = CodeType.stack_base_id + 1 + (Isa.StrM.cardinal imm_var_map) in *)(* id starts from stack_base_id + 1 *)
                let id = imm_var_unset - 1 - (Isa.StrM.cardinal imm_var_map) in (* id starts from imm_var_unset, going negative!!! *)
                Printf.printf "new imm_var: %s %d\n" name id;
                ((Isa.StrM.add name id imm_var_map, id :: symbols), ImmLabel id)
              end
            end
          | ImmBExp (i1, i2), hd :: tl ->
            let (imm_var_map1, symbols1), new_i1 = fill_id_helper (imm_var_map, symbols) (i1, [ hd ]) in
            let (imm_var_map2, symbols2), new_i2 = fill_id_helper (imm_var_map1, symbols1) (i2, tl) in
            ((imm_var_map2, symbols2), Isa.ImmBExp (new_i1, new_i2))
          | _ -> parse_error "parse_tokens fail to generate imm_var_id"
        in
        let (imm_var_map, symbols), ts = List.fold_left_map (fun imm_var_map token ->
          match token with
          (* TODO!!! *)
          | ImmTok (imm, name_list) ->
            let (new_imm_var_map, symbols), new_imm = fill_id_helper imm_var_map (imm, name_list) in
            ((new_imm_var_map, symbols), ImmTok (new_imm, name_list))
          (* | ImmTok (ImmLabel v, Some name) ->
            if v <> imm_var_unset
            then (imm_var_map, token)
            else begin
              if Isa.StrM.mem name imm_var_map
              then (imm_var_map, ImmTok (ImmLabel (Isa.StrM.find name imm_var_map), Some name))
              else begin
                let id = CodeType.stack_base_id + 1 + (Isa.StrM.cardinal imm_var_map) in (* id starts from stack_base_id + 1 *)
                Printf.printf "new imm_var: %s %d\n" name id;
                (Isa.StrM.add name id imm_var_map, ImmTok (ImmLabel id, Some name))
              end
            end *)
          | _ -> (imm_var_map, token)
        ) (imm_var_map, []) ts
        in
        imm_var_map, symbols, parse_operands ts
      end
    in
    let inst: Isa.instruction = 
      match (mnemonic, operands) with
      | ("rep", [LabelOp label]) ->
        begin match split_opcode_size IsaBasic.rep_opcode_list label None with
        | "movs", Some size ->
          RepMovs (size, MemAnno.make_empty (), MemAnno.make_empty ())
        | "lods", Some size ->
          RepLods (size, MemAnno.make_empty ())
        | "stos", Some size ->
          RepStos (size, MemAnno.make_empty ())
        | _ -> parse_error (Printf.sprintf "Cannot parse rep op for %s" label)
        end
        (* if label = "stosq" then RepStosq 
        else if label = "movsq" then RepMovsq 
        else begin
          Printf.printf "rep %s\n" label;
          parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
        end *)
      | ("jmp", [LabelOp lb]) -> Jmp (lb, None)
      | ("call", [LabelOp lb])
      | ("callq", [LabelOp lb])  -> Call (lb, None)
      (* | ("callq", [x]) -> 
        Printf.printf "Callq %s\n" (Isa.string_of_operand x);
        parse_error "callq" *)
      | (_, [LabelOp lb]) ->
        begin match Isa.op_of_cond_jump mnemonic with
        | Some op -> Jcond (op, lb, None)
        | None -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
        end
      | ("ret", []) | ("retq", []) -> Jmp (Isa.ret_label, None) 
      | ("syscall", []) -> Syscall
      | ("hlt", []) -> Hlt
      | _ -> let opcode, opread_size_list, op_size = get_operand_size mnemonic operands in
        begin match Isa.op_of_binst opcode, Isa.op_of_uinst opcode, Isa.op_of_tinst opcode with
        | Some op, _, _ -> get_binst op opread_size_list op_size
        | None, Some op, _ -> get_uinst op opread_size_list
        | None, None, Some op -> get_tinst op opread_size_list
        | _ ->
          begin match opcode, opread_size_list with
          | "xchg", [opr1; opr2] -> Xchg (dst opr2, dst opr1, src opr2, src opr1)
          | "push", [opr] -> Push ((src opr), (MemAnno.make_empty ()))
          | "pop", [opr] -> Pop ((dst opr), (MemAnno.make_empty ()))
          | "cmp", [opr1; opr2] -> Cmp (src opr2, src opr1)
          | "test", [opr1; opr2] -> Test (src opr2, src opr1)
          | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
          end
        end
        (* begin match Isa.op_of_binst opcode with
        | Some op -> get_binst op opread_size_list op_size
        | None ->
          begin match Isa.op_of_uinst opcode with
          | Some op -> get_uinst op opread_size_list
          | None -> (* TODO: xchg *)
            begin match opcode, opread_size_list with
            | "xchg", [opr1; opr2] -> Xchg (dst opr2, dst opr1, src opr2, src opr1)
            | "push", [opr] -> Push ((src opr), (MemAnno.make_empty ()))
            | "pop", [opr] -> Pop ((dst opr), (MemAnno.make_empty ()))
            | "cmp", [opr1; opr2] -> Cmp (src opr2, src opr1)
            | "test", [opr1; opr2] -> Test (src opr2, src opr1)
            | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
            end
          end
        end *)
    in
    (imm_var_map, symbols), (inst, mnemonic)

  let parse_instr (imm_var_map: Isa.imm_var_map) (line: string) : (Isa.imm_var_map * (IsaBasic.imm_var_id list)) * (Isa.instruction * string) (* mnemonic *) =
    (* print_endline ("Parsing " ^ line); *)
    let tokens = tokenize_line line in
    (* print_endline (String.concat ", " (List.map string_of_token tokens)); *)
    parse_tokens imm_var_map tokens

  let rec find_comment_start (line: string) : int option =
    let res = match String.rindex_opt line '#', String.rindex_opt line '"' with
    | Some x, Some y ->
      if x < y then None else Some x
    | None, Some _ -> None
    | Some x, None -> Some x
    | None, None -> None
    in
    match res with
    | None -> None
    | Some x -> begin
      match find_comment_start (String.sub line 0 x) with
      | None -> Some x
      | Some z -> Some z
      end

  let parse_basic_block (info: (Isa.imm_var_map * (Isa.label option) * (IsaBasic.imm_var_id list))) (lines: string list)
  : (Isa.imm_var_map * (Isa.label option) * (IsaBasic.imm_var_id list)) * Isa.basic_block =
    let imm_var_map, func, symbols = info in
    let label = parse_label (List.hd lines) in
    let func = if Isa.is_label_function_entry label then Some label else func in
    let (imm_var_map, new_symbols), insts_with_mne = List.fold_left_map (
      fun (imm_var_map, symbols) line ->
      let (imm_var_map, new_symbols), result = parse_instr imm_var_map line in
      (imm_var_map, new_symbols @ symbols), result
    ) (imm_var_map, []) (List.tl lines) in
    let insts, mnemonics = List.split insts_with_mne in
    (imm_var_map, func, new_symbols @ symbols), {
      label = label; 
      insts = insts;
      mnemonics = mnemonics;
      orig_asm = List.map (fun x -> Some x) lines;
    }

  let split_on_chars sep_list (s: string) : string list =
    let helper (s: string list) sep : string list =
      List.concat_map (String.split_on_char sep) s
    in
    List.fold_left helper [ s ] sep_list

  let line_processor (trim: bool) (line: string) : string list =
    (* ignore ';' and do not split lines if is annotation (dirty) *)
    let trimmed = String.trim line in
    if String.equal trimmed "" then [] else
    let first_word = match String.index_opt trimmed ' ' with
    | None -> trimmed
    | Some idx -> String.sub trimmed 0 idx
    in
    let splitted =
      if is_mne_compiler_annotation first_word then
        [line]
      else
        (String.split_on_char ';' line)
    in

    List.filter_map (fun (line: string) ->
      (* get rid of comments *)
      let line = match find_comment_start line with
      | Some pos -> String.sub line 0 pos
      | None -> line
      in
      let line = String.trim line in
      if line = "" then None else
      match trim, Isa.is_label line with
      | true, _ -> Some line
      | false, true -> Some line
      | false, false -> Some ("\t" ^ line)
    ) splitted

  let rec split_helper split_here bb acc_bb lines =
    match lines with
    | [] -> List.rev (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
    | line :: lines ->
      if split_here line then
        split_helper split_here [line] (if bb = [] then acc_bb else (List.rev bb) :: acc_bb) lines
      else
        split_helper split_here (line :: bb) acc_bb lines

  let parse_program (source: string) : Isa.prog =
    let lines = source
      |> String.split_on_char '\n'
      |> List.concat_map (fun line -> line_processor true line)
    in

    (* split in function level *)
    let lines_of_funcs = split_helper Isa.is_func_label [] [] lines in
    (* there could be lines before the first func; merge them into the first func *)
    let lines_of_funcs = match (Isa.is_func_label (List.hd lines)), lines_of_funcs with
    | false, fake_func :: (first_func_first_line :: first_func_rest) :: rest_funcs ->
      if Isa.is_label (List.hd fake_func) then
        parse_error "first fake block should not have a label";
      if not (Isa.is_func_label first_func_first_line) then
        parse_error "expecting function label";
      ((first_func_first_line :: fake_func) @ (Isa.fake_bb_sentry_label :: first_func_rest)) :: rest_funcs
    | _, _ -> lines_of_funcs
    in

    (* in each function, split by labels to get BBs *)
    let bbs_of_funcs = List.map (fun lines_of_func ->
      split_helper Isa.is_label [] [] lines_of_func
    ) lines_of_funcs in

    let imm_var_map, func_list =
      let helper 
          (imm_var_map: Isa.imm_var_map) (lines_of_bbs: string list list)
          : Isa.imm_var_map * Isa.func =
        let (imm_var_map, func_name, symbols), bbs =
          List.fold_left_map parse_basic_block (imm_var_map, None, []) lines_of_bbs
        in
        let rev_imm_var_map = Isa.get_rev_imm_var_map imm_var_map in
        let symbols = symbols
          |> List.sort_uniq (fun (x: IsaBasic.imm_var_id) (y: IsaBasic.imm_var_id) -> Int.compare x y)
          |> List.map (fun var_id -> Isa.IntM.find var_id rev_imm_var_map)
        in
        Printf.printf "symbols in %s\n\t" (Option.get func_name);
        List.iter (fun symbol -> Printf.printf "%s " symbol) symbols;
        Printf.printf "\n%!";
        imm_var_map, {
          name = Option.get func_name;
          body = bbs;
          related_gsymbols = symbols;
          subfunctions = [];
        }
      in
      List.fold_left_map helper Isa.StrM.empty bbs_of_funcs
    in

    (* subfunctions extraction *)
    let func_list = List.map(fun (func: Isa.func) ->
      let calls = List.fold_left (fun acc (bb: Isa.basic_block) ->
        let calls = List.filter_map (fun (inst: Isa.instruction) ->
          match inst with
          | Call (subfunc, _) -> Some subfunc
          | _ -> None
        ) bb.insts
        in
        calls @ acc
      ) [] func.body
      in
      let calls = List.sort_uniq (fun x y -> String.compare x y) calls in
      List.iter (fun subfunc -> Printf.printf "%s calls to %s\n" func.name subfunc) calls;
      {func with subfunctions = calls}
    ) func_list in
    (* related gsymbols propagation *)
    let same_func_list (l1: Isa.func list) (l2: Isa.func list) =
      List.fold_left2 (fun acc (f1: Isa.func) (f2: Isa.func) ->
        if not (String.equal f1.name f2.name) then
          parse_error "func list invalid";
        let cmp = (0 = List.compare (fun a b -> String.compare a b) f1.related_gsymbols f2.related_gsymbols) in
        if cmp = false then false else acc
      ) true l1 l2
    in
    let rec symbol_propagate_helper (func_list: Isa.func list) : Isa.func list =
      let old_list = func_list in

      let func_list = List.map (fun (func: Isa.func) ->
        let related_gsymbols = List.fold_left (fun acc (subfunc: Isa.label) ->
          let subfunc_match =
            List.find_opt (fun (func: Isa.func) ->
              func.name = subfunc
            ) old_list
          in
          match subfunc_match with
          | None ->
            Printf.printf "warning: skipping %s in symbol propagation\n" subfunc;
            acc
          | Some subfunc ->
            let missing = List.filter (fun (symbol: Isa.label) ->
              not (List.mem symbol acc)
            ) subfunc.related_gsymbols
            in
            List.iter (fun s -> Printf.printf "adding %s to %s (from %s)\n" s func.name subfunc.name) missing;
            missing @ acc
        ) func.related_gsymbols func.subfunctions in
        {func with related_gsymbols = related_gsymbols }
      ) func_list in

      if same_func_list old_list func_list then
        old_list
      else
        symbol_propagate_helper func_list
    in
    let func_list = symbol_propagate_helper func_list in
    List.iter (fun (func: Isa.func) ->
      Printf.printf "global symbol of %s:\n" func.name;
      List.iter (fun symbol -> Printf.printf "\t%s\n" symbol) func.related_gsymbols;
    ) func_list;

    (* add jmp for two adjacent basic blocks if the previous one does not end with ret *)
    let rec add_jmp_for_adj_bb (bbs: Isa.basic_block list) (acc: Isa.basic_block list) =
      match bbs with
      | [] -> List.rev acc
      | bb :: [] -> List.rev (bb :: acc)
      | bb1 :: bb2 :: bbs ->
          (* now annotations can appear at the end of each BB; we need to find the real last instruction *)
          let last_nonanno_inst = List.find_opt (fun (inst: Isa.instruction) ->
            match inst with
            | Annotation _ -> false
            | _ -> true
          ) (List.rev bb1.insts)
          in

          let bb1 = if List.length bb1.insts > 0 && Option.is_some last_nonanno_inst && Isa.inst_is_uncond_jump (Option.get last_nonanno_inst)
            then bb1
            else {bb1 with insts = bb1.insts @ [Jmp (bb2.label, None)]; mnemonics = bb1.mnemonics @ ["jmp"]; orig_asm = bb1.orig_asm @ [None] }
          in
          add_jmp_for_adj_bb (bb2 :: bbs) (bb1 :: acc)
    in
    let func_list = List.map (
      fun (func: Isa.func) ->
        { func with body = add_jmp_for_adj_bb func.body []}
    ) func_list in
    (* let bbs = add_jmp_for_adj_bb bbs [] in *)
    let get_ret_bb : Isa.basic_block =
      { label = Isa.ret_label; insts = []; mnemonics = []; orig_asm = [None (* "line" of the label *)] } in
    let func_list = List.map (
      fun (func: Isa.func) ->
        { func with body = func.body @ [get_ret_bb] }
    ) func_list
    in
    { funcs = func_list; imm_var_map = imm_var_map }
    (* {bbs = bbs @ [get_ret_bb]; imm_var_map = imm_var_map} *)

  let prog_to_file (filename: string) (prog: Isa.prog) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (Isa.sexp_of_prog prog)

  let prog_from_file (filename: string) : Isa.prog =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    Isa.prog_of_sexp s_exp

end

let parse (source: string) = Parser.parse_program source

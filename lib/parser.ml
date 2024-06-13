open Isa
open Code_type

module Parser = struct

  exception LexicalError of string
  exception ParseError of string

  let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))
  let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

  let parse_label (line: string) : Isa.label =
    let label_name = String.sub line 0 ((String.length line) - 1) in
    label_name

  let parse_register (name: string) : Isa.register =
    match name with
    |     "rax" -> RAX |     "rcx" -> RCX |     "rdx" -> RDX |     "rbx" -> RBX | "rsp" -> RSP  | "rbp" -> RBP  | "rsi" -> RSI  | "rdi" -> RDI  | "r8" -> R8  | "r9" -> R9  | "r10" -> R10  | "r11" -> R11  | "r12" -> R12  | "r13" -> R13  | "r14" -> R14  | "r15" -> R15
    |     "eax" -> EAX |     "ecx" -> ECX |     "edx" -> EDX |     "ebx" -> EBX | "esp" -> ESP  | "ebp" -> EBP  | "esi" -> ESI  | "edi" -> EDI  | "r8d" -> R8D | "r9d" -> R9D | "r10d" -> R10D | "r11d" -> R11D | "r12d" -> R12D | "r13d" -> R13D | "r14d" -> R14D | "r15d" -> R15D
    |      "ax" -> AX |      "cx" -> CX |      "dx" -> DX |      "bx" -> BX |  "sp" -> SP  |  "bp" -> BP  |  "si" -> SI  |  "di" -> DI  | "r8w" -> R8W | "r9w" -> R9W | "r10w" -> R10W | "r11w" -> R11W | "r12w" -> R12W | "r13w" -> R13W | "r14w" -> R14W | "r15w" -> R15W
    | "ah" -> AH | "al" -> AL | "ch" -> CH | "cl" -> CL | "dh" -> DH | "dl" -> DL | "bh" -> BH | "bl" -> BL |  "spl" -> SPL |  "bpl" -> BPL |  "sil" -> SIL |  "dil" -> DIL | "r8b" -> R8B | "r9b" -> R9B | "r10b" -> R10B | "r11b" -> R11B | "r12b" -> R12B | "r13b" -> R13B | "r14b" -> R14B | "r15b" -> R15B
    | _ -> parse_error ("parse_register: invalid register name " ^ name)

  let string_of_register (r: Isa.register) : string =
    match r with
    |     RAX -> "rax" |     RCX -> "rcx" |     RDX -> "rdx" |     RBX -> "rbx" | RSP -> "rsp"  | RBP -> "rbp"  | RSI -> "rsi"  | RDI -> "rdi"  | R8 -> "r8"  | R9 -> "r9"  | R10 -> "r10"  | R11 -> "r11"  | R12 -> "r12"  | R13 -> "r13"  | R14 -> "r14"  | R15 -> "r15"
    |     EAX -> "eax" |     ECX -> "ecx" |     EDX -> "edx" |     EBX -> "ebx" | ESP -> "esp"  | EBP -> "ebp"  | ESI -> "esi"  | EDI -> "edi"  | R8D -> "r8d" | R9D -> "r9d" | R10D -> "r10d" | R11D -> "r11d" | R12D -> "r12d" | R13D -> "r13d" | R14D -> "r14d" | R15D -> "r15d"
    |      AX -> "ax" |      CX -> "cx" |      DX -> "dx" |      BX -> "bx" |  SP -> "sp"  |  BP -> "bp"  |  SI -> "si"  |  DI -> "di"  | R8W -> "r8w" | R9W -> "r9w" | R10W -> "r10w" | R11W -> "r11w" | R12W -> "r12w" | R13W -> "r13w" | R14W -> "r14w" | R15W -> "r15w"
    | AH -> "ah" | AL -> "al" | CH -> "ch" | CL -> "cl" | DH -> "dh" | DL -> "dl" | BH -> "bh" | BL -> "bl" |  SPL -> "spl" |  BPL -> "bpl" |  SIL -> "sil" |  DIL -> "dil" | R8B -> "r8b" | R9B -> "r9b" | R10B -> "r10b" | R11B -> "r11b" | R12B -> "r12b" | R13B -> "r13b" | R14B -> "r14b" | R15B -> "r15b"

  
  type token =
    | MneTok of string
    | RegTok of Isa.register
    | ImmTok of (Isa.immediate * (string option) list)
    | Comma
    | LParen
    | RParen

  let imm_var_unset : Isa.imm_var_id = -1


  let rec string_of_imm_token (i: Isa.immediate) (name_list: (string option) list) : string =
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
    (* | ImmTok (ImmNum n, _) -> "ImmTok " ^ (Int64.to_string n)
    | ImmTok (ImmLabel v, name) -> "ImmTok (var " ^ (string_of_int v) ^ ": " ^ Option.get name ^ ")"
    | ImmTok (exp, _) -> "ImmTok (" ^ (Isa.string_of_immediate exp) ^ ")"  *)
    | ImmTok (i, name_list) -> "ImmTok " ^ (string_of_imm_token i name_list)
    | Comma -> "Comma"
    | LParen -> "LParen"
    | RParen -> "RParen"
  
  let is_char_of_name_start (c: char) : bool =
    Char.code c >= 65 && Char.code c <= 90
    || Char.code c >= 97 && Char.code c <= 122
    || c = '_' || c = '.'

  let is_char_of_name (c: char) : bool =
    Char.code c >= 48 && Char.code c <= 57 
    || Char.code c >= 65 && Char.code c <= 90
    || Char.code c >= 97 && Char.code c <= 122
    || c = '_' || c = '.'

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

  let rec consume_immediate (cs: char list) : Isa.immediate * (string option) list * char list =
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
          go cs (RegTok (parse_register name) :: acc)
      | '$' :: cs ->
          go cs acc
      | _ ->
          let (imm, name, cs) = consume_immediate cs in
          go cs (ImmTok (imm, name) :: acc)
      
    in
    let cs = line |> String.to_seq |> List.of_seq in
    let (mne_str, cs) = consume_name cs in 
    go cs [MneTok mne_str]

  let imm_to_scale (imm: Isa.immediate) : Isa.scale =
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
    | LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts -> (* (base + index) *)
      (MemOp (None, Some base, Some index, None), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: RParen :: ts -> (* disp(base) *)
      (MemOp (Some disp, Some base, None, None), ts)
    | LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* (base, index, scale) *)
      (MemOp (None, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts ->  (* disp(base, index) *)
      (MemOp (Some disp, Some base, Some index, None), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(base, index, scale) *)
      (MemOp (Some disp, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(index, scale) *)
      (MemOp (Some disp, None, Some index, Some (imm_to_scale imm)), ts)
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

  let parse_tokens (imm_var_map: Isa.imm_var_map) (ts: token list) : Isa.imm_var_map * Isa.instruction =
    let src (opr_size: Isa.operand * int64 option) : Isa.operand =
      match opr_size with
      | MemOp (disp, base, index, scale), Some size -> LdOp (disp, base, index, scale, size)
      | MemOp _, None -> parse_error "no size for mem op"
      | StOp _, _ -> parse_error "src"
      | LdOp (disp, base, index, scale, _), Some size -> LdOp (disp, base, index, scale, size)
      | LdOp _, None -> parse_error "no size for ld op"
      (* | RegOp r -> if Isa.get_reg_size r = size then opr else parse_error "src reg size does not match opcode size" *)
      | opr, _ -> opr
    in
    let dst (opr_size: Isa.operand * int64 option) : Isa.operand =
      match opr_size with
      | MemOp (disp, base, index, scale), Some size -> StOp (disp, base, index, scale, size)
      | MemOp _, None -> parse_error "no size for mem op"
      | LdOp _, _ -> parse_error "dst"
      | StOp (disp, base, index, scale, _), Some size -> StOp (disp, base, index, scale, size)
      | StOp _, None -> parse_error "no size for st op"
      (* | RegOp r -> if Isa.get_reg_size r = size then opr else parse_error "dst reg size does not match opcode size" *)
      | opr, _ -> opr
    in
    let (mnemonic, ts) = match ts with
    | MneTok mnemonic :: ts -> (mnemonic, ts)
    | _ -> parse_error ("parse_tokens: unexpected end of input: " ^ (String.concat ", " (List.map string_of_token ts)))
    in
    let imm_var_map, operands = if Isa.inst_referring_label mnemonic
      then imm_var_map, [convert_imm_to_label ts]
      else begin 
        let rec fill_id_helper 
            (imm_var_map: Isa.imm_var_map) (imm_name: Isa.immediate * (string option) list) :
            Isa.imm_var_map * Isa.immediate =
          match imm_name with
          | ImmNum n, None :: [] -> (imm_var_map, ImmNum n)
          | ImmLabel v, (Some name) :: [] ->
            if v <> imm_var_unset then (imm_var_map, ImmLabel v)
            else begin
              if Isa.StrM.mem name imm_var_map
              then (imm_var_map, ImmLabel (Isa.StrM.find name imm_var_map))
              else begin
                let id = CodeType.stack_base_id + 1 + (Isa.StrM.cardinal imm_var_map) in (* id starts from stack_base_id + 1 *)
                Printf.printf "new imm_var: %s %d\n" name id;
                (Isa.StrM.add name id imm_var_map, ImmLabel id)
              end
            end
          | ImmBExp (i1, i2), hd :: tl ->
            let imm_var_map1, new_i1 = fill_id_helper imm_var_map (i1, [ hd ]) in
            let imm_var_map2, new_i2 = fill_id_helper imm_var_map1 (i2, tl) in
            (imm_var_map2, Isa.ImmBExp (new_i1, new_i2))
          | _ -> parse_error "parse_tokens fail to generate imm_var_id"
        in
        let imm_var_map, ts = List.fold_left_map (fun imm_var_map token ->
          match token with
          (* TODO!!! *)
          | ImmTok (imm, name_list) ->
            let new_imm_var_map, new_imm = fill_id_helper imm_var_map (imm, name_list) in
            (new_imm_var_map, ImmTok (new_imm, name_list))
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
        ) imm_var_map ts
        in
        imm_var_map, parse_operands ts
      end
    in
    let dirty_op_size = Isa.get_reg_op_size operands in
    let inst: Isa.instruction = match (mnemonic, operands) with
      | ("mov", [opr1; opr2]) -> Mov (dst(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("movb", [opr1; opr2]) -> Mov (dst(opr2, Some 1L), src(opr1, Some 1L))
      | ("movw", [opr1; opr2]) -> Mov (dst(opr2, Some 2L), src(opr1, Some 2L))
      | ("movl", [opr1; opr2]) -> Mov (dst(opr2, Some 4L), src(opr1, Some 4L))
      | ("movq", [opr1; opr2]) 
      | ("movabsq", [opr1; opr2]) -> Mov (dst(opr2, Some 8L), src(opr1, Some 8L))
      | ("movs", [opr1; opr2]) -> MovS (dst(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("movslq", [opr1; opr2]) -> MovS (dst(opr2, Some 8L), src(opr1, Some 4L))
      | ("movz", [opr1; opr2]) -> MovZ (dst(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("movzbl", [opr1; opr2]) -> MovZ  (dst(opr2, Some 4L), src(opr1, Some 1L))
      | ("lea", [opr1; opr2]) -> Lea (dst(opr2, dirty_op_size), opr1)
      | ("leal", [opr1; opr2]) -> Lea (dst(opr2, Some 4L), opr1)
      | ("leaq", [opr1; opr2]) -> Lea (dst(opr2, Some 8L), opr1) (* the memory operand in LEA is not converted to ld/st *)
      | ("add", [opr1; opr2]) -> Add (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("addq", [opr1; opr2]) -> Add (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("sub", [opr1; opr2]) -> Sub (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("subq", [opr1; opr2]) -> Sub (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L)) (* note that the minuend is opr2 *)
      | ("sal", [opr1; opr2]) -> Sal (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("sall", [opr1; opr2]) -> Sal (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("salq", [opr1; opr2]) -> Sal (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("sar", [opr1; opr2]) -> Sar (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("sarl", [opr1; opr2]) -> Sar (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("sarq", [opr1; opr2]) -> Sar (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("shrl", [opr1; opr2])   -> Shr (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("shrq", [opr1; opr2])   -> Shr (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("xor", [opr1; opr2]) -> Xor (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("xorb", [opr1; opr2]) -> Xor (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("xorl", [opr1; opr2]) -> Xor (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("xorq", [opr1; opr2]) -> Xor (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("not", [opr]) -> Not (dst(opr, dirty_op_size), src(opr, dirty_op_size))
      | ("notq", [opr]) -> Not (dst(opr, Some 8L), src(opr, Some 8L))
      | ("and", [opr1; opr2]) -> And (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("andb", [opr1; opr2]) -> And (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("andw", [opr1; opr2]) -> And (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("andl", [opr1; opr2]) -> And (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("andq", [opr1; opr2]) -> And (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("cmp", [opr1; opr2]) -> Cmp (src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("cmpb", [opr1; opr2]) -> Cmp (src(opr2, Some 1L), src(opr1, Some 1L))
      | ("cmpw", [opr1; opr2]) -> Cmp (src(opr2, Some 2L), src(opr1, Some 2L))
      | ("cmpl", [opr1; opr2]) -> Cmp (src(opr2, Some 4L), src(opr1, Some 4L))
      | ("cmpq", [opr1; opr2]) -> Cmp (src(opr2, Some 8L), src(opr1, Some 8L))
      | ("testb", [opr1; opr2]) -> Test (src(opr2, Some 1L), src(opr1, Some 1L))
      | ("testw", [opr1; opr2]) -> Test (src(opr2, Some 2L), src(opr1, Some 2L))
      | ("testl", [opr1; opr2]) -> Test (src(opr2, Some 4L), src(opr1, Some 4L))
      | ("testq", [opr1; opr2]) -> Test (src(opr2, Some 8L), src(opr1, Some 8L))
      | ("jmp", [LabelOp lb]) -> Jmp (lb)
      | ("je", [LabelOp lb]) -> Jcond (lb, JE)
      | ("jne", [LabelOp lb]) -> Jcond (lb, JNe)
      | ("jbe", [LabelOp lb]) -> Jcond (lb, JLe) (* TODO: Note that this is not correct, just for quick test only *)
      | ("jb", [LabelOp lb]) -> Jcond (lb, JL) (* TODO: Note that this is not correct, just for quick test only *)
      | ("call", [LabelOp lb]) -> Call (lb)
      | ("ret", []) -> Jmp (Isa.ret_label)
      | ("pushq", [opr]) -> Push (src(opr, Some 8L))
      | ("popq", [opr]) -> Pop (src(opr, Some 8L))
      | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
    in
    imm_var_map, inst

  let parse_instr (imm_var_map: Isa.imm_var_map) (line: string) : Isa.imm_var_map * Isa.instruction =
    (* print_endline ("Parsing " ^ line); *)
    let tokens = tokenize_line line in
    (* print_endline (String.concat ", " (List.map string_of_token tokens)); *)
    parse_tokens imm_var_map tokens

  let parse_basic_block (info: (Isa.imm_var_map * (Isa.label option))) (lines: string list)
  : (Isa.imm_var_map * (Isa.label option)) * Isa.basic_block =
    let imm_var_map, func = info in
    let label = parse_label (List.hd lines) in
    let func = if Isa.is_label_function_entry label then Some label else func in
    let imm_var_map, insts = List.fold_left_map (
      fun imm_var_map line -> parse_instr imm_var_map line
    ) imm_var_map (List.tl lines) in
    (imm_var_map, func), 
    {
      label = label; 
      func = Option.get func;
      rsp_offset = 0L;
      insts = insts
      }

  let parse_program (source: string) : Isa.program =
    let lines = source
      (* Split lines *)
      |> String.split_on_char '\n'
      (* Remove empty lines and comment lines *)
      |> List.filter_map (fun line ->
           let first_sharp = String.index_opt line '#' in
           let line = if first_sharp <> None then
             String.sub line 0 (Option.get first_sharp)
           else
             line
           in
           let line = String.trim line in
           if line = "" then None
           else Some(line)
         )
    in
    (* Split lines into basic blocks *)
    let rec bb_spliter lines bb acc_bb =
      match lines with
      | [] -> List.rev (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
      | line :: lines ->
          if line.[(String.length line) - 1] <> ':'
          then bb_spliter lines (line :: bb) acc_bb
          else bb_spliter lines [line] (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
    in
    let bbs = bb_spliter lines [] [] in
    let (imm_var_map, func), bbs = List.fold_left_map (
      fun info bb -> parse_basic_block info bb
    ) (Isa.StrM.empty, None) bbs in

    (* add jmp for two adjacent basic blocks if the previous one does not end with ret *)
    let rec add_jmp_for_adj_bb (bbs: Isa.basic_block list) (acc: Isa.basic_block list) =
      match bbs with
      | [] -> List.rev acc
      | bb :: [] -> List.rev (bb :: acc)
      | bb1 :: bb2 :: bbs ->
          let bb1 = if List.length bb1.insts > 0 && Isa.inst_is_uncond_jump (List.hd (List.rev bb1.insts))
            then bb1
            else {bb1 with insts = bb1.insts @ [Jmp bb2.label]}
          in
          add_jmp_for_adj_bb (bb2 :: bbs) (bb1 :: acc)
    in
    let bbs = add_jmp_for_adj_bb bbs [] in
    let get_ret_bb : Isa.basic_block =
      { label = Isa.ret_label; func = Option.get func; rsp_offset = 0L; insts = [] } in
    {bbs = bbs @ [get_ret_bb]; imm_var_map = imm_var_map}

end

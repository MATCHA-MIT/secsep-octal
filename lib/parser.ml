open Isa

module Parser = struct

  exception LexicalError of string
  exception ParseError of string

  let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))
  let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

  let parse_label (line: string) : Isa.label =
    let label_name = String.sub line 0 ((String.length line) - 1) in
    label_name

  let parse_register (name: string) : Isa.register option =
    match name with
    |     "rax" -> Some RAX |     "rcx" -> Some RCX |     "rdx" -> Some RDX |     "rbx" -> Some RBX | "rsp" -> Some RSP  | "rbp" -> Some RBP  | "rsi" -> Some RSI  | "rdi" -> Some RDI  | "r8" -> Some R8  | "r9" -> Some R9  | "r10" -> Some R10  | "r11" -> Some R11  | "r12" -> Some R12  | "r13" -> Some R13  | "r14" -> Some R14  | "r15" -> Some R15
    |     "eax" -> Some EAX |     "ecx" -> Some ECX |     "edx" -> Some EDX |     "ebx" -> Some EBX | "esp" -> Some ESP  | "ebp" -> Some EBP  | "esi" -> Some ESI  | "edi" -> Some EDI  | "r8d" -> Some R8D | "r9d" -> Some R9D | "r10d" -> Some R10D | "r11d" -> Some R11D | "r12d" -> Some R12D | "r13d" -> Some R13D | "r14d" -> Some R14D | "r15d" -> Some R15D
    |      "ax" -> Some AX |      "cx" -> Some CX |      "dx" -> Some DX |      "bx" -> Some BX |  "sp" -> Some SP  |  "bp" -> Some BP  |  "si" -> Some SI  |  "di" -> Some DI  | "r8w" -> Some R8W | "r9w" -> Some R9W | "r10w" -> Some R10W | "r11w" -> Some R11W | "r12w" -> Some R12W | "r13w" -> Some R13W | "r14w" -> Some R14W | "r15w" -> Some R15W
    | "ah" -> Some AH | "al" -> Some AL | "ch" -> Some CH | "cl" -> Some CL | "dh" -> Some DH | "dl" -> Some DL | "bh" -> Some BH | "bl" -> Some BL |  "spl" -> Some SPL |  "bpl" -> Some BPL |  "sil" -> Some SIL |  "dil" -> Some DIL | "r8b" -> Some R8B | "r9b" -> Some R9B | "r10b" -> Some R10B | "r11b" -> Some R11B | "r12b" -> Some R12B | "r13b" -> Some R13B | "r14b" -> Some R14B | "r15b" -> Some R15B
    | "rip" -> None (* Dirty support for rip *)
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
    | RipTok
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
    | RipTok -> "RipTok"
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
    | ImmTok (disp, _) :: LParen :: RipTok :: RParen :: ts -> (* disp(rip) *)
      (ImmOp disp, ts)
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
                (* let id = CodeType.stack_base_id + 1 + (Isa.StrM.cardinal imm_var_map) in *)(* id starts from stack_base_id + 1 *)
                let id = imm_var_unset - 1 - (Isa.StrM.cardinal imm_var_map) in (* id starts from imm_var_unset, going negative!!! *)
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
      | ("movsbq", [opr1; opr2]) -> MovS (dst(opr2, Some 8L), src(opr1, Some 1L))
      | ("movswq", [opr1; opr2]) -> MovS (dst(opr2, Some 8L), src(opr1, Some 2L))
      | ("movslq", [opr1; opr2]) -> MovS (dst(opr2, Some 8L), src(opr1, Some 4L))
      | ("cltq", []) -> MovS (dst(RegOp RAX, Some 8L), src(RegOp EAX, Some 4L))
      | ("movz", [opr1; opr2]) -> MovZ (dst(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("movzbl", [opr1; opr2]) -> MovZ  (dst(opr2, Some 4L), src(opr1, Some 1L))
      | ("xchg", [opr1; opr2]) -> Xchg (dst(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("xchgb", [opr1; opr2]) -> Xchg (dst(opr2, Some 1L), src(opr1, Some 1L))
      | ("xchgw", [opr1; opr2]) -> Xchg (dst(opr2, Some 2L), src(opr1, Some 2L))
      | ("xchgl", [opr1; opr2]) -> Xchg (dst(opr2, Some 4L), src(opr1, Some 4L))
      | ("xchgq", [opr1; opr2]) -> Xchg (dst(opr2, Some 8L), src(opr1, Some 8L))
      | ("lea", [opr1; opr2]) -> Lea (dst(opr2, dirty_op_size), opr1)
      | ("leab", [opr1; opr2]) -> Lea (dst(opr2, Some 1L), opr1)
      | ("leaw", [opr1; opr2]) -> Lea (dst(opr2, Some 2L), opr1)
      | ("leal", [opr1; opr2]) -> Lea (dst(opr2, Some 4L), opr1)
      | ("leaq", [opr1; opr2]) -> Lea (dst(opr2, Some 8L), opr1) (* the memory operand in LEA is not converted to ld/st *)
      | ("add", [opr1; opr2]) -> Add (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("addb", [opr1; opr2]) -> Add (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("addw", [opr1; opr2]) -> Add (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("addl", [opr1; opr2]) -> Add (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("addq", [opr1; opr2]) -> Add (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("adc", [opr1; opr2]) -> Adc (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("adcb", [opr1; opr2]) -> Adc (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("adcw", [opr1; opr2]) -> Adc (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("adcl", [opr1; opr2]) -> Adc (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("adcq", [opr1; opr2]) -> Adc (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("sub", [opr1; opr2]) -> Sub (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("subb", [opr1; opr2]) -> Sub (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("subw", [opr1; opr2]) -> Sub (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("subl", [opr1; opr2]) -> Sub (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("subq", [opr1; opr2]) -> Sub (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L)) (* note that the minuend is opr2 *)
      | ("mul", [opr1]) -> Imul (dst(RegOp (Isa.fix_reg_size RAX (Option.get dirty_op_size)), dirty_op_size), src(RegOp (Isa.fix_reg_size RAX (Option.get dirty_op_size)), dirty_op_size), src(opr1, dirty_op_size))
      | ("mulb", [opr1]) -> Imul (dst(RegOp AL, Some 1L), src(RegOp AL, Some 1L), src(opr1, Some 1L))
      | ("mulw", [opr1]) -> Imul (dst(RegOp AX, Some 2L), src(RegOp AX, Some 2L), src(opr1, Some 2L))
      | ("mull", [opr1]) -> Imul (dst(RegOp EAX, Some 4L), src(RegOp EAX, Some 4L), src(opr1, Some 4L))
      | ("mulq", [opr1]) -> Imul (dst(RegOp RAX, Some 8L), src(RegOp RAX, Some 8L), src(opr1, Some 8L))
      | ("imul", [opr1; opr2]) -> Imul (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("imulb", [opr1; opr2]) -> Imul (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("imulw", [opr1; opr2]) -> Imul (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("imull", [opr1; opr2]) -> Imul (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("imulq", [opr1; opr2]) -> Imul (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("imul", [opr0; opr1; opr2]) -> Imul (dst(opr2, dirty_op_size), src(opr1, dirty_op_size), src(opr0, dirty_op_size))
      | ("imulb", [opr0; opr1; opr2]) -> Imul (dst(opr2, Some 1L), src(opr1, Some 1L), src(opr0, Some 1L))
      | ("imulw", [opr0; opr1; opr2]) -> Imul (dst(opr2, Some 2L), src(opr1, Some 2L), src(opr0, Some 2L))
      | ("imull", [opr0; opr1; opr2]) -> Imul (dst(opr2, Some 4L), src(opr1, Some 4L), src(opr0, Some 4L))
      | ("imulq", [opr0; opr1; opr2]) -> Imul (dst(opr2, Some 8L), src(opr1, Some 8L), src(opr0, Some 8L))
      | ("sal", [opr1; opr2]) -> Sal (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("salb", [opr1; opr2]) -> Sal (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("salw", [opr1; opr2]) -> Sal (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("sall", [opr1; opr2]) -> Sal (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("salq", [opr1; opr2]) -> Sal (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("sal", [opr2]) -> Sal (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(ImmOp (ImmNum 1L), dirty_op_size))
      | ("salb", [opr2]) -> Sal (dst(opr2, Some 1L), src(opr2, Some 1L), src(ImmOp (ImmNum 1L), Some 1L))
      | ("salw", [opr2]) -> Sal (dst(opr2, Some 2L), src(opr2, Some 2L), src(ImmOp (ImmNum 1L), Some 2L))
      | ("sall", [opr2]) -> Sal (dst(opr2, Some 4L), src(opr2, Some 4L), src(ImmOp (ImmNum 1L), Some 4L))
      | ("salq", [opr2]) -> Sal (dst(opr2, Some 8L), src(opr2, Some 8L), src(ImmOp (ImmNum 1L), Some 8L))
      | ("sar", [opr1; opr2]) -> Sar (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("sarb", [opr1; opr2]) -> Sar (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("sarw", [opr1; opr2]) -> Sar (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("sarl", [opr1; opr2]) -> Sar (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("sarq", [opr1; opr2]) -> Sar (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("sar", [opr2]) -> Sar (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(ImmOp (ImmNum 1L), dirty_op_size))
      | ("sarb", [opr2]) -> Sar (dst(opr2, Some 1L), src(opr2, Some 1L), src(ImmOp (ImmNum 1L), Some 1L))
      | ("sarw", [opr2]) -> Sar (dst(opr2, Some 2L), src(opr2, Some 2L), src(ImmOp (ImmNum 1L), Some 2L))
      | ("sarl", [opr2]) -> Sar (dst(opr2, Some 4L), src(opr2, Some 4L), src(ImmOp (ImmNum 1L), Some 4L))
      | ("sarq", [opr2]) -> Sar (dst(opr2, Some 8L), src(opr2, Some 8L), src(ImmOp (ImmNum 1L), Some 8L))
      | ("shr", [opr1; opr2]) -> Shr (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("shrb", [opr1; opr2]) -> Shr (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("shrw", [opr1; opr2]) -> Shr (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("shrl", [opr1; opr2]) -> Shr (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("shrq", [opr1; opr2]) -> Shr (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("shr", [opr2]) -> Shr (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(ImmOp (ImmNum 1L), dirty_op_size))
      | ("shrb", [opr2]) -> Shr (dst(opr2, Some 1L), src(opr2, Some 1L), src(ImmOp (ImmNum 1L), Some 1L))
      | ("shrw", [opr2]) -> Shr (dst(opr2, Some 2L), src(opr2, Some 2L), src(ImmOp (ImmNum 1L), Some 2L))
      | ("shrl", [opr2]) -> Shr (dst(opr2, Some 4L), src(opr2, Some 4L), src(ImmOp (ImmNum 1L), Some 4L))
      | ("shrq", [opr2]) -> Shr (dst(opr2, Some 8L), src(opr2, Some 8L), src(ImmOp (ImmNum 1L), Some 8L))
      | ("rol", [opr1; opr2]) -> Rol (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("rolb", [opr1; opr2]) -> Rol (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("rolw", [opr1; opr2]) -> Rol (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("roll", [opr1; opr2]) -> Rol (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("rolq", [opr1; opr2]) -> Rol (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("rol", [opr2]) -> Rol (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(ImmOp (ImmNum 1L), dirty_op_size))
      | ("rolb", [opr2]) -> Rol (dst(opr2, Some 1L), src(opr2, Some 1L), src(ImmOp (ImmNum 1L), Some 1L))
      | ("rolw", [opr2]) -> Rol (dst(opr2, Some 2L), src(opr2, Some 2L), src(ImmOp (ImmNum 1L), Some 2L))
      | ("roll", [opr2]) -> Rol (dst(opr2, Some 4L), src(opr2, Some 4L), src(ImmOp (ImmNum 1L), Some 4L))
      | ("rolq", [opr2]) -> Rol (dst(opr2, Some 8L), src(opr2, Some 8L), src(ImmOp (ImmNum 1L), Some 8L))
      | ("ror", [opr1; opr2]) -> Ror (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("rorb", [opr1; opr2]) -> Ror (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("rorw", [opr1; opr2]) -> Ror (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("rorl", [opr1; opr2]) -> Ror (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("rorq", [opr1; opr2]) -> Ror (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("ror", [opr2]) -> Ror (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(ImmOp (ImmNum 1L), dirty_op_size))
      | ("rorb", [opr2]) -> Ror (dst(opr2, Some 1L), src(opr2, Some 1L), src(ImmOp (ImmNum 1L), Some 1L))
      | ("rorw", [opr2]) -> Ror (dst(opr2, Some 2L), src(opr2, Some 2L), src(ImmOp (ImmNum 1L), Some 2L))
      | ("rorl", [opr2]) -> Ror (dst(opr2, Some 4L), src(opr2, Some 4L), src(ImmOp (ImmNum 1L), Some 4L))
      | ("rorq", [opr2]) -> Ror (dst(opr2, Some 8L), src(opr2, Some 8L), src(ImmOp (ImmNum 1L), Some 8L))
      | ("xor", [opr1; opr2]) -> Xor (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("xorb", [opr1; opr2]) -> Xor (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("xorw", [opr1; opr2]) -> Xor (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("xorl", [opr1; opr2]) -> Xor (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("xorq", [opr1; opr2]) -> Xor (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("not", [opr]) -> Not (dst(opr, dirty_op_size), src(opr, dirty_op_size))
      | ("notb", [opr]) -> Not (dst(opr, Some 1L), src(opr, Some 1L))
      | ("notw", [opr]) -> Not (dst(opr, Some 2L), src(opr, Some 2L))
      | ("notl", [opr]) -> Not (dst(opr, Some 4L), src(opr, Some 4L))
      | ("notq", [opr]) -> Not (dst(opr, Some 8L), src(opr, Some 8L))
      | ("and", [opr1; opr2]) -> And (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("andb", [opr1; opr2]) -> And (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("andw", [opr1; opr2]) -> And (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("andl", [opr1; opr2]) -> And (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("andq", [opr1; opr2]) -> And (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("or", [opr1; opr2]) -> Or (dst(opr2, dirty_op_size), src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("orb", [opr1; opr2]) -> Or (dst(opr2, Some 1L), src(opr2, Some 1L), src(opr1, Some 1L))
      | ("orw", [opr1; opr2]) -> Or (dst(opr2, Some 2L), src(opr2, Some 2L), src(opr1, Some 2L))
      | ("orl", [opr1; opr2]) -> Or (dst(opr2, Some 4L), src(opr2, Some 4L), src(opr1, Some 4L))
      | ("orq", [opr1; opr2]) -> Or (dst(opr2, Some 8L), src(opr2, Some 8L), src(opr1, Some 8L))
      | ("bswap", [opr]) -> Bswap (dst(opr, Some 8L))
      | ("rep", [LabelOp label]) ->
          if label = "stosq" then RepStosq 
          else if label = "movsq" then RepMovsq 
          else begin
            Printf.printf "rep %s\n" label;
            parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
          end
      | ("cmp", [opr1; opr2]) -> Cmp (src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("cmpb", [opr1; opr2]) -> Cmp (src(opr2, Some 1L), src(opr1, Some 1L))
      | ("cmpw", [opr1; opr2]) -> Cmp (src(opr2, Some 2L), src(opr1, Some 2L))
      | ("cmpl", [opr1; opr2]) -> Cmp (src(opr2, Some 4L), src(opr1, Some 4L))
      | ("cmpq", [opr1; opr2]) -> Cmp (src(opr2, Some 8L), src(opr1, Some 8L))
      | ("test", [opr1; opr2]) -> Test (src(opr2, dirty_op_size), src(opr1, dirty_op_size))
      | ("testb", [opr1; opr2]) -> Test (src(opr2, Some 1L), src(opr1, Some 1L))
      | ("testw", [opr1; opr2]) -> Test (src(opr2, Some 2L), src(opr1, Some 2L))
      | ("testl", [opr1; opr2]) -> Test (src(opr2, Some 4L), src(opr1, Some 4L))
      | ("testq", [opr1; opr2]) -> Test (src(opr2, Some 8L), src(opr1, Some 8L))
      | ("jmp", [LabelOp lb]) -> Jmp (lb)
      | ("je", [LabelOp lb]) -> Jcond (lb, JE)
      | ("jne", [LabelOp lb]) -> Jcond (lb, JNe)
      | ("jbe", [LabelOp lb]) -> Jcond (lb, JBe) (* TODO: Note that this is not correct, just for quick test only *)
      | ("jb", [LabelOp lb]) -> Jcond (lb, JB) (* TODO: Note that this is not correct, just for quick test only *)
      | ("jnb", [LabelOp lb]) -> Jcond (lb, JAe)  (* TODO: Note that this is not correct, just for quick test only *)
      | ("call", [LabelOp lb]) -> Call (lb)
      | ("ret", []) -> Jmp (Isa.ret_label)
      | ("pushq", [opr]) -> Push (src(opr, Some 8L))
      | ("popq", [opr]) -> Pop (src(opr, Some 8L))
      | ("syscall", []) -> Syscall
      | ("hlt", []) -> Hlt
      | _ -> 
        Printf.printf "%s %d\n" mnemonic (List.length operands);
        parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
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
    (imm_var_map, func), {
      label = label; 
      insts = insts
    }

  let filter_compiler_annotation (source: string) : bool =
    if String.length source < 2 then true
    else
      let s = String.sub source 0 2 in
      s = "\t."

  let split_on_chars sep_list (s: string) : string list =
    let helper (s: string list) sep : string list =
      List.concat (List.map (String.split_on_char sep) s)
    in
    List.fold_left helper [ s ] sep_list

  let parse_program (source: string) : Isa.prog =
    let lines = source
      (* Split lines *)
      (* |> String.split_on_char '\n' *)
      |> split_on_chars [ '\n'; ';' ]
      (* Remove empty lines and comment lines *)
      |> List.filter_map (fun line ->
          if filter_compiler_annotation line then None 
          else begin
            let first_sharp = String.index_opt line '#' in
            let line = if first_sharp <> None then
              String.sub line 0 (Option.get first_sharp)
            else
              line
            in
            let line = String.trim line in
            if line = "" then None
            else Some(line)
          end
         )
    in
    (* Printf.printf "Remain len %d\n" (List.length lines);
    List.iter (fun x -> Printf.printf "%s\n" x) lines; *)
    let is_label (line: string) : bool =
      line.[(String.length line) - 1] = ':'
    in
    (* We assume function label does not start with "."! *)
    let is_func_label (line: string) : bool =
      (is_label line) && line.[0] <> '.'
    in
    let rec spliter_helper is_start bb acc_bb lines =
      match lines with
      | [] -> List.rev (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
      | line :: lines ->
          if is_start line then
            spliter_helper is_start [line] (if bb = [] then acc_bb else (List.rev bb) :: acc_bb) lines
          else
            spliter_helper is_start (line :: bb) acc_bb lines
    in
    (* Split lines into basic blocks *)
    let bb_spliter = spliter_helper is_label in
    let func_spliter lines =
      let func_str_list = spliter_helper is_func_label [] [] lines in
      List.map (bb_spliter [] []) func_str_list
    in
    let func_list = func_spliter lines in
    let imm_var_map, func_list =
      let helper 
          (imm_var_map: Isa.imm_var_map) (bbs: string list list) :
          Isa.imm_var_map * Isa.func =
        let (imm_var_map, func_name), bbs =
          List.fold_left_map parse_basic_block (imm_var_map, None) bbs
        in
        imm_var_map, { name = Option.get func_name; body = bbs }
      in
      List.fold_left_map helper Isa.StrM.empty func_list
    in
    (* let rec bb_spliter lines bb acc_bb =
      match lines with
      | [] -> List.rev (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
      | line :: lines ->
          if line.[(String.length line) - 1] <> ':'
          then bb_spliter lines (line :: bb) acc_bb
          else bb_spliter lines [line] (if bb = [] then acc_bb else (List.rev bb) :: acc_bb)
    in
    let bbs = bb_spliter lines [] [] in
    let (imm_var_map, _), bbs = List.fold_left_map (
      fun info bb -> parse_basic_block info bb
    ) (Isa.StrM.empty, None) bbs in *)

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
    let func_list = List.map (
      fun (func: Isa.func) ->
        { func with body = add_jmp_for_adj_bb func.body []}
    ) func_list in
    (* let bbs = add_jmp_for_adj_bb bbs [] in *)
    let get_ret_bb : Isa.basic_block =
      { label = Isa.ret_label; insts = [] } in
    let func_list = List.map (
      fun (func: Isa.func) ->
        { func with body = func.body @ [get_ret_bb] }
    ) func_list
    in
    { funcs = func_list; imm_var_map = imm_var_map }
    (* {bbs = bbs @ [get_ret_bb]; imm_var_map = imm_var_map} *)

end

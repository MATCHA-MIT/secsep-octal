open Isa

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
    | ImmTok of (Isa.immediate * string option)
    | Comma
    | LParen
    | RParen

  let imm_var_unset : Isa.imm_var_id = -1

  let string_of_token (t: token) : string =
    match t with
    | MneTok s -> "MneTok " ^ s
    | RegTok r -> "RegTok " ^ (string_of_register r)
    | ImmTok (ImmNum n, _) -> "ImmTok " ^ (string_of_int n)
    | ImmTok (ImmLabel v, name) -> "ImmTok (var " ^ (string_of_int v) ^ ": " ^ Option.get name ^ ")"
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

  let consume_int (cs: char list) : int * char list =
    let rec go (cs: char list) (acc: char list) =
      match cs with
      | [] -> (List.rev acc |> List.to_seq |> String.of_seq |> int_of_string, [])
      | c :: cs ->
        if c = '-' || c = 'x' || c = 'X'
          || (Char.code c >= 48 && Char.code c <= 57)
          || (Char.code c >= 65 && Char.code c <= 70)
          || (Char.code c >= 97 && Char.code c <= 102)
        then
          go cs (c :: acc)
        else
          (List.rev acc |> List.to_seq |> String.of_seq |> int_of_string, c :: cs)
    in
    if cs = [] then
      lexical_error "consume_int: unexpected end of input"
    else
      go cs []

  let consume_immediate (cs: char list) : Isa.immediate * string option * char list =
    if cs = [] then
      lexical_error "consume_immediate: unexpected end of input"
    else if is_char_of_name_start (List.hd cs) then (
      let (name, cs) = consume_name cs in
      (ImmLabel imm_var_unset, Some name, cs)
    )
    else (
      let (num, cs) = consume_int cs in
      (ImmNum num, None, cs)
    )

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
    | ImmNum 1 -> Scale1
    | ImmNum 2 -> Scale2
    | ImmNum 4 -> Scale4
    | ImmNum 8 -> Scale8
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

  module M = Map.Make(String)
  type imm_var_map = Isa.imm_var_id M.t

  let parse_jmp (ts: token list) : Isa.operand =
    match ts with
    | ImmTok (ImmLabel _, name) :: [] -> LabelOp (Option.get name)
    | _ -> parse_error "parse_jmp"

  let parse_tokens (imm_var_map: imm_var_map) (ts: token list) : imm_var_map * Isa.instruction =
    let src (opr: Isa.operand) : Isa.operand =
      match opr with
      | MemOp (disp, base, index, scale) -> LdOp (disp, base, index, scale)
      | StOp _ -> parse_error "src"
      | _ -> opr
    in
    let dst (opr: Isa.operand) : Isa.operand =
      match opr with
      | MemOp (disp, base, index, scale) -> StOp (disp, base, index, scale)
      | LdOp _ -> parse_error "dst"
      | _ -> opr
    in
    let (mnemonic, ts) = match ts with
    | MneTok mnemonic :: ts -> (mnemonic, ts)
    | _ -> parse_error ("parse_tokens: unexpected end of input: " ^ (String.concat ", " (List.map string_of_token ts)))
    in
    let imm_var_map, operands = if Isa.is_jmp_mnemonic mnemonic
      then imm_var_map, [parse_jmp ts]
      else begin 
        let imm_var_map, ts = List.fold_left_map (fun imm_var_map token ->
          match token with
          | ImmTok (ImmLabel v, Some name) ->
            if v <> imm_var_unset
            then (imm_var_map, token)
            else begin
              if M.mem name imm_var_map
              then (imm_var_map, ImmTok (ImmLabel (M.find name imm_var_map), Some name))
              else begin
                let id = M.cardinal imm_var_map in (* TODO: start stack id *)
                print_endline (name ^ " " ^ (string_of_int id));
                (M.add name id imm_var_map, ImmTok (ImmLabel id, Some name))
              end
            end
          | _ -> (imm_var_map, token)
        ) imm_var_map ts
        in
        imm_var_map, parse_operands ts
      end
    in
    let inst: Isa.instruction = match (mnemonic, operands) with
      | ("mov", [opr1; opr2])
      | ("movq", [opr1; opr2])
      | ("movl", [opr1; opr2])   -> Mov   (dst(opr2), src(opr1))
      | ("movs", [opr1; opr2])
      | ("movslq", [opr1; opr2]) -> MovS  (dst(opr2), src(opr1))
      | ("movz", [opr1; opr2])
      | ("movzbl", [opr1; opr2]) -> MovZ  (dst(opr2), src(opr1))
      | ("lea", [opr1; opr2])
      | ("leaq", [opr1; opr2])   -> Lea   (dst(opr2), opr1) (* the memory operand in LEA is not converted to ld/st *)
      | ("add", [opr1; opr2])
      | ("addq", [opr1; opr2])   -> Add   (dst(opr2), src(opr1), src(opr2))
      | ("sub", [opr1; opr2])
      | ("subq", [opr1; opr2])   -> Sub   (dst(opr2), src(opr2), src(opr1)) (* note that the minuend is opr2 *)
      | ("sal", [opr1; opr2])
      | ("salq", [opr1; opr2])   -> Sal   (dst(opr2), src(opr2), src(opr1))
      | ("sar", [opr1; opr2])
      | ("sarq", [opr1; opr2])   -> Sar   (dst(opr2), src(opr2), src(opr1))
      | ("xor", [opr1; opr2])
      | ("xorb", [opr1; opr2])
      | ("xorl", [opr1; opr2])
      | ("xorq", [opr1; opr2])   -> Xor   (dst(opr2), src(opr1), src(opr2))
      | ("not", [opr])
      | ("notq", [opr])          -> Not   (dst(opr), src(opr))
      | ("and", [opr1; opr2])
      | ("andl", [opr1; opr2])
      | ("andq", [opr1; opr2])   -> And   (dst(opr2), src(opr1), src(opr2))
      | ("cmp", [opr1; opr2])
      | ("cmpq", [opr1; opr2])   -> Cmp   (src(opr1), src(opr2))
      | ("jmp", [LabelOp lb])    -> Jmp   (lb)
      | ("jne", [LabelOp lb])    -> Jcond (lb, JNe)
      | ("ret", []) -> Ret
      | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
    in
    imm_var_map, inst

  let parse_instr (imm_var_map: imm_var_map) (line: string) : imm_var_map * Isa.instruction =
    (* print_endline ("Parsing " ^ line); *)
    let tokens = tokenize_line line in
    (* print_endline (String.concat ", " (List.map string_of_token tokens)); *)
    parse_tokens imm_var_map tokens

  let parse_basic_block (imm_var_map: imm_var_map) (lines: string list) : imm_var_map * Isa.basic_block =
    let label = parse_label (List.hd lines) in
    let imm_var_map, insts = List.fold_left_map (
      fun imm_var_map line -> parse_instr imm_var_map line
    ) imm_var_map (List.tl lines) in
    (imm_var_map, {label = label; insts = insts})

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
    let _, bbs = List.fold_left_map (
      fun imm_var_map bb -> parse_basic_block imm_var_map bb
    ) M.empty bbs in
    bbs

end

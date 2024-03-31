(* x86_64 assembly *)

type label = string

type register =
  |     RAX |     RCX |     RDX |     RBX | RSP  | RBP  | RSI  | RDI  | R8  | R9  | R10  | R11  | R12  | R13  | R14  | R15
  |     EAX |     ECX |     EDX |     EBX | ESP  | EBP  | ESI  | EDI  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D
  |      AX |      CX |      DX |      BX |  SP  |  BP  |  SI  |  DI  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W
  | AH | AL | CH | CL | DH | DL | BH | BL |  SPL |  BPL |  SIL |  DIL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B

type immediate =
  | ImmNum of int
  | ImmLabel of string

type scale = Scale1 | Scale2 | Scale4 | Scale8

type operand =
  | ImmOpr of immediate
  | RegOpr of register
  | MemOpr of immediate option * register option * register option * scale option (* disp, base, index, scale *)
  | LabelOpr of label

(* destination (if any) is the first operand *)
type instruction =
  | Mov of operand * operand
  | MovS of operand * operand
  | MovZ of operand * operand
  | Lea of operand * operand
  | Add of operand * operand
  | Sub of operand * operand
  | Sal of operand * immediate
  | Sar of operand * immediate
  | Xor of operand * operand
  | Not of operand
  | And of operand * operand
  | Cmp of operand * operand
  | Jmp of operand
  | Jne of operand
  | Ret
  | Nop

type basic_block = {
  label: label;
  instrs: instruction list;
}

type program = basic_block list

exception LexicalError of string

exception ParseError of string

let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))

let parse_label (line: string) : label =
  let label_name = String.sub line 0 ((String.length line) - 1) in
  label_name

let parse_register (name: string) : register =
  match name with
  |     "rax" -> RAX |     "rcx" -> RCX |     "rdx" -> RDX |     "rbx" -> RBX | "rsp" -> RSP  | "rbp" -> RBP  | "rsi" -> RSI  | "rdi" -> RDI  | "r8" -> R8  | "r9" -> R9  | "r10" -> R10  | "r11" -> R11  | "r12" -> R12  | "r13" -> R13  | "r14" -> R14  | "r15" -> R15
  |     "eax" -> EAX |     "ecx" -> ECX |     "edx" -> EDX |     "ebx" -> EBX | "esp" -> ESP  | "ebp" -> EBP  | "esi" -> ESI  | "edi" -> EDI  | "r8d" -> R8D | "r9d" -> R9D | "r10d" -> R10D | "r11d" -> R11D | "r12d" -> R12D | "r13d" -> R13D | "r14d" -> R14D | "r15d" -> R15D
  |      "ax" -> AX |      "cx" -> CX |      "dx" -> DX |      "bx" -> BX |  "sp" -> SP  |  "bp" -> BP  |  "si" -> SI  |  "di" -> DI  | "r8w" -> R8W | "r9w" -> R9W | "r10w" -> R10W | "r11w" -> R11W | "r12w" -> R12W | "r13w" -> R13W | "r14w" -> R14W | "r15w" -> R15W
  | "ah" -> AH | "al" -> AL | "ch" -> CH | "cl" -> CL | "dh" -> DH | "dl" -> DL | "bh" -> BH | "bl" -> BL |  "spl" -> SPL |  "bpl" -> BPL |  "sil" -> SIL |  "dil" -> DIL | "r8b" -> R8B | "r9b" -> R9B | "r10b" -> R10B | "r11b" -> R11B | "r12b" -> R12B | "r13b" -> R13B | "r14b" -> R14B | "r15b" -> R15B
  | _ -> parse_error ("parse_register: invalid register name " ^ name)

let string_of_register (r: register) : string =
  match r with
  |     RAX -> "rax" |     RCX -> "rcx" |     RDX -> "rdx" |     RBX -> "rbx" | RSP -> "rsp"  | RBP -> "rbp"  | RSI -> "rsi"  | RDI -> "rdi"  | R8 -> "r8"  | R9 -> "r9"  | R10 -> "r10"  | R11 -> "r11"  | R12 -> "r12"  | R13 -> "r13"  | R14 -> "r14"  | R15 -> "r15"
  |     EAX -> "eax" |     ECX -> "ecx" |     EDX -> "edx" |     EBX -> "ebx" | ESP -> "esp"  | EBP -> "ebp"  | ESI -> "esi"  | EDI -> "edi"  | R8D -> "r8d" | R9D -> "r9d" | R10D -> "r10d" | R11D -> "r11d" | R12D -> "r12d" | R13D -> "r13d" | R14D -> "r14d" | R15D -> "r15d"
  |      AX -> "ax" |      CX -> "cx" |      DX -> "dx" |      BX -> "bx" |  SP -> "sp"  |  BP -> "bp"  |  SI -> "si"  |  DI -> "di"  | R8W -> "r8w" | R9W -> "r9w" | R10W -> "r10w" | R11W -> "r11w" | R12W -> "r12w" | R13W -> "r13w" | R14W -> "r14w" | R15W -> "r15w"
  | AH -> "ah" | AL -> "al" | CH -> "ch" | CL -> "cl" | DH -> "dh" | DL -> "dl" | BH -> "bh" | BL -> "bl" |  SPL -> "spl" |  BPL -> "bpl" |  SIL -> "sil" |  DIL -> "dil" | R8B -> "r8b" | R9B -> "r9b" | R10B -> "r10b" | R11B -> "r11b" | R12B -> "r12b" | R13B -> "r13b" | R14B -> "r14b" | R15B -> "r15b"

(* x86_64 assembly parser *)

type token =
  | MneTok of string
  | RegTok of register
  | ImmTok of immediate
  | Comma
  | LParen
  | RParen

let string_of_token (t: token) : string =
  match t with
  | MneTok s -> "MneTok " ^ s
  | RegTok r -> "RegTok " ^ (string_of_register r)
  | ImmTok (ImmNum n) -> "ImmTok " ^ (string_of_int n)
  | ImmTok (ImmLabel s) -> "ImmTok " ^ s
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

let consume_immediate (cs: char list) : immediate * char list =
  if cs = [] then
    lexical_error "consume_immediate: unexpected end of input"
  else if is_char_of_name_start (List.hd cs) then (
    let (name, cs) = consume_name cs in
    (ImmLabel name, cs)
  )
  else (
    let (num, cs) = consume_int cs in
    (ImmNum num, cs)
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
    | c :: _ ->
        let (imm, cs) = consume_immediate cs in
        go cs (ImmTok imm :: acc)

  in
  let cs = line |> String.to_seq |> List.of_seq in
  let (mne_str, cs) = consume_name cs in 
  go cs [MneTok mne_str]

let imm_to_scale (imm: immediate) : scale =
  match imm with
  | ImmNum 1 -> Scale1
  | ImmNum 2 -> Scale2
  | ImmNum 4 -> Scale4
  | ImmNum 8 -> Scale8
  | _ -> parse_error "imm_to_scale"

let parse_memory_operand (ts: token list) : operand * token list =
  match ts with
  | LParen :: RegTok base :: RParen :: ts -> (* (base) *)
    (MemOpr (None, Some base, None, None), ts)
  | LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts -> (* (base) *)
    (MemOpr (None, Some base, Some index, None), ts)
  | ImmTok disp :: LParen :: RegTok base :: RParen :: ts -> (* disp(base) *)
    (MemOpr (Some disp, Some base, None, None), ts)
  | LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok imm :: RParen :: ts ->  (* (base, index, scale) *)
    (MemOpr (None, Some base, Some index, Some (imm_to_scale imm)), ts)
  | ImmTok disp :: LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts ->  (* disp(base, index) *)
    (MemOpr (Some disp, Some base, Some index, None), ts)
  | ImmTok disp :: LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok imm :: RParen :: ts ->  (* disp(base, index, scale) *)
    (MemOpr (Some disp, Some base, Some index, Some (imm_to_scale imm)), ts)
  | ImmTok disp :: LParen :: RegTok index :: Comma :: ImmTok imm :: RParen :: ts ->  (* disp(index, scale) *)
    (MemOpr (Some disp, None, Some index, Some (imm_to_scale imm)), ts)
  | _ -> parse_error ("parse_memory_operand: " ^ (String.concat ", " (List.map string_of_token ts)))

let parse_operand (ts: token list) : operand * token list =
  match ts with
  | RegTok reg :: ts -> (RegOpr reg, ts)
  | ImmTok _ :: LParen :: _ -> parse_memory_operand ts
  | ImmTok imm :: ts -> (ImmOpr imm, ts)
  | LParen :: _ -> parse_memory_operand ts
  | _ -> parse_error "parse_operand"

let parse_operands (ts: token list) : operand list =
  let rec go ts acc =
    match ts with
    | [] -> List.rev acc
    | Comma :: ts -> go ts acc
    | _ ->
      let (opr, ts) = parse_operand ts in
      go ts (opr :: acc)
  in
  go ts []

let parse_tokens (ts: token list) : instruction =
  let (mnemonic, operands) = match ts with
  | MneTok mnemonic :: ts -> (mnemonic, parse_operands ts)
  | _ -> parse_error ("parse_tokens: unexpected end of input: " ^ (String.concat ", " (List.map string_of_token ts)))
  in
  match (mnemonic, operands) with
  | ("mov", [opr1; opr2]) -> Mov (opr2, opr1)
  | ("movq", [opr1; opr2]) -> Mov (opr2, opr1)
  | ("movl", [opr1; opr2]) -> Mov (opr2, opr1)
  | ("movs", [opr1; opr2]) -> MovS (opr2, opr1)
  | ("movslq", [opr1; opr2]) -> MovS (opr2, opr1)
  | ("movz", [opr1; opr2]) -> MovZ (opr2, opr1)
  | ("movzbl", [opr1; opr2]) -> MovZ (opr2, opr1)
  | ("lea", [opr1; opr2]) -> Lea (opr2, opr1)
  | ("leaq", [opr1; opr2]) -> Lea (opr2, opr1)
  | ("add", [opr1; opr2]) -> Add (opr2, opr1)
  | ("addq", [opr1; opr2]) -> Add (opr2, opr1)
  | ("sub", [opr1; opr2]) -> Sub (opr2, opr1)
  | ("subq", [opr1; opr2]) -> Sub (opr2, opr1)
  | ("sal", [ImmOpr imm; opr1]) -> Sal (opr1, imm)
  | ("salq", [ImmOpr imm; opr1]) -> Sal (opr1, imm)
  | ("sar", [ImmOpr imm; opr1]) -> Sar (opr1, imm)
  | ("sarq", [ImmOpr imm; opr1]) -> Sar (opr1, imm)
  | ("xor", [opr1; opr2]) -> Xor (opr2, opr1)
  | ("xorb", [opr1; opr2]) -> Xor (opr2, opr1)
  | ("xorl", [opr1; opr2]) -> Xor (opr2, opr1)
  | ("xorq", [opr1; opr2]) -> Xor (opr2, opr1)
  | ("not", [opr]) -> Not opr
  | ("notq", [opr]) -> Not opr
  | ("and", [opr1; opr2]) -> And (opr2, opr1)
  | ("andl", [opr1; opr2]) -> And (opr2, opr1)
  | ("andq", [opr1; opr2]) -> And (opr2, opr1)
  | ("cmp", [opr1; opr2]) -> Cmp (opr1, opr2)
  | ("cmpq", [opr1; opr2]) -> Cmp (opr1, opr2)
  | ("jmp", [opr1]) -> Jmp (opr1)
  | ("jne", [opr1]) -> Jne (opr1)
  | ("ret", []) -> Ret
  | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)

let parse_instr (line: string) : instruction =
  (* print_endline ("Parsing " ^ line); *)
  let tokens = tokenize_line line in
  (* print_endline (String.concat ", " (List.map string_of_token tokens)); *)
  parse_tokens tokens

let parse_basic_block (lines: string list) : basic_block = {
  label = parse_label (List.hd lines);
  instrs = List.map parse_instr (List.tl lines)
}

let parse_program (source: string) : program =
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
  List.map parse_basic_block bbs

let _ = parse_program "
table_select:
.LFB82:
# Reg read: rdx: k1, rsi: k2
# Reg write: r9
# Reg unused:
movl %edx, %r9d
movslq %esi, %rsi
leaq (%rsi,%rsi,2), %rsi
salq $8, %rsi
addq $k25519Precomp, %rsi
movl $1, %edi
# rdx: k1, rdi: 1, rsi: k25519Precomp + k2 * 768, r9: k1
.L80:
# Reg read: r9: k3, rdi: k4
# Reg write: rax, rdx
# Reg unused: rsi: k5
movq %r9, %rax
xorq %rdi, %rax
movq %rax, %rdx
subq $1, %rax
notq %rdx
andq %rax, %rdx
xorl %eax, %eax
sarq $63, %rdx
# To L79:
# rax: 0, rdx: sth(0/1), rdi: k4, rsi: k5, r9: k3
.L79:
# Reg read: rax: k6, rdx: k7, rsi: k8, rdi: k9
# Reg write: rcx
# Reg unused:
movzbl (%rsi,%rax), %ecx
# Minimum req: k8=ptr, k6=idx or rsi=idx, rax=ptr
andl %edx, %ecx
xorb %cl, -88(%rsp,%rax)
# Minimum req: k6=idx
addq $1, %rax
cmpq $96, %rax
jne .L79
# To L79:
# rax: k6+1{96}, rcx: any/data, rdx: k7, rsi: k8, rdi: k9
# To next:
# rax: {96}, rcx: any/data, rdx: k7, rsi: k8, rdi: k9
addq $1, %rdi
movzbl -88(%rsp), %eax
addq $96, %rsi
cmpq $9, %rdi
jne .L80
# To L80:
# rax: any/data, rcx: any/data, rdi: k9+1{9}, rsi: k8+96,
# To next:
# rax: any/data, rcx: any/data, rdi: {9}, rsi: k8+96,
# ...
ret
"
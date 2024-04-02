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

(* FIX (maybe later): distinguish store memopr, access memopr, pure memopr *)

(* FIX: add destination *)
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

exception TypeAlgoError of string

let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))

let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

let type_algo_error msg = raise (TypeAlgoError ("[Type Algorithm Error] " ^ msg))

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
  | LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts -> (* (base + index) *)
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

let int_of_immediate (imm: immediate) : int =
  match imm with
  | ImmNum n -> n
  | ImmLabel l -> type_algo_error "int_of_immediate: label lookup not supported yet"

type singleton_type =
| SingletonValue of int
| SingletonSymbol of int (* FIXME *)
(* memory: {symbol + offset: type; ...}*)

type arith_op =
| AddOp (* includes sub *)
| MulOp

type reg_type =
| SingletonT of singleton_type
| RangeT of int * int * int (* beg, end, step *) (* TODO: inclusive? *)
| VariableT of int (* variable id *)
| ArithT of arith_op * reg_type * int
| TypeArithT of arith_op * reg_type * reg_type
| SetSubT of reg_type * reg_type (* TODO: think of the second one *)
| TopT
| BottomT

type gpr_types = {
  rax: reg_type; rcx: reg_type; rdx: reg_type; rbx: reg_type;
  rsp: reg_type; rbp: reg_type; rsi: reg_type; rdi: reg_type;
  r8:  reg_type; r9:  reg_type; r10: reg_type; r11: reg_type;
  r12: reg_type; r13: reg_type; r14: reg_type; r15: reg_type;
}

type type_state = gpr_types * ((int * reg_type) list) (* reg types, (offset from initial rsp of func & type) *)

module VarMap = Map.Make (struct
  type t = int
  let compare = compare
end)

type subtype_relation = reg_type * reg_type

let is_function_entry (l: label) = String.length l >= 2 && l.[0] <> '.'

let get_reg_type (types: type_state) (reg: register) =
  let (gpr_types, _) = types in
  match reg with
  | RAX | EAX | AX | AH | AL -> gpr_types.rax
  | RCX | ECX | CX | CH | CL -> gpr_types.rcx
  | RDX | EDX | DX | DH | DL -> gpr_types.rdx
  | RBX | EBX | BX | BH | BL -> gpr_types.rbx
  | RSP | ESP | SP | SPL -> gpr_types.rsp
  | RBP | EBP | BP | BPL -> gpr_types.rbp
  | RSI | ESI | SI | SIL -> gpr_types.rsi
  | RDI | EDI | DI | DIL -> gpr_types.rdi
  | R8 | R8D | R8W | R8B -> gpr_types.r8
  | R9 | R9D | R9W | R9B -> gpr_types.r9
  | R10 | R10D | R10W | R10B -> gpr_types.r10
  | R11 | R11D | R11W | R11B -> gpr_types.r11
  | R12 | R12D | R12W | R12B -> gpr_types.r12
  | R13 | R13D | R13W | R13B -> gpr_types.r13
  | R14 | R14D | R14W | R14B -> gpr_types.r14
  | R15 | R15D | R15W | R15B -> gpr_types.r15

let set_reg_type (types: type_state) (reg: register) (t: reg_type)=
  let (gpr_types, stack_types) = types in
  match reg with
  | RAX | EAX | AX | AH | AL -> ({gpr_types with rax = t}, stack_types)
  | RCX | ECX | CX | CH | CL -> ({gpr_types with rcx = t}, stack_types)
  | RDX | EDX | DX | DH | DL -> ({gpr_types with rdx = t}, stack_types)
  | RBX | EBX | BX | BH | BL -> ({gpr_types with rbx = t}, stack_types)
  | RSP | ESP | SP | SPL -> ({gpr_types with rsp = t}, stack_types)
  | RBP | EBP | BP | BPL -> ({gpr_types with rbp = t}, stack_types)
  | RSI | ESI | SI | SIL -> ({gpr_types with rsi = t}, stack_types)
  | RDI | EDI | DI | DIL -> ({gpr_types with rdi = t}, stack_types)
  | R8 | R8D | R8W | R8B -> ({gpr_types with r8 = t}, stack_types)
  | R9 | R9D | R9W | R9B -> ({gpr_types with r9 = t}, stack_types)
  | R10 | R10D | R10W | R10B -> ({gpr_types with r10 = t}, stack_types)
  | R11 | R11D | R11W | R11B -> ({gpr_types with r11 = t}, stack_types)
  | R12 | R12D | R12W | R12B -> ({gpr_types with r12 = t}, stack_types)
  | R13 | R13D | R13W | R13B -> ({gpr_types with r13 = t}, stack_types)
  | R14 | R14D | R14W | R14B -> ({gpr_types with r14 = t}, stack_types)
  | R15 | R15D | R15W | R15B -> ({gpr_types with r15 = t}, stack_types)

let offset_of_mem_operand () :  =
  

let set_mem_type (types: type_state) (opr: operand) (t: reg_type) : type_state =
  let (gpr_types, stack_types) = types in
  match opr with
  | MemOpr (disp, base, index, scale) -> begin
      match disp, base, index, scale with begin
      | None, Some base, None, None ->
          let base_type = get_reg_type types base in
          (gpr_types, stack_types')
      (* TODO: match all patterns of memory addressing that refers to stack slot *)
      end
    end
  | _ -> type_algo_error "set_mem_type: expecting memory addressing operand"

let type_add (t1: reg_type) (t2: reg_type) : reg_type =
  match t1, t2 with
  | TopT, _ | _, TopT -> TopT
  | SingletonT (SingletonValue v1), SingletonT (SingletonValue v2) -> SingletonT (SingletonValue (v1 + v2))
  | SingletonT (SingletonBase), SingletonT (SingletonValue v)
  | SingletonT (SingletonValue v), SingletonT (SingletonBase) -> ArithT (AddOp, SingletonT (SingletonBase), v)
  | ArithT (AddOp, t, i), SingletonT (SingletonValue v)
  | SingletonT (SingletonValue v), ArithT (AddOp, t, i) -> ArithT (AddOp, t, i + v)
  | RangeT (bg, ed, st), SingletonT (SingletonValue v)
  | SingletonT (SingletonValue v) , RangeT (bg, ed, st) -> RangeT (bg + v, ed + v, st)
  (* TODO *)
  | _ -> TypeArithT (AddOp, t1, t2)

let stack_base_id = 1

(* TODO: Add size; warn when loads / stores' size does not match *)
let (types: type_state) (mem_opr: reg_type) : reg_type =
  let (_, stack_types) = types in
  match mem_opr with
  | ArithT (AddOp, SingletonSymbol s', SingletonValue offset) ->
      if s' = stack_base_id
      then begin
        List.find (fun (off, _) -> off = offset) stack_types |> snd
      end

let get_mem_type (types: type_state) (mem_opr: operand) : reg_type =
  let (_, stack_types) = types in
  let load_stack_type_by_offset (offset: int) : reg_type =
    try
      List.find (fun (off, _) -> off = offset) stack_types |> snd
    with
    | Not_found -> type_algo_error ("get_mem_type: stack offset " ^ (string_of_int offset) ^ " not in record")
  in
  let load_stack_type_by_type (offset_type: reg_type) (extra: int) : reg_type =
    match offset_type with
    | SingletonT (SingletonBase) -> load_stack_type_by_offset extra
    | ArithT (AddOp, SingletonT SingletonBase, off) -> load_stack_type_by_offset (off + extra)
    (* TODO: stack base + a range ? *)
    | _ -> type_algo_error "get_mem_type: unsupporting stack loading"
  in
  let helper disp base index scale =
    match disp, base, index, scale with
    | None, Some base, None, None ->
        load_stack_type_by_type (get_reg_type types base) 0 (* (base) *)
    | Some disp, Some base, None, None ->
        load_stack_type_by_type (get_reg_type types base) (int_of_immediate disp) (* disp(base) *)
    | None, Some base, Some index, None ->
        let base_type = get_reg_type types base in
        let index_type = get_reg_type types index in
        load_stack_type_by_type (TypeArithT (AddOp, base_type, index_type)) 0 (* (base + index) *)
    | _ -> type_algo_error "get_operand_type: unsupported memory operand"
  in 
  match mem_opr with
  | MemOpr (disp, base, index, scale) -> helper disp base index scale
  | _ -> type_algo_error "get_mem_type: expecting memory operand"

let get_operand_type (types: type_state) (opr: operand) : reg_type =
  match opr with
  | ImmOpr imm -> SingletonT (SingletonValue (int_of_immediate imm))
  | RegOpr r -> get_reg_type types r
  | MemOpr (_, _, _, _)-> get_mem_type types opr
  | LabelOpr _ -> type_algo_error "get_operand_type: label type not supported yet"

(* operand (memory operand) --> offset 
     --> use offset as value (LEA)
     --> use offset to load / store


*)

(* FIXME: only the part in "type_add" is different for different opcodes. reformat *)

let prop_add (types: type_state) (opr1: operand) (opr2: operand) : type_state =
  let new_type = type_add (get_operand_type types opr1) (get_operand_type types opr2) in
  match opr1, opr2 with
  | RegOpr r, _ ->
      set_reg_type types r new_type
  | MemOpr _, _ ->
      set_mem_type types opr1 new_type
  | _ -> type_algo_error "prop_add: unsupported"

let propagate_fwd (bb: basic_block) (init_types: type_state) : type_state =
  let rec helper (instrs: instruction list) (types: type_state) : type_state =
    match instrs with
    | [] -> types
    | instr :: instrs -> begin
        let types' = match instr with
        | Add(opr1, opr2) -> prop_add types opr1 opr2
        | _ -> type_algo_error "propagate_fwd: unsupported instruction"
        in
        helper instrs types'
      end

  in
  helper bb.instrs init_types

let foo (target: int) (r1: reg_type * reg_type) (r2: reg_type * reg_type) : reg_type option =
  match r1, r2 with
  | (SingletonValue base, VariableT var1), (SetSubT (ArithT (op, VariableT var2, step), SingletonValue bound))
  | (SetSubT (ArithT (op, VariableT var2, step), SingletonValue bound)), (SingletonValue base, VariableT var1) ->
      if var1 = var2 && var1 = target then begin
        match op with
        | AddOp ->
            if base < bound && step > 0 then
              Some RangeT (base, bound, step) (* FIXME: range inclusiveness *)
            else if base > bound && step < 0 then
              Some RangeT (bound, base, -step)
            else
              None
      end
      else None
  | _ -> None

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
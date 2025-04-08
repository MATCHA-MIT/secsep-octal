open Isa_basic
open Isa
open Full_mem_anno
open Branch_anno
open Call_anno

module Parser = struct

  exception LexicalError of string
  exception ParseError of string

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno) (BranchAnno) (CallAnno)

  let lexical_error msg = raise (LexicalError ("[Lexical Error] " ^ msg))
  let parse_error msg = raise (ParseError ("[Parse Error] " ^ msg))

  type token =
    | MneTok of string
    | RegTok of IsaBasic.register
    | RipTok
    | ImmTok of (IsaBasic.immediate * (string option) list)
    | Comma
    | LParen
    | RParen
    | Raw of string

  let rec string_of_imm_token (i: IsaBasic.immediate) (name_list: (string option) list) : string =
    match i, name_list with
    | ImmNum (n, _), None :: [] -> Int64.to_string n
    | ImmLabel (v, _), Some name :: [] -> "(var " ^ (string_of_int v) ^ ": " ^  name ^ ")"
    | ImmBExp ((i1, i2), _), hd :: tl ->
      (string_of_imm_token i1 [hd]) ^ " + " ^ (string_of_imm_token i2 tl)
    | _ -> parse_error "string_of_imm_token" 

  let string_of_token (t: token) : string =
    match t with
    | MneTok s -> "MneTok " ^ s
    | RegTok r -> "RegTok " ^ (IsaBasic.string_of_reg r)
    | RipTok -> "RipTok"
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
  
  let rec consume_whitespace (cs: char list) : char list =
    match cs with
    | [] -> []
    | c :: rest when (c = ' ' || c = '\t') -> consume_whitespace rest
    | _ -> cs

  let rec consume_immediate (cs: char list) : IsaBasic.immediate * (string option) list * char list =
    let exp, name_list, remained_cs =
    begin if cs = [] then
      lexical_error "consume_immediate: unexpected end of input"
    else if is_char_of_name_start (List.hd cs) then (
      let (name, cs) = consume_name cs in
      (Isa.ImmLabel (IsaBasic.imm_var_unset, None), [ Some name ], cs)
    )
    else (
      let (num, cs) = consume_int cs in
      (Isa.ImmNum (num, None), [ None ], cs)
    ) end 
    in

    let remained_cs = consume_whitespace remained_cs in
    match remained_cs with
    | '+' :: remained_cs_tl ->
      let other_exp, other_name_list, other_remained_cs =
        consume_immediate (consume_whitespace remained_cs_tl)
      in
      (Isa.ImmBExp ((exp, other_exp), None), name_list @ other_name_list, other_remained_cs)
    | '-' :: _ ->
      lexical_error "consume_immediate: constant subtraction expression not supported"
    | _ -> (exp, name_list, remained_cs)

  let tokenize_line (line: string) : token list =
    if Isa.line_is_directive line then [Raw line] else

    let rec go (cs: char list) (acc: token list) =
      match cs with
      | [] -> List.rev acc
      | ' ' :: cs | '\t' :: cs -> go cs acc
      | ',' :: cs -> go cs (Comma :: acc)
      | '(' :: cs -> go cs (LParen :: acc)
      | ')' :: cs -> go cs (RParen :: acc)
      | '%' :: cs ->
          let (name, cs) = consume_name cs in
          if name = "rip" then
            go cs (RipTok :: acc)
          else
            go cs (RegTok (name |> IsaBasic.string_to_reg |> Option.get) :: acc)
      | '$' :: cs ->
          go cs acc
      | _ ->
          let (imm, name, cs) = consume_immediate cs in
          go cs (ImmTok (imm, name) :: acc)
    in
    let cs = line |> String.to_seq |> List.of_seq in
    let (mne_str, cs) = consume_name cs in
    go cs [MneTok mne_str]

  let parse_memory_operand (ts: token list) : Isa.operand * token list =
    let imm_to_scale (imm: IsaBasic.immediate) : Isa.scale =
      match imm with
      | ImmNum (1L, _) -> Scale1
      | ImmNum (2L, _) -> Scale2
      | ImmNum (4L, _) -> Scale4
      | ImmNum (8L, _) -> Scale8
      | _ -> parse_error "imm_to_scale"
    in

    let (d, b, i, s), ts = match ts with
    | LParen :: RegTok base :: RParen :: ts -> (* (base) *)
      ((None, Some base, None, None), ts)
    | LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts -> (* (base, index) *)
      ((None, Some base, Some index, None), ts)
    | LParen :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* (, index, scale) *)
      ((None, None, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: RParen :: ts -> (* disp(base) *)
      ((Some disp, Some base, None, None), ts)
    | LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* (base, index, scale) *)
      ((None, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: RParen :: ts ->  (* disp(base, index) *)
      ((Some disp, Some base, Some index, None), ts)
    | ImmTok (disp, _) :: LParen :: RegTok base :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(base, index, scale) *)
      ((Some disp, Some base, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: Comma :: RegTok index :: Comma :: ImmTok (imm, _) :: RParen :: ts ->  (* disp(, index, scale) *)
      ((Some disp, None, Some index, Some (imm_to_scale imm)), ts)
    | ImmTok (disp, _) :: LParen :: Comma :: RegTok index :: RParen :: ts ->  (* disp(, index) *)
      ((Some disp, None, Some index, Some Scale1), ts)
    | ImmTok (disp, _) :: LParen :: RipTok :: RParen :: ts -> (* disp(rip) *)
      ((Some disp, None, None, None), ts)
    | _ -> parse_error ("parse_memory_operand: " ^ (String.concat ", " (List.map string_of_token ts)))
    in
    MemOp (d, b, i, s, None), ts

  let parse_operand (ts: token list) : Isa.operand * token list =
    (* ImmOp and XMM RegOp do not have offset/size info yet *)
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

  (* try to obtain instruction's data size through mnemonic suffix *)
  let split_opcode_size
      (opcode_list: string list) (* list of valid mnemonics *)
      (mnemonic: string)
      : string * Isa.data_size =
    let helper (opcode: string) : (string * Isa.data_size) option =
      if String.starts_with mnemonic ~prefix:opcode then
        let suffix = String.sub mnemonic (String.length opcode) (String.length mnemonic - String.length opcode) in
        let op_size = match suffix with
        | "" -> parse_error (Printf.sprintf "split_opcode_size: expecting size suffix for %s" mnemonic)
        | "b" -> 1L
        | "w" -> 2L
        | "l" | "d" -> 4L
        | "q" -> 8L
        | _ -> parse_error (Printf.sprintf
            "split_opcode_size: failed when extracting %s's data size: %s + %s"
            mnemonic
            opcode
            suffix
          )
        in
        Some (opcode, op_size)
      else
        None
    in
    match List.find_map helper opcode_list with
    | Some result -> result
    | None -> parse_error ("split_opcode_size cannot parse " ^ mnemonic)

  let fill_os_for_opr (suggested: Isa.data_off_size) (opr: Isa.operand) : Isa.operand =
    let complain () =
      parse_error (Printf.sprintf "%s not compatible with %s"
        (Isa.string_of_operand opr)
        (Isa.string_of_data_off_size (Some suggested))
      )
    in
    let sz_none_or_compatible (suggested: Isa.data_off_size) (sz_opt: Isa.data_size option) =
      match sz_opt with
      | None -> true
      | Some sz -> sz = snd suggested
    in
    let os_none_or_compatible (suggested: Isa.data_off_size) (r: Isa.register) (os_opt: Isa.data_off_size option) =
      let suggested = match Isa.is_xmm r with
      | true -> suggested
      | false ->
        (* offset relaxed *)
        let reg_off, _ = Isa.get_reg_offset_size r in
        let _, size = suggested in (reg_off, size)
      in
      match os_opt with
      | None -> true
      | Some os -> os = suggested
    in

    match opr with
    | LabelOp _ -> opr
    | ImmOp imm ->
      if IsaBasic.get_imm_size imm |> sz_none_or_compatible suggested then
        ImmOp (IsaBasic.set_imm_size imm (Isa.data_off_size_to_size (Some suggested)))
      else
        complain ()
    | MemOp mem_op ->
      if Isa.get_mem_op_size mem_op |> sz_none_or_compatible suggested then
        MemOp (Isa.set_mem_op_size mem_op (Isa.data_off_size_to_size (Some suggested)))
      else
        complain ()
    | RegOp r ->
      if r |> Isa.get_reg_offset_size_opt |> os_none_or_compatible suggested r then
        RegOp (Isa.fill_os_for_reg r suggested)
      else
        complain ()
    | RegMultOp r_list -> RegMultOp (List.map (fun r ->
        if r |> Isa.get_reg_offset_size_opt |> os_none_or_compatible suggested r then
          Isa.fill_os_for_reg r suggested
        else
          complain ()
      ) r_list)
    | LdOp (_, _, _, _, size, _)
    | StOp (_, _, _, _, size, _) ->
      if size = snd suggested then opr else complain ()
  
  let get_operand_size (opr: Isa.operand) : Isa.data_size =
    match opr with
    | LabelOp _ -> parse_error "unexpected"
    | ImmOp imm -> IsaBasic.get_imm_size imm |> Option.get
    | MemOp mem_op -> Isa.get_mem_op_size mem_op |> Option.get 
    | RegOp r -> r |> Isa.get_reg_size
    | RegMultOp _ -> parse_error "cannot get size of multiple operands"
    | LdOp (_, _, _, _, size, _)
    | StOp (_, _, _, _, size, _) -> size

  let decode_operand_size 
      (mnemonic: string) 
      (operands: Isa.operand list)
      : string * (Isa.operand list) =
    (*
    Printf.printf "decode_operand_size:\n\t%s: %s\n"
      mnemonic
      (String.concat ", " (List.map (fun opr -> Isa.string_of_operand opr) operands));
    *)
    
    let fill (size_list: Isa.data_size list) =
      List.map2 (fun (opr: Isa.operand) (sz: Isa.data_size) ->
        fill_os_for_opr (0L, sz) opr
      ) operands size_list
    in
    let fill' (opr_list: Isa.operand list) (size_list: Isa.data_size list) =
      List.map2 (fun (opr: Isa.operand) (sz: Isa.data_size) ->
        fill_os_for_opr (0L, sz) opr
      ) opr_list size_list
    in
    let fill_opr_os (opr_list: Isa.operand list) (off_size_list: Isa.data_off_size list) =
      List.map2 (fun (opr: Isa.operand) (os: Isa.data_off_size) ->
        fill_os_for_opr os opr
      ) opr_list off_size_list
    in

    match mnemonic with
    (* Handle cases where suffix is not single-letter *)
    (* move with sign-extension *)
    | "movsbq" -> "movs", (fill [1L; 8L])
    | "movswq" -> "movs", (fill [2L; 8L])
    | "movslq" -> "movs", (fill [4L; 8L])
    | "movsbl" -> "movs", (fill [1L; 4L])
    | "movswl" -> "movs", (fill [2L; 4L])
    | "movsbw" -> "movs", (fill [1L; 2L])
    (* move with zero-extension *)
    | "movzbq" -> "movz", (fill [1L; 8L])
    | "movzwq" -> "movz", (fill [2L; 8L])
    | "movzlq" -> "movz", (fill [4L; 8L])
    | "movzbl" -> "movz", (fill [1L; 4L])
    | "movzwl" -> "movz", (fill [2L; 4L])
    | "movzbw" -> "movz", (fill [1L; 2L])
    (* size conversions *)
    | "cbtw" -> "movs", (fill' [Isa.RegOp AL; Isa.RegOp AX] [1L; 2L])
    | "cwtl" -> "movs", (fill' [Isa.RegOp AX; Isa.RegOp EAX] [2L; 4L])
    | "cltq" -> "movs", (fill' [Isa.RegOp EAX; Isa.RegOp RAX] [4L; 8L])
    (* SIMDs *)
    | "xorps" | "xorpd" -> "xorp", (fill [16L; 16L])
    | "pxor" | "pandn" | "pand" | "por" -> mnemonic, (fill [16L; 16L])
    | "pshufb" -> "pshuf", (fill [16L; 16L])
    | "pshufw" | "pshufd" | "pshuflw" | "pshufhw" -> "pshuf", (fill [1L; 16L; 16L])
    | "punpcklbw" | "punpcklwd" | "punpckldq" | "punpcklqdq" -> begin
        if List.length operands != 2 then
          parse_error "punpckl: expecting two xmm operands";
        let opr1 = List.hd operands in
        let opr2 = List.hd (List.tl operands) in
        "punpckl", (fill_opr_os [opr1; opr2; opr2] [(0L, 8L); (0L, 8L); (0L, 16L)])
      end
    | "punpckhbw" | "punpckhwd" | "punpckhdq" | "punpckhqdq" -> begin
        if List.length operands != 2 then
          parse_error "punpckh: expecting two xmm operands";
        let opr1 = List.hd operands in
        let opr2 = List.hd (List.tl operands) in
        "punpckh", (fill_opr_os [opr1; opr2; opr2] [(8L, 8L); (8L, 8L); (0L, 16L)])
      end
    | "packusdw" | "packuswb" -> "packus", (fill [16L; 16L])
    | "packssdw" | "packsswb" -> "packss", (fill [16L; 16L])
    | "movaps" | "movapd" | "movups" | "movupd" -> "mov", (fill [16L; 16L])
    | "movdqu" | "movdqa" -> "mov", (fill [16L; 16L])
    (* instructions with single-letter suffix indicating size: [bwldq] *)
    (* special size *)
    | _ ->
      let opcode, op_size = split_opcode_size Isa.common_opcode_list mnemonic in
      let op_os: Isa.data_off_size = (0L, op_size) in
      let oprs_with_size = match opcode with
      | "sal" | "sar" | "shl" | "shr" when (List.length operands = 2) -> begin
          (* suggest size for dest only *)
          List.map2 (fun (opr: Isa.operand) (sz: Isa.data_size) ->
            fill_os_for_opr (0L, sz) opr
          ) operands [1L; op_size] 
        end
      | _ -> begin
          (* suggest size for each operand *)
          List.map (
            fun (opr: Isa.operand) : Isa.operand -> fill_os_for_opr op_os opr
          ) operands
        end
      in
      opcode, oprs_with_size
  
  let src (opr: Isa.operand) : Isa.operand =
    match opr with
    | MemOp (disp, base, index, scale, Some size) -> LdOp (disp, base, index, scale, size, MemAnno.make_empty ())
    | MemOp (_, _, _, _, None) -> parse_error "no size for mem op"
    | StOp _ -> parse_error "src"
    | LdOp _ -> parse_error "LdOp not expected"
    | opr -> opr

  let dst (opr: Isa.operand) : Isa.operand =
    match opr with
    | MemOp (disp, base, index, scale, Some size) -> StOp (disp, base, index, scale, size, MemAnno.make_empty ())
    | MemOp (_, _, _, _, None) -> parse_error "no size for mem op"
    | LdOp _ -> parse_error "dst"
    | StOp _ -> parse_error "StOp not expected"
    | opr -> opr

  let get_binst_with_one_operand (op: Isa.bop) (opr: Isa.operand) : Isa.instruction =
    let size = opr |> get_operand_size in
    match op with
    | Sal | Sar | Shr | Shl | Rol | Ror -> BInst (op, dst opr, src opr, src (ImmOp (ImmNum (1L, Some size))))
    | Mul | Imul -> begin
        let (mul_src: Isa.operand), (mul_dst: Isa.operand) = match size with
        | 1L -> RegOp AL, RegOp AX
        | 2L -> RegOp AX, RegMultOp [AX; DX]
        | 4L -> RegOp EAX, RegMultOp [EAX; EDX]
        | 8L -> RegOp RAX, RegMultOp [RAX; RDX]
        | _ -> parse_error "unexpected size for mul"
        in
        BInst (op, dst mul_dst, src mul_src, src opr)
      end
    | _ -> parse_error (Printf.sprintf "Cannot get default op for op %s" (Sexplib.Sexp.to_string (Isa.sexp_of_bop op)))

  let get_binst (op: Isa.bop) (operands: Isa.operand list) : Isa.instruction =
    match operands with
    | [opr1] -> 
      get_binst_with_one_operand op opr1
    | [opr1; opr2] ->
      BInst (op, dst opr2, src opr2, src opr1)
    | [opr1; opr2; opr3] ->
      BInst (op, dst opr3, src opr2, src opr1)
    | _ -> parse_error ("get_binst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let get_uinst (op: Isa.uop) (operands: Isa.operand list) : Isa.instruction =
    match op, operands with
    | Lea, [opr1; opr2] -> UInst (op, dst opr2, opr1)
    | _, [opr1; opr2] -> UInst (op, dst opr2, src opr1)
    | _, [opr] -> UInst (op, dst opr, src opr)
    | _ -> parse_error ("get_uinst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let get_tinst (op: Isa.top) (operands: Isa.operand list) : Isa.instruction =
    let num_operands = List.length operands in
    let rev_operands = List.rev operands in
    if num_operands >= 3 then
      TInst (op, dst (List.hd rev_operands), List.map src rev_operands)
    else
      parse_error ("get_tinst: invalid number of operands " ^ (string_of_int (List.length operands)))

  let parse_tokens
      (imm_var_map: Isa.imm_var_map)
      (ts: token list)
      : (Isa.imm_var_map * (IsaBasic.imm_var_id list)) * (Isa.instruction * string (* mnemonic *)) =
    (* handle directives specially *)
    let directive_opt = match ts with
    | Raw raw_line :: [] ->
      Some (imm_var_map, (Isa.Directive raw_line, raw_line)) (* mnemonic is the whole line *)
    | _ -> None
    in
    if Option.is_some directive_opt then begin
      let imm_var_map, (inst, mnemonic) = Option.get directive_opt in
      (imm_var_map, []), (inst, mnemonic)
    end else

    (* handle instructions *)
    let (mnemonic, ts) = match ts with
    | MneTok mnemonic :: ts -> (mnemonic, ts)
    | _ -> parse_error ("parse_tokens: unexpected end of input: " ^ (String.concat ", " (List.map string_of_token ts)))
    in
    let imm_var_map, referred_imm_vars, operands =
      if Isa.inst_with_single_label mnemonic then
        imm_var_map, [], [convert_imm_to_label ts]
      else begin 
        (* replace name with imm_var_id; allocate new id if not existing in the map *)
        let rec fill_id_helper 
            (info: Isa.imm_var_map * (IsaBasic.imm_var_id list))
            (imm_name: IsaBasic.immediate * (string option) list)
            : (IsaBasic.imm_var_map * (IsaBasic.imm_var_id list)) * IsaBasic.immediate =
          let imm_var_map, referred_imm_vars = info in
          match imm_name with
          | ImmNum (n, sz), None :: [] -> ((imm_var_map, referred_imm_vars), ImmNum (n, sz))
          | ImmLabel (v, sz), (Some name) :: [] ->
            if v <> IsaBasic.imm_var_unset then ((imm_var_map, v :: referred_imm_vars), ImmLabel (v, sz))
            else begin
              if Isa.StrM.mem name imm_var_map
              then begin
                let id = Isa.StrM.find name imm_var_map in
                ((imm_var_map, id :: referred_imm_vars), ImmLabel (id, None))
              end else begin
                (* let id = CodeType.stack_base_id + 1 + (Isa.StrM.cardinal imm_var_map) in *)(* id starts from stack_base_id + 1 *)
                let id = IsaBasic.imm_var_unset - 1 - (Isa.StrM.cardinal imm_var_map) in (* id starts from imm_var_unset, going negative!!! *)
                Printf.printf "new imm_var: %s %d\n" name id;
                ((Isa.StrM.add name id imm_var_map, id :: referred_imm_vars), ImmLabel (id, None))
              end
            end
          | ImmBExp( (i1, i2), sz), hd :: tl ->
            let (imm_var_map1, referred_imm_vars), new_i1 = fill_id_helper (imm_var_map, referred_imm_vars) (i1, [ hd ]) in
            let (imm_var_map2, referred_imm_vars), new_i2 = fill_id_helper (imm_var_map1, referred_imm_vars) (i2, tl) in
            ((imm_var_map2, referred_imm_vars), Isa.ImmBExp ((new_i1, new_i2), sz))
          | _ -> parse_error "parse_tokens fail to generate imm_var_id"
        in
        let (imm_var_map, referred_imm_vars), ts = List.fold_left_map (fun info token ->
          match token with
          | ImmTok (imm, name_list) ->
            let (new_imm_var_map, referred_imm_vars), new_imm = fill_id_helper info (imm, name_list) in
            ((new_imm_var_map, referred_imm_vars), ImmTok (new_imm, name_list))
          | _ -> (info, token)
        ) (imm_var_map, []) ts
        in
        (* parse tokens into operands *)
        imm_var_map, referred_imm_vars, parse_operands ts
      end
    in

    (* construct instruction *)
    let inst: Isa.instruction = 
      match (mnemonic, operands) with
      | ("syscall", []) -> Syscall
      | ("hlt", []) -> Hlt
      | ("nop", []) -> Nop
      (* rep instructions *)
      | ("rep", [LabelOp label]) ->
        begin match split_opcode_size IsaBasic.rep_opcode_list label with
        | "movs", size ->
          RepMovs (size, MemAnno.make_empty (), MemAnno.make_empty ())
        | "lods", size ->
          RepLods (size, MemAnno.make_empty ())
        | "stos", size ->
          RepStos (size, MemAnno.make_empty ())
        | _ -> parse_error (Printf.sprintf "Cannot parse rep op for %s" label)
        end
      (* function routines *)
      | ("call", [LabelOp lb])
      | ("callq", [LabelOp lb]) -> Call (lb, None)
      | ("ret", []) | ("retq", []) -> Jmp (Isa.ret_label, None) 
      (* jumps *)
      | ("jmp", [LabelOp lb]) -> Jmp (lb, None)
      | (_, [LabelOp lb]) ->
        begin match Isa.op_of_cond_jump mnemonic with
        | Some op -> Jcond (op, lb, None)
        | None -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
        end
      (* common instructions with operands *)
      | _ ->
        let opcode, oprs_with_size = decode_operand_size mnemonic operands in
        begin match Isa.op_of_binst opcode, Isa.op_of_uinst opcode, Isa.op_of_tinst opcode with
        | Some op, None, None -> get_binst op oprs_with_size
        | None, Some op, None -> get_uinst op oprs_with_size
        | None, None, Some op -> get_tinst op oprs_with_size
        | None, None, None ->
          begin match opcode, oprs_with_size with
          | "xchg", [opr1; opr2] -> Xchg (dst opr2, dst opr1, src opr2, src opr1)
          | "push", [opr] -> Push ((src opr), (MemAnno.make_empty ()))
          | "pop", [opr] -> Pop ((dst opr), (MemAnno.make_empty ()))
          | "cmp", [opr1; opr2] -> Cmp (src opr2, src opr1)
          | "test", [opr1; opr2] -> Test (src opr2, src opr1)
          | _ -> parse_error ("parse_tokens: invalid instruction " ^ mnemonic)
          end
        | _ -> parse_error "unexpected opcode"
        end
    in
    (imm_var_map, referred_imm_vars), (inst, mnemonic)

  let parse_instr
      (imm_var_map: Isa.imm_var_map)
      (line: string)
      : (Isa.imm_var_map * (IsaBasic.imm_var_id list)) * (Isa.instruction * string (* mnemonic *)) =
    (* print_endline ("Parsing " ^ line); *)
    let tokens = tokenize_line line in
    (* print_endline (String.concat ", " (List.map string_of_token tokens)); *)
    parse_tokens imm_var_map tokens

  let parse_basic_block
      (info: Isa.imm_var_map * (Isa.label option) * (IsaBasic.imm_var_id list))
      (lines: string list)
      : (Isa.imm_var_map * (Isa.label option) * (IsaBasic.imm_var_id list)) * Isa.basic_block =
    let imm_var_map, gsym_name, referred_gsyms = info in

    (* try to extract label & global symbol name *)
    let label, gsym_name, body_lines =
      let hd_line = List.hd lines in
      match Isa.get_label_in_line hd_line, Isa.get_global_label_in_line hd_line with
      | None, None ->
        (* first line is not a label --> directives at head of file *)
        if Option.is_some gsym_name then
          parse_error "multiple global symbol label candidate";
        Isa.sentry_func_label, Some Isa.sentry_func_label, lines
      | Some label, None -> label, gsym_name, List.tl lines
      | Some label, Some gsym ->
        if Option.is_some gsym_name then 
          parse_error "multiple global symbol label candidate";
        label, Some gsym, List.tl lines
      | _ -> parse_error "unexpected when extracting label for bb"
    in

    (* parse each instruction in the BB *)
    let (imm_var_map, new_referred_gsyms), insts_with_mne = List.fold_left_map (
      fun (imm_var_map, acc_gsyms) line ->
        let (imm_var_map, gsyms), result = parse_instr imm_var_map line in
        (imm_var_map, gsyms @ acc_gsyms), result
    ) (imm_var_map, []) body_lines in
    let insts, mnemonics = List.split insts_with_mne in

    (imm_var_map, gsym_name, new_referred_gsyms @ referred_gsyms), {
      label = label; 
      insts = insts;
      mnemonics = mnemonics;
      orig_asm = List.map (fun x -> Some x) lines;
    }

  let line_processor (line: string) : string list =
    (* every line is trimmed; asm-gen needs to add spaces carefully *)
    let line = String.trim line in
    if line = "" then [] else

    (* leave directive lines untouched *)
    if Isa.line_is_directive line then [line] else

    (* remove comments *)
    let comment_pattern = Str.regexp {|^\(\(\([^#"']\)\|\("\([^"]\|\\"\)*"\)\|\('\([^']\|\\'\)*'\)\)*\)\(#.*\)?$|} in
    let line = match Str.string_match comment_pattern line 0 with
    | false -> line
    | true ->
      let noncomm_end = Str.group_end 1 in
      String.sub line 0 noncomm_end |> String.trim
    in
    if line = "" then [] else

    let one_instr_pattern = Str.regexp {|^\(\(\([^;"']\)\|\("\([^"]\|\\"\)*"\)\|\('\([^']\|\\'\)*'\)\)*\)\(;.*\)$|} in
    let rec split_sub_instrs (acc: string list) (curr: string) =
      match Str.string_match one_instr_pattern curr 0 with
      | false -> curr :: acc |> List.rev
      | true ->
        let inst_end = Str.group_end 1 in
        let inst = String.sub curr 0 inst_end in
        let curr' = String.sub curr (inst_end + 1) ((String.length curr) - inst_end - 1) in
        split_sub_instrs (inst :: acc) curr'
    in
    line |> split_sub_instrs [] |> List.filter_map (fun (line: string) ->
      (* trim again *)
      let line = String.trim line in
      if line = "" then None else Some line
    )

  let split_helper split_here lines =
    let add_group curr_group acc_groups =
      if curr_group = [] then
        acc_groups
      else
        (List.rev curr_group) :: acc_groups
    in
    let rec helper split_here curr_group acc_groups lines =
      match lines with
      | [] -> add_group curr_group acc_groups
      | line :: lines ->
        if split_here line then
          helper split_here [line] (add_group curr_group acc_groups) lines
        else
          helper split_here (line :: curr_group) acc_groups lines
    in
    helper split_here [] [] lines |> List.rev

  let parse_program (source: string) : Isa.prog =
    let lines = source
      |> String.split_on_char '\n'
      |> List.concat_map (fun line -> line_processor line)
    in

    (* divide linees into groups based on BB and global symbols *)
    let lines_of_bbs_of_gsyms =
      (* split by global symbols *)
      split_helper Isa.line_is_global_label lines |>
      (* within each global symbol, split by labels to get BBs *)
      List.map (fun lines_of_gs ->
        split_helper Isa.line_is_label lines_of_gs
      )
    in

    (* parse each BB in each global symbol *)
    let imm_var_map, func_list =
      let gsym_helper 
          (imm_var_map: Isa.imm_var_map) (lines_of_bbs: string list list)
          : Isa.imm_var_map * Isa.func =
        let (imm_var_map, gsym_name, referred_gsyms), bbs =
          List.fold_left_map parse_basic_block (imm_var_map, None, []) lines_of_bbs
        in

        (* debug print *)
        let rev_imm_var_map = Isa.get_rev_imm_var_map imm_var_map in
        let referred_gsyms = referred_gsyms
          |> List.sort_uniq (fun (x: IsaBasic.imm_var_id) (y: IsaBasic.imm_var_id) -> Int.compare x y)
          |> List.map (fun var_id -> Isa.IntM.find var_id rev_imm_var_map)
        in
        Printf.printf "symbols in %s\n\t" (Option.get gsym_name);
        List.iter (fun symbol -> Printf.printf "%s " symbol) referred_gsyms;
        Printf.printf "\n%!";

        (* subfunctions extraction *)
        let callees = List.fold_left (fun acc (bb: Isa.basic_block) ->
          let callees = List.filter_map (fun (inst: Isa.instruction) ->
            match inst with
            | Call (subfunc, _) -> Some subfunc
            | _ -> None
          ) bb.insts
          in
          callees @ acc
        ) [] bbs
        in
        let callees = List.sort_uniq (fun x y -> String.compare x y) callees in
        List.iter (fun subfunc -> Printf.printf "%s calls to %s\n" (Option.get gsym_name) subfunc) callees;

        imm_var_map, {
          name = Option.get gsym_name;
          body = bbs;
          related_gsyms = referred_gsyms; (* initialization; propagate later *)
          subfunctions = callees; (* initialization; propagate later *)
        }
      in
      List.fold_left_map gsym_helper Isa.StrM.empty lines_of_bbs_of_gsyms
    in

    (* related gsymbols propagation *)
    let same_func_list (l1: Isa.func list) (l2: Isa.func list) =
      List.fold_left2 (fun acc (f1: Isa.func) (f2: Isa.func) ->
        if not (String.equal f1.name f2.name) then
          parse_error "func list invalid";
        let cmp = (0 = List.compare (fun a b -> String.compare a b) f1.related_gsyms f2.related_gsyms) in
        if cmp = false then false else acc
      ) true l1 l2
    in
    let rec symbol_propagate_helper (func_list: Isa.func list) : Isa.func list =
      let old_list = func_list in

      let func_list = List.map (fun (func: Isa.func) ->
        let related_gsyms = List.fold_left (fun acc (subfunc: Isa.label) ->
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
            ) subfunc.related_gsyms
            in
            List.iter (fun s -> Printf.printf "adding %s to %s (from %s)\n" s func.name subfunc.name) missing;
            missing @ acc
        ) func.related_gsyms func.subfunctions in
        {func with related_gsyms = related_gsyms }
      ) func_list in

      if same_func_list old_list func_list then
        old_list
      else
        symbol_propagate_helper func_list
    in
    let func_list = symbol_propagate_helper func_list in
    List.iter (fun (func: Isa.func) ->
      Printf.printf "global symbol of %s:\n" func.name;
      List.iter (fun symbol -> Printf.printf "\t%s\n" symbol) func.related_gsyms;
    ) func_list;

    (* add jmp for two adjacent basic blocks if the previous one does not end with ret *)
    let rec add_jmp_for_adj_bb (bbs: Isa.basic_block list) (acc: Isa.basic_block list) =
      match bbs with
      | [] -> List.rev acc
      | bb :: [] -> List.rev (bb :: acc)
      | bb1 :: bb2 :: bbs ->
          (* find the real last instruction, skipping directives *)
          let last_inst = List.find_opt (fun (inst: Isa.instruction) ->
            match inst with
            | Directive _ -> false
            | _ -> true
          ) (List.rev bb1.insts)
          in

          let bb1 = if List.length bb1.insts > 0 && Option.is_some last_inst && Isa.inst_is_uncond_jump (Option.get last_inst)
            then bb1
            else {bb1 with insts = bb1.insts @ [Jmp (bb2.label, None)]; mnemonics = bb1.mnemonics @ ["jmp"]; orig_asm = bb1.orig_asm @ [None] }
          in
          add_jmp_for_adj_bb (bb2 :: bbs) (bb1 :: acc)
    in
    let func_list = List.map (
      fun (func: Isa.func) ->
        { func with body = add_jmp_for_adj_bb func.body [] }
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

open Parser
open Isa
open Full_mem_anno
open Transform

module AsmGen = struct

  exception AsmGenError of string
  let asm_gen_error msg = raise (AsmGenError ("[AsmGen Error] " ^ msg))

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  type context = {
    global_var_map: Isa.imm_var_rev_map;
  }

  let is_src_same_as_dst (dst: Isa.operand) (src: Isa.operand) : bool =
    Isa.cmp_operand (Isa.extract_memop_of_ldst dst) (Isa.extract_memop_of_ldst src)

  let str_of_immediate (ctx: context) (prefix: bool) (imm: Isa.immediate) : string =
    (if prefix then "$" else "")
    ^
    match imm with
    | ImmNum x -> (Int64.to_string x)
    | ImmLabel label -> Isa.IntM.find label ctx.global_var_map
    | _ -> asm_gen_error "str_of_immediate: not implemented"

  let str_of_memop (ctx: context) (d: Isa.immediate option) (b: Isa.register option) (i: Isa.register option) (s: Isa.scale option) : string =
    let str_of_disp (d: Isa.immediate option) : string = 
      match d with
      | None -> ""
      | Some d -> str_of_immediate ctx false d
    in
    match d, b, i, s with
    | _, Some base, None, None -> Printf.sprintf "%s(%s)" (str_of_disp d) (Isa.string_of_reg base)
    | None, Some base, Some index, Some scale -> Printf.sprintf "(%s, %s, %s)" (Isa.string_of_reg base) (Isa.string_of_reg index) (Isa.scale_to_string scale)
    | Some _, None, None, None -> Printf.sprintf "%s" (str_of_disp d)
    | Some _, None, Some index, Some scale -> Printf.sprintf "%s(, %s, %s)" (str_of_disp d) (Isa.string_of_reg index) (Isa.scale_to_string scale)
    (* TODO: support more cases *)
    | _ -> asm_gen_error "str_of_memop: not implemented"

  let str_of_operand (ctx: context) (op: Isa.operand) : string =
    match op with
    | ImmOp imm -> str_of_immediate ctx true imm
    | RegOp reg -> Isa.string_of_reg reg
    | LdOp (d, b, i, s, _, _)
    | StOp (d, b, i, s, _, _) -> str_of_memop ctx d b i s
    | LabelOp label -> label
    | MemOp _ -> asm_gen_error "str_of_operand: MemOp not implemented/expected"

  let str_of_binst_operands (ctx: context) (_: Isa.bop) (dst: Isa.operand) (src2: Isa.operand) (src1: Isa.operand) : string =
    let str_of_operand' = str_of_operand ctx in
    if is_src_same_as_dst dst src1 then
      Printf.sprintf "%s, %s" (str_of_operand' dst) (str_of_operand' src2)
    else if is_src_same_as_dst dst src2 then
      Printf.sprintf "%s, %s" (str_of_operand' dst) (str_of_operand' src1)
    else
      asm_gen_error "str_of_binst_operands: dst is not the same as src1 or src2" (* may have 3 distinct operands? *)

  let str_of_uinst_operands (ctx: context) (_: Isa.uop) (dst: Isa.operand) (src: Isa.operand) : string =
    Printf.sprintf "%s, %s" (str_of_operand ctx dst) (str_of_operand ctx src)
  
  let str_of_operands (ctx: context) (oprs: Isa.operand list) : string =
    String.concat ", " (List.map (str_of_operand ctx) oprs) (* TODO: check if need reverse *)

  let asm_of_inst (ctx: context) (inst: Isa.instruction) (mnemonic: string option) : string =
    let get_tab (opcode: string): string =
      if String.length opcode <= 3 then "\t\t" else "\t"
    in

    let mnemonic = match mnemonic with
    | Some m -> m
    | None -> Isa.mnemonic_of_instruction inst
    in

    let str_operands = match inst with

    | BInst (bop, dst, src2, src1) -> str_of_binst_operands ctx bop dst src2 src1
    | UInst (uop, dst, src) -> str_of_uinst_operands ctx uop dst src

    | Cmp (src2, src1)
    | Test (src2, src1) -> str_of_operands ctx [src1; src2]
    | Xchg (dst2, dst1, _, _) -> str_of_operands ctx [dst1; dst2]

    | Nop
    | Syscall
    | Hlt -> ""

    | Jmp label
    | Jcond (_, label)
    | Call (label, _) -> label

    | Push (src, _)
    | Pop (src, _) -> str_of_operand ctx src

    | RepStosq
    | RepMovsq -> asm_gen_error "not implemented"

    in

    Printf.sprintf "\t%s%s%s" mnemonic (get_tab mnemonic) str_operands

  let asm_of_inst_tf (ctx: context) (it: InstTransform.t) : string list =
    if it.changed then
      let lines = [""] in
      let lines = ("#" ^ it.orig_asm ^ "\t; <==was now==>") :: lines in
      let lines = List.fold_left (fun acc_lines inst ->
        (asm_of_inst ctx inst None) :: acc_lines
      ) lines it.inst_pre in
      let lines = (asm_of_inst ctx it.inst (Some it.mnemonic)) :: lines in
      let lines = List.fold_left (fun acc_lines inst ->
        (asm_of_inst ctx inst None) :: acc_lines
      ) lines it.inst_post in
      let lines = "" :: lines in
      lines
    else
      [it.orig_asm]

  let asm_of_basic_block (ctx: context) (bb: Transform.basic_block) : string list =
    let lines = [ Printf.sprintf "%s:" bb.label ] in
    let lines = List.fold_left (fun acc_lines it ->
      (asm_of_inst_tf ctx it) @ acc_lines
    ) lines bb.insts in
    (* top list where lines are still reversed *)
    List.rev lines

  let get_rev_imm_var_map (global_var_map: Isa.imm_var_map) : Isa.imm_var_rev_map =
    let kv_list = Isa.StrM.to_list global_var_map in
    let vk_map = List.fold_left (fun acc_map (k, v) ->
      if Isa.IntM.mem v acc_map then
        transform_error "Unexpected duplicated value in imm_var_map";
      Isa.IntM.add v k acc_map
    ) Isa.IntM.empty kv_list in
    vk_map

  let gen_asm (prog: Isa.prog) (tf_func_states: Transform.func_state list) : string =
    let ctx = { global_var_map = get_rev_imm_var_map prog.imm_var_map } in

    prog.orig_lines
    |> Parser.preprocess_lines (Parser.line_processor true false) (* format preserved, a line must be trimmed before use *)
    |> Parser.spliter_helper Parser.is_label [] []
    |> List.map (fun (func_lines: string list) : string list ->
      let func_name = Parser.parse_label (List.hd func_lines |> String.trim) in
      if not (Isa.is_label_function_entry func_name) then
        asm_gen_error (Printf.sprintf "expecting a function entry label, got %s" func_name);
      let func_states = List.find (fun (fs: Transform.func_state) -> fs.func_name = func_name) tf_func_states in
      if not (Transform.is_func_transformed func_states) then
        func_lines
      else
        let lines_of_bbs = Parser.spliter_helper Parser.is_label [] [] func_lines in
        List.map (fun (bb_lines: string list) : string list ->
          let bb_label = Parser.parse_label (List.hd bb_lines |> String.trim) in
          let bb_state = List.find (fun (bb: Transform.basic_block) -> bb.label = bb_label) func_states.bbs in
          if not (Transform.is_bb_transformed bb_state) then
            bb_lines
          else
            asm_of_basic_block ctx bb_state
        ) lines_of_bbs
        |> List.flatten
    )
    |> List.flatten
    |> String.concat "\n"

end

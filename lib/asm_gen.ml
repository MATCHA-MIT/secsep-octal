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
    let str_of_reg_option (reg_option: Isa.register option): string =
      match reg_option with
      | None -> ""
      | Some reg -> "%" ^ (Isa.string_of_reg reg)
    in
    let str_of_scale_option (scale_option: Isa.scale option): string =
      match scale_option with
      | None -> ""
      | Some scale -> Isa.scale_to_string scale
    in
    match d, b, i, s with
    | _, Some base, None, None -> Printf.sprintf "%s(%%%s)" (str_of_disp d) (Isa.string_of_reg base)
    | None, Some base, Some index, Some scale -> Printf.sprintf "(%%%s, %%%s, %s)" (Isa.string_of_reg base) (Isa.string_of_reg index) (Isa.scale_to_string scale)
    | Some _, None, None, None -> Printf.sprintf "%s" (str_of_disp d)
    | Some _, None, Some index, Some scale -> Printf.sprintf "%s(, %%%s, %s)" (str_of_disp d) (Isa.string_of_reg index) (Isa.scale_to_string scale)
    | Some _, Some base, Some index, None -> Printf.sprintf "%s(%%%s, %%%s, )" (str_of_disp d) (Isa.string_of_reg base) (Isa.string_of_reg index)
    | Some _, _, _, _ -> Printf.sprintf "%s(%s, %s, %s)" (str_of_disp d) (str_of_reg_option b) (str_of_reg_option i) (str_of_scale_option s)
    (* TODO: support more cases *)
    | _ -> asm_gen_error (Printf.sprintf "str_of_memop: not implemented: %s\n" (Isa.string_of_operand(Isa.MemOp(d, b, i, s))))

  let str_of_operand (ctx: context) (op: Isa.operand) : string =
    match op with
    | ImmOp imm -> str_of_immediate ctx true imm
    | RegOp reg -> "%" ^ (Isa.string_of_reg reg)
    | LdOp (d, b, i, s, _, _)
    | StOp (d, b, i, s, _, _) -> 
      (* Printf.printf "operand = %s\n%!" (Isa.string_of_operand op); *)
      str_of_memop ctx d b i s
    | LabelOp label -> label
    | MemOp _ -> asm_gen_error "str_of_operand: MemOp not implemented/expected"

  let str_of_tinst_operands (ctx: context) (_: Isa.top) (dst: Isa.operand) (oprs: Isa.operand list) : string =
    let _, _, _ = ctx, dst, oprs in
    asm_gen_error "str_of_tinst_operands: not implemented"

  let str_of_binst_operands (ctx: context) (bop: Isa.bop) (dst: Isa.operand) (src2: Isa.operand) (src1: Isa.operand) : string =
    let str_of_operand' = str_of_operand ctx in
    if bop = Isa.Mul then begin
      Printf.sprintf "%s" (str_of_operand' src1)
    end else
    if is_src_same_as_dst dst src1 then
      Printf.sprintf "%s, %s" (str_of_operand' src2) (str_of_operand' dst)
    else if is_src_same_as_dst dst src2 then
      Printf.sprintf "%s, %s" (str_of_operand' src1) (str_of_operand' dst)
    else
      Printf.sprintf "%s, %s, %s" (str_of_operand' src1) (str_of_operand' src2) (str_of_operand' dst)

  let str_of_uinst_operands (ctx: context) (_: Isa.uop) (dst: Isa.operand) (src: Isa.operand) : string =
    Printf.sprintf "%s, %s" (str_of_operand ctx src) (str_of_operand ctx dst)
  
  let str_of_operands (ctx: context) (oprs: Isa.operand list) : string =
    String.concat ", " (List.map (str_of_operand ctx) oprs) (* TODO: check if need reverse *)

  let get_tab (opcode: string): string =
    if String.length opcode <= 3 then "\t\t" else "\t"

  let asm_of_inst (ctx: context) (inst: Isa.instruction) (mnemonic: string option) : string =
    let get_tab (opcode: string): string =
      if String.length opcode <= 3 then "\t\t" else "\t"
    in

    let mnemonic = match mnemonic with
    | Some m -> m
    | None -> Isa.mnemonic_of_instruction inst
    in

    let str_operands = match inst with

    | TInst (top, dst, oprs) -> str_of_tinst_operands ctx top dst oprs
    | BInst (bop, dst, src2, src1) -> str_of_binst_operands ctx bop dst src2 src1
    | UInst (uop, dst, src) -> str_of_uinst_operands ctx uop dst src

    | Cmp (src2, src1)
    | Test (src2, src1) -> str_of_operands ctx [src1; src2]
    | Xchg (dst2, dst1, _, _) -> str_of_operands ctx [dst1; dst2]

    | Nop
    | Syscall
    | Hlt -> ""

    | Jmp (label, _)
    | Jcond (_, label, _)
    | Call (label, _) -> label

    | Push (src, _)
    | Pop (src, _) -> str_of_operand ctx src

    | RepMovs _
    | RepLods _
    | RepStos _ -> asm_gen_error "not implemented"

    | Annotation _ -> "" (* annotation is in mnemonic *)

    in

    Printf.sprintf "\t%s%s%s" mnemonic (get_tab mnemonic) str_operands

  let asm_of_inst_tf (ctx: context) (it: InstTransform.t) (last_changed: bool): bool * (string list) =
    match it.orig_asm with
    | None -> (false, [])
    | Some orig_asm ->
      if it.changed then
        let lines = if last_changed then [] else [""] in
        let lines = ("#   " ^ orig_asm) :: lines in
        let lines = List.fold_left (fun acc_lines inst ->
          (asm_of_inst ctx inst None) :: acc_lines
        ) lines it.inst_pre in
        let lines = (asm_of_inst ctx it.inst (if it.use_orig_mne then Some it.mnemonic else None)) :: lines in
        let lines = List.fold_left (fun acc_lines inst ->
          (asm_of_inst ctx inst None) :: acc_lines
        ) lines it.inst_post in
        let lines = "" :: lines in
        (true, lines)
      else if it.failed then
        (false, [""; "#   " ^ orig_asm; ""])
      else
        (false, ["\t" ^ orig_asm])

  let asm_of_basic_block (ctx: context) (bb: Transform.basic_block) : string list =
    let lines = [ Printf.sprintf "%s:" bb.label ] in
    let lines, _ = List.fold_left (fun acc it ->
      let acc_lines, changed = acc in
      let changed', lines = asm_of_inst_tf ctx it changed in
      lines @ acc_lines, changed'
    ) (lines, true) bb.insts in
    (* top list where lines are still reversed *)
    List.rev lines

  let gen_asm (prog: Isa.prog) (tf_func_states: Transform.func_state list) : string =
    let ctx = { global_var_map = Isa.get_rev_imm_var_map prog.imm_var_map } in

    List.map (fun (func: Isa.func) ->
      let helper_bb_orig_lines (bb: Isa.basic_block) =
        List.filter_map (fun line ->
          match line with
          | None -> None
          | Some line ->
            let line = String.trim line in
            if Isa.is_label line then
              Some line
            else
              Some ("\t" ^ line)
        ) bb.orig_asm
      in
      let helper_sentry_exist (func_lines: string list) : string option =
        match List.find_opt (fun x -> String.equal (x |> String.trim) (Isa.fake_bb_sentry_label)) func_lines with
        | Some _ ->
          let first_label = List.hd func_lines in
          if not (Isa.is_label_function_entry first_label) then
            asm_gen_error "expecting first line to be a label when sentry exists";
          Printf.printf "first_label = %s" first_label;
          Some first_label
        | None -> None
      in

      let helper_remove_sentry (sentry_replacing: string option) (lines: string list) =
        List.filter_map (fun line ->
          match sentry_replacing, line with
          | Some real_entry, x when (String.equal x (Isa.fake_bb_sentry_label)) -> Some real_entry
          | Some real_entry, x when (String.equal x real_entry) -> None
          | _ -> Some line
        ) lines
      in
      let func_state = List.find_opt (fun (fs: Transform.func_state) -> fs.func_name = func.name) tf_func_states in

      let func_orig_lines = List.map (fun (bb: Isa.basic_block) ->
        helper_bb_orig_lines bb
      ) func.body |> List.flatten in
      let sentry_info = helper_sentry_exist func_orig_lines in
      let func_orig_lines = helper_remove_sentry sentry_info func_orig_lines in

      if Option.is_none func_state then func_orig_lines else
      let func_state = Option.get func_state in
      if not (Transform.is_func_transformed func_state) then func_orig_lines else

      List.map (fun (bb: Isa.basic_block) ->
        let bb_state = List.find (fun (tf_bb: Transform.basic_block) -> tf_bb.label = bb.label) func_state.bbs in
        if not (Transform.is_bb_transformed bb_state) then
          helper_bb_orig_lines bb
        else
          asm_of_basic_block ctx bb_state
      ) func.body
      |> List.flatten
      |> helper_remove_sentry sentry_info
    ) prog.funcs
    |> List.flatten
    |> String.concat "\n"

end

(* open Type.Isa *)
(* open Type.Set_sexp *)
open Type.Smt_emitter
(* open Z3_sexp *)
open Basic_type
(* open Mem_anno *)
open Stack_spill_info
open Reg_type
open Flag_type
open Mem_type
open Mem_anno
open Arch_type_basic
open Func_interface
(* open Z3 *)

module ArchType = struct
include ArchTypeBasic

  let get_mem_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) (mem_op: Isa.mem_op) : entry_t =
    let ctx, _ = smt_ctx in
    (* I ignore mem_op size since it should always be 8! *)
    let disp, base, index, scale, _ = mem_op in
    let disp =
      match disp with
      | Some disp_imm -> DepType.get_imm_exp ctx disp_imm
      | None -> DepType.get_const_exp ctx 0L 64
    in
    let base =
      match base with
      | Some base_r -> RegType.get_reg_type ctx curr_type.reg_type base_r
      | None -> BasicType.get_const_type ctx 0L 64
    in
    let index =
      match index with
      | Some index_r -> RegType.get_reg_type ctx curr_type.reg_type index_r
      | None -> BasicType.get_const_type ctx 0L 64
    in
    let scale =
      match scale with
      | Some s -> DepType.get_const_exp ctx (Isa.scale_val s) 64
      | None -> DepType.get_const_exp ctx 1L 64
    in
    BasicType.get_mem_op_type ctx disp base index scale

  let get_ld_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) (ld_op: Isa.ldst_op) : entry_t option =
    (* <TODO> Check this against infer and ld rule, make sure it checks everything *)
    let disp, base, index, scale, size, (slot_anno, taint_anno) = ld_op in
    let addr_dep_type, addr_taint_type =
      get_mem_op_type smt_ctx curr_type (disp, base, index, scale, None)
    in
    if not (TaintType.check_untaint smt_ctx addr_taint_type) then begin
      Printf.printf "get_ld_op_type op %s taint addr %s\n"
        (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand (LdOp ld_op)))
        (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
      None
    end else begin
      match addr_dep_type with
      | Top _ ->
        Printf.printf "get_ld_op_type op %s addr is top\n"
          (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
        None
      | Exp addr_exp ->
        let addr_off = MemOffset.addr_size_to_offset (fst smt_ctx) addr_exp size in
        match MemType.get_mem_type smt_ctx curr_type.mem_type addr_off slot_anno with
        | None -> 
          Printf.printf "get_ld_op_type op %s fail to load from addr %s\n"
            (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand (LdOp ld_op)))
            (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
          None
        | Some (dep_type, taint_type) ->
          if TaintType.check_subtype smt_ctx true taint_type taint_anno then
            Some (dep_type, taint_type)
          else begin
            Printf.printf "get_ld_op_type op %s ld op taint annot mismatch for addr %s\n"
              (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand (LdOp ld_op)))
              (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
            None
          end
    end

  let get_src_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) (src: Isa.operand) : entry_t option =
    let ctx, _ = smt_ctx in
    (* TODO: Should we use data size here? *)
    match src with
    | ImmOp imm -> Some (BasicType.get_imm_type ctx imm) 
    | RegOp r -> Some (RegType.get_reg_type ctx curr_type.reg_type r)
    | RegMultOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a reg mult op"
    | MemOp mem_op -> Some (get_mem_op_type smt_ctx curr_type mem_op)
    | LdOp ld_op -> get_ld_op_type smt_ctx curr_type ld_op
    | StOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a st op"
    | LabelOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a label op"

  let get_dest_op_size (dest: Isa.operand) : int =
    match dest with
    | RegOp r -> Isa.get_reg_size r |> Int64.to_int
    | RegMultOp r_list -> Isa.get_reg_mult_op_size r_list |> Int64.to_int
    | StOp (_, _, _, _, size, _) -> size |> Int64.to_int
    | ImmOp _ | MemOp _ | LdOp _ | LabelOp _ -> arch_type_error "get_dest_op_size: dest is not reg or st op"

  let set_st_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (st_op: Isa.ldst_op)
      (new_type: entry_t) : MemType.t option =
    (* <TODO> Check this against infer and st rule, make sure it checks everything *)
    let disp, base, index, scale, size, (slot_anno, taint_anno) = st_op in
    let addr_dep_type, addr_taint_type =
      get_mem_op_type smt_ctx curr_type (disp, base, index, scale, None)
    in
    if not (TaintType.check_untaint smt_ctx addr_taint_type) then begin
      Printf.printf "set_st_op_type op %s taint addr %s\n"
        (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand (StOp st_op)))
        (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
      None
    end else begin
      match addr_dep_type with
      | Top _ ->
        Printf.printf "set_st_op_type op %s addr is top\n"
          (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
        None
      | Exp addr_exp ->
        let addr_off = MemOffset.addr_size_to_offset (fst smt_ctx) addr_exp size in
        let st_dep_type, st_taint_type = new_type in
        if not (TaintType.check_subtype smt_ctx false st_taint_type taint_anno) then None
        else
          (* Note: we check taint_anno against original taint in set_mem_type *)
          let new_mem_opt =
            MemType.set_mem_type smt_ctx 
              (StackSpillInfo.is_spill curr_type.stack_spill_info) 
              curr_type.mem_type addr_off slot_anno (st_dep_type, taint_anno)
          in
          begin match new_mem_opt with
          | None ->
            Printf.printf "set_st_op_type op %s fail to store to addr %s\n"
              (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand (StOp st_op)))
              (Sexplib.Sexp.to_string_hum (BasicType.sexp_of_t (addr_dep_type, addr_taint_type)));
            None
          | Some _ -> new_mem_opt
          end
    end

  let set_dest_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (dest: Isa.operand)
      (new_type: entry_t) (flag_update_list: (Isa.flag * entry_t) list) : t option =
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_update_list in
    match dest with
    | RegOp r ->
      Some { curr_type with
        reg_type = RegType.set_reg_type (fst smt_ctx) curr_type.reg_type r new_type;
        flag_type = new_flags }
    | RegMultOp r_list -> 
      Some { curr_type with
        reg_type = RegType.set_reg_mult_type (fst smt_ctx) curr_type.reg_type r_list new_type;
        flag_type = new_flags }
    | StOp st_op ->
      begin match set_st_op_type smt_ctx curr_type st_op new_type with
      | None -> None
      | Some new_mem_type ->
        Some { curr_type with 
          mem_type = new_mem_type;
          flag_type = new_flags }
      end
    | ImmOp _ | MemOp _ | LdOp _ | LabelOp _ -> arch_type_error "set_dest_op_type: dest is not reg or st op"

  let add_offset_rsp
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) (offset: int64) : t =
    match get_src_op_type smt_ctx curr_type (RegOp RSP) with
    | None -> arch_type_error "add_offset_rsp: cannot get rsp type"
    | Some (Top _, _) -> arch_type_error "add_offset_rsp: rsp is top"
    | Some (Exp rsp_exp, rsp_taint) ->
      let ctx, _ = smt_ctx in
      let off_exp = DepType.get_const_exp ctx offset 64 in
      let new_rsp_exp = Z3.BitVector.mk_add ctx rsp_exp off_exp in
      let new_reg_type = RegType.set_reg_type ctx curr_type.reg_type RSP (Exp new_rsp_exp, rsp_taint) in
      { curr_type with reg_type = new_reg_type }

  (* TODO:
     (1) type check at branch (or call) needs to check whether ctx_map contain all local (or local and input) variables
         (we should use the sup_a_type's global/input var as reference!!!)
         for func call, we need to check both input and output a_type's var!!!
     (2) type check at call also needs to check other fields in call_anno
     (3) we need to check validity of func interface against its function body, and also check its non-overlap info is constrained correctly *)

  type nary_op = | BOp of Isa.bop | UOp of Isa.uop | TOp of Isa.top
  [@@deriving sexp]

  let add_offset_rsp
      (smt_ctx: SmtEmitter.t)
      (curr_type: t) (offset: int64) : t =
    match get_src_op_type smt_ctx curr_type (RegOp RSP) with
    | None -> arch_type_error "add_offset_rsp: cannot get rsp type"
    | Some (Top _, _) -> arch_type_error "add_offset_rsp: rsp is top"
    | Some (Exp rsp_exp, rsp_taint) ->
      let ctx, _ = smt_ctx in
      let off_exp = DepType.get_const_exp ctx offset 64 in
      let new_rsp_exp = Z3.BitVector.mk_add ctx rsp_exp off_exp in
      let new_reg_type = RegType.set_reg_type ctx curr_type.reg_type RSP (Exp new_rsp_exp, rsp_taint) in
      { curr_type with reg_type = new_reg_type }

  let shift_rsp
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (offset: int64) =
    match get_src_op_type smt_ctx curr_type (RegOp RSP) with
    | None -> arch_type_error "shift_rsp: Cannot get type of %rsp"
    | Some (Top _, _) -> arch_type_error "shift_rsp: %rsp is top"
    | Some (Exp rsp_dep, rsp_tnt) ->
        let ctx, _ = smt_ctx in
        let off_exp = DepType.get_const_exp ctx offset 64 in
        let new_rsp_dep = DepType.Exp (Z3.BitVector.mk_add ctx rsp_dep off_exp) in
        (* let offset = if push then "-8" else "8" in
        let new_dep = Z3.BitVector.mk_sub ctx rsp_dep (Z3.BitVector.mk_numeral ctx offset 64) in
        let new_rsp = DepType.Exp new_dep, rsp_tnt in *)
        { curr_type with reg_type = RegType.set_reg_type ctx curr_type.reg_type RSP (new_rsp_dep, rsp_tnt) }
  
  let exe_nary
      ?(ignore_flags: bool = false) (* If true, flags will not be updated. This is to allow for n-ary operations to be applied as part of larger instructions, e.g. incrementing/decrementing %rsp in push/pop *)
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (n: int) (nop: nary_op)
      (dest: Isa.operand) (src_ops: Isa.operand list) : bool * t =
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx curr_type) src_ops in
    if List.length src_type_list <> n then false, curr_type else
    let ctx, _ = smt_ctx in
    let dest_type, flag_update_list =
      begin match n, nop with
      | 1, UOp uop -> BasicType.exe_uop ctx uop src_type_list get_src_flag_func (get_dest_op_size dest)
      | 2, BOp bop -> BasicType.exe_bop ctx bop src_type_list get_src_flag_func (get_dest_op_size dest)
      | 3, TOp top -> BasicType.exe_top ctx top src_type_list get_src_flag_func (get_dest_op_size dest)
      | _ -> arch_type_error "exe_nary: n and nop size do not match"
      end
    in
    let flag_update_list = if ignore_flags then [] else flag_update_list in
    begin match set_dest_op_type smt_ctx curr_type dest dest_type flag_update_list with
    | None -> false, curr_type
    | Some next_type -> true, next_type
    end

  let exe_xchg
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (dest0: Isa.operand) (dest1: Isa.operand)
      (src0: Isa.operand) (src1: Isa.operand) =
    let src_type_opt_0 = get_src_op_type smt_ctx curr_type src0 in
    let src_type_opt_1 = get_src_op_type smt_ctx curr_type src1 in
    match src_type_opt_0, src_type_opt_1 with
    | Some src_type_0, Some src_type_1 ->
      begin match set_dest_op_type smt_ctx curr_type dest0 src_type_0 [] with
      | None -> false, curr_type
      | Some result_type_1 ->
        begin
        match set_dest_op_type smt_ctx result_type_1 dest1 src_type_1 [] with
        | None -> false, curr_type
        | Some result_type_2 -> true, result_type_2
        end
      end
    | _ -> false, curr_type

  let exe_cmp
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (src0: Isa.operand)
      (src1: Isa.operand) =
    (* Note that src0 is not the true dest; in cmp, the result is discarded *)
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx curr_type) [ src0; src1 ] in
    if List.length src_type_list <> 2 then false, curr_type else
    let ctx, _ = smt_ctx in
    let _, flag_list = BasicType.exe_bop ctx Sub src_type_list get_src_flag_func (get_dest_op_size src0) in 
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_list in
    true, { curr_type with flag_type = new_flags }
  
  let exe_test
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (src0: Isa.operand)
      (src1: Isa.operand) =
    (* Note that src0 is not the true dest; in test, the result is discarded *)
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx curr_type) [ src0; src1 ] in
    if List.length src_type_list <> 2 then false, curr_type else
    let ctx, _ = smt_ctx in
    let _, flag_list = BasicType.exe_bop ctx And src_type_list get_src_flag_func (get_dest_op_size src0) in 
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_list in
    true, { curr_type with flag_type = new_flags }

  let exe_push
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (src: Isa.operand)
      (memslot: MemAnno.t) =
    let src_size = Isa.get_op_size src in
    let rsp_type = shift_rsp smt_ctx curr_type (Int64.neg src_size) in
    (* src should be reg, so always return Some *)
    let src_type = Option.get (get_src_op_type smt_ctx rsp_type src) in
    match set_dest_op_type smt_ctx rsp_type (StOp (None, Some RSP, None, None, src_size, memslot)) src_type [] with
    | None -> false, curr_type
    | Some result_type -> true, result_type

  let exe_pop
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (dest: Isa.operand)
      (memslot: MemAnno.t) =
    (* Store memory taint in dest *)
    let dest_size = Isa.get_op_size dest in
    let ld_op = Isa.LdOp (None, Some RSP, None, None, dest_size, memslot) in
    match get_src_op_type smt_ctx curr_type ld_op with
    | None -> false, curr_type
    | Some ld_type ->
      (* dest should be reg, so always return Some *)
      let result_type = Option.get (set_dest_op_type smt_ctx curr_type dest ld_type []) in
      true, shift_rsp smt_ctx result_type dest_size

  let exe_repmovs
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (size: int64)
      (mem1: MemAnno.t)
      (mem2: MemAnno.t) =
    (* <TODO> Fix this! *)
    let ld_op = Isa.LdOp (None, Some RSI, None, None, size, mem1) in
    let st_op = Isa.StOp (None, Some RDI, None, None, size, mem2) in
    let ld_type = Option.get (get_src_op_type smt_ctx curr_type ld_op) in
    match set_dest_op_type smt_ctx curr_type st_op ld_type [] with
    | None -> false, curr_type
    | Some result_type -> true, result_type
 
  let exe_replods
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (size: int64)
      (mem: MemAnno.t) =
    (* <TODO> Fix this! *)
    let ld_op = Isa.LdOp (None, Some RSI, None, None, size, mem) in
    let ld_type = Option.get (get_src_op_type smt_ctx curr_type ld_op) in
    match set_dest_op_type smt_ctx curr_type (RegOp RAX) ld_type [] with
    | None -> false, curr_type
    | Some result_type -> true, result_type
  
  let exe_repstos
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (size: int64)
      (mem: MemAnno.t) =
    (* <TODO> Fix this! *)
    let rax_type = Option.get (get_src_op_type smt_ctx curr_type (RegOp RAX)) in
    let st_op = Isa.StOp (None, Some RDI, None, None, size, mem) in
    match set_dest_op_type smt_ctx curr_type st_op rax_type [] with
    | None -> false, curr_type
    | Some result_type -> true, result_type
  
  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (inst: Isa.instruction) : bool * t =
    match inst with
    | BInst (bop, dest, src0, src1)           -> exe_nary    smt_ctx curr_type 2 (BOp bop) dest [ src0; src1 ]
    | UInst (uop, dest, src)                  -> exe_nary    smt_ctx curr_type 1 (UOp uop) dest [ src ]
    | TInst (top, dest, [ src0; src1; src2 ]) -> exe_nary    smt_ctx curr_type 3 (TOp top) dest [ src0; src1; src2 ]
    | Xchg (dest0, dest1, src0, src1)         -> exe_xchg    smt_ctx curr_type dest0 dest1 src0 src1
    | Cmp (src0, src1)                        -> exe_cmp     smt_ctx curr_type src0 src1
    | Test (src0, src1)                       -> exe_test    smt_ctx curr_type src0 src1
    | Push (src, memslot)                     -> exe_push    smt_ctx curr_type src memslot
    | Pop (dest, memslot)                     -> exe_pop     smt_ctx curr_type dest memslot
    | RepMovs (size, mem1, mem2)              -> exe_repmovs smt_ctx curr_type size mem1 mem2 
    | RepLods (size, mem)                     -> exe_replods smt_ctx curr_type size mem
    | RepStos (size, mem)                     -> exe_repstos smt_ctx curr_type size mem
    | Nop                                     -> true, curr_type
    | Hlt                                     -> true, curr_type
    | _ -> arch_type_error "<TODO> type_prop_non_branch not implemented yet"

  let type_prop_check_one_inst
      (smt_ctx: SmtEmitter.t)
      (func_interface_list: FuncInterface.t list)
      (block_type_list: t list)
      (curr_type: t)
      (inst: Isa.instruction) : bool * t =
    (* 1. Prop inst
       2. Ensure if can proceed to next inst, pc + 1 < dead_pc *)
    let _ = func_interface_list, block_type_list in (* TODO: remove this later *)
    match inst with
    | Jmp _ | Jcond _ ->
      arch_type_error "<TODO> not implemented yet"
      (* <TODO> For cond branch, 
         Taken: check dest is not dead block;
         Not taken: check if not taken is possible, next_pc < dead_pc, else next_pc = dead_pc! *)
    | Call _ ->
      arch_type_error "<TODO> not implemented yet"
    | _ ->
      (* Get state before executing next inst *)
      let checked, next_type = type_prop_non_branch smt_ctx curr_type inst in
      let next_type = { next_type with pc = next_type.pc + 1 } in
      (* For non-branch, it always proceed, so the next pc must be smaller than dead pc *)
      (* <TODO> Double check whether it is ok to check dead_pc here and use it for judgement in type_prop_check_one_block *)
      if checked && next_type.pc < next_type.dead_pc then
        true, next_type
      else false, next_type

  let type_prop_check_one_block
      (smt_ctx: SmtEmitter.t)
      (func_interface_list: FuncInterface.t list)
      (block_type_list: t list)
      (block_type: t)
      (block: Isa.instruction list) : bool =
    (* Check block type:
       1. Check block well-formness
          1. s-val belongs to s-alloc for each mem slot
       2. Prop if dead_pc > pc and check dead_pc *)
    (* <TODO> Check block well-formness *)
    false &&
    begin
      SmtEmitter.push smt_ctx;
      SmtEmitter.add_assertions smt_ctx (fst block_type.context);
      SmtEmitter.add_assertions smt_ctx (snd block_type.context);
      let check_prop =
        if block_type.pc >= block_type.dead_pc then true else
        List.fold_left (
          fun (acc: bool * t) (inst: Isa.instruction) ->
            let acc_check, curr_type = acc in
            if not acc_check then acc 
            else if curr_type.pc < curr_type.dead_pc then
              (* if not dead_pc, type_prop_check_one_inst prop one inst and also check dead_pc validity for next inst *)
              type_prop_check_one_inst smt_ctx func_interface_list block_type_list curr_type inst
            else acc
        ) (true, block_type) block |> fst
      in
      SmtEmitter.pop smt_ctx 1;
      check_prop
    end

  let type_prop_check_one_func
      (smt_ctx: SmtEmitter.t)
      (func_interface_list: FuncInterface.t list)
      (func_name: Isa.label)
      (func_type: t list)
      (func: Isa.basic_block list) : bool =
    (* Check items:
       1. Check first block well-formness
          1. overlap/non-overlap is constrained correctly 
             (only need to be checked here, and block subtype ensures it holds for all blocks in the same func)
             (otherwise we need to translate non-overlap info and do extra check on non-overlap at func call)
       2. Check func type matches its interface
       3. Check func type correctness (prop/symbolic execution) *)
    let _ = func_name in (* <TODO> Remove this later *)
    (* <TODO> Check non-overlap *)
    false &&
    (* <TODO> Check func type matches its interface *)
    false &&
    (* Check func type correctness (prop/symbolic execution) *)
    List.fold_left2 (
      fun (acc: bool) (block_type: t) (block: Isa.basic_block) ->
        if not acc then acc else
        type_prop_check_one_block smt_ctx func_interface_list func_type block_type block.insts
    ) true func_type func

end

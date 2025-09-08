(* open Type.Isa *)
(* open Type.Set_sexp *)
open Type.Smt_emitter
(* open Z3_sexp *)
open Basic_type
open Dep_change_ctx
open Isa_flag_config
(* open Mem_anno *)
open Reg_type
open Flag_type
open Mem_type
open Mem_anno
open Branch_anno
open Arch_type_basic
open Func_interface
(* open Z3 *)

module ArchType = struct
include ArchTypeBasic

  let get_mem_op_type
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t) (mem_op: Isa.mem_op) : entry_t =
    let ctx, _ = smt_ctx in
    (* I ignore mem_op size since it should always be 8! *)
    let disp, base, index, scale, _ = mem_op in
    let disp =
      match disp with
      | Some disp_imm -> DepType.get_imm_exp_size_expected ctx disp_imm (Some (Isa.get_gpr_full_size ()))
      | None -> DepType.get_const_exp ctx 0L 64
    in
    let base =
      match base with
      | Some base_r -> RegType.get_reg_type_size_expected ctx curr_type.reg_type base_r (Some (Isa.get_gpr_full_size ()))
      | None -> BasicType.get_const_type ctx 0L 64
    in
    let index =
      match index with
      | Some index_r -> RegType.get_reg_type_size_expected ctx curr_type.reg_type index_r (Some (Isa.get_gpr_full_size ()))
      | None -> BasicType.get_const_type ctx 0L 64
    in
    let scale =
      match scale with
      | Some s -> DepType.get_const_exp ctx (Isa.scale_val s) 64
      | None -> DepType.get_const_exp ctx 1L 64
    in
    BasicType.get_mem_op_type ctx is_non_change_exp disp base index scale

  let get_ld_op_type
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t) (ld_op: Isa.ldst_op) : entry_t option =
    (* <TODO> Check this against infer and ld rule, make sure it checks everything *)
    let disp, base, index, scale, size, (slot_anno, taint_anno) = ld_op in
    let slot_anno, taint_anno = (Option.get slot_anno), (Option.get taint_anno) in
    let addr_dep_type, addr_taint_type =
      get_mem_op_type smt_ctx is_non_change_exp curr_type (disp, base, index, scale, None)
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
        match MemType.get_mem_type smt_ctx is_non_change_exp curr_type.mem_type addr_off slot_anno with
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
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t) 
      ?(check_reg_valid=true)
      (src: Isa.operand) : entry_t option =
    let ctx, _ = smt_ctx in
    (* TODO: Should we use data size here? *)
    match src with
    | ImmOp imm -> Some (BasicType.get_imm_type ctx imm) 
    | RegOp r -> Some (RegType.get_reg_type ctx curr_type.reg_type ~check_valid:check_reg_valid r)
    | RegMultOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a reg mult op"
    | MemOp mem_op -> Some (get_mem_op_type smt_ctx is_non_change_exp curr_type mem_op)
    | LdOp ld_op -> get_ld_op_type smt_ctx is_non_change_exp curr_type ld_op
    | StOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a st op"
    | LabelOp _ -> arch_type_error "get_src_op_type: cannot get src op type of a label op"

  let get_dest_op_size (dest: Isa.operand) : int = (* return: # bits *)
    let size_bytes = match dest with
    | RegOp r -> Isa.get_reg_size r |> Int64.to_int
    | RegMultOp r_list -> Isa.get_reg_mult_op_size r_list |> Int64.to_int
    | StOp (_, _, _, _, size, _) -> size |> Int64.to_int
    | ImmOp _ | MemOp _ | LdOp _ | LabelOp _ -> arch_type_error "get_dest_op_size: dest is not reg or st op"
    in
    size_bytes * 8

  let set_st_op_type
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (st_op: Isa.ldst_op)
      (new_type: entry_t) : MemType.t option =
    (* <TODO> Check this against infer and st rule, make sure it checks everything *)
    let disp, base, index, scale, size, (slot_anno, taint_anno) = st_op in
    let slot_anno, taint_anno = (Option.get slot_anno), (Option.get taint_anno) in
    let addr_dep_type, addr_taint_type =
      get_mem_op_type smt_ctx is_non_change_exp curr_type (disp, base, index, scale, None)
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
        if not (TaintType.check_subtype smt_ctx false st_taint_type taint_anno) then begin
          Printf.printf "set_st_op_type: taint subtype check failed\nst_taint_type=%s\ntaint_anno=%s\n"
            (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t st_taint_type))
            (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t taint_anno));
          None
        end else
          (* Note: we check taint_anno against original taint in set_mem_type *)
          let new_mem_opt =
            MemType.set_mem_type smt_ctx 
              is_non_change_exp
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
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (dest: Isa.operand) (new_type: entry_t) 
      (flag_update_list: (Isa.flag * entry_t) list)
      (flag_update_config: (Isa.flag * bool) list) : t option =
    (* Printf.printf "set_dest_op_type: dest=%s, type=%s\n"
      (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand dest))
      (Sexplib.Sexp.to_string_hum (sexp_of_entry_t new_type)); *)
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_update_list flag_update_config in
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
      begin match set_st_op_type smt_ctx is_non_change_exp curr_type st_op new_type with
      | None -> Printf.printf "set_st_op_type failed\n"; None
      | Some new_mem_type ->
        Some { curr_type with 
          mem_type = new_mem_type;
          flag_type = new_flags }
      end
    | ImmOp _ | MemOp _ | LdOp _ | LabelOp _ -> arch_type_error "set_dest_op_type: dest is not reg or st op"

  (* TODO:
     (1) type check at branch (or call) needs to check whether ctx_map contain all local (or local and input) variables
         (we should use the sup_a_type's global/input var as reference!!!)
         for func call, we need to check both input and output a_type's var!!!
     (2) type check at call also needs to check other fields in call_anno
     (3) we need to check validity of func interface against its function body, and also check its non-overlap info is constrained correctly *)

  type nary_op = | BOp of Isa.bop | UOp of Isa.uop | TOp of Isa.top
  [@@deriving sexp]

  let shift_rsp
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (offset: int64) =
    match get_src_op_type smt_ctx is_non_change_exp curr_type (RegOp RSP) with
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
  
  let get_op_flag_update_config (op: nary_op) : bool IsaFlagConfig.flag_map_t =
    match op with
    | BOp bop -> IsaFlagConfig.get_bop_config bop |> snd
    | UOp uop -> IsaFlagConfig.get_uop_config uop |> snd
    | TOp top -> IsaFlagConfig.get_top_config top |> snd

  let exe_nary
      ?(ignore_flags: bool = false) (* If true, flags will not be updated. This is to allow for n-ary operations to be applied as part of larger instructions, e.g. incrementing/decrementing %rsp in push/pop *)
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (n: int) (nop: nary_op)
      (dest: Isa.operand) (src_ops: Isa.operand list) : bool * t =
    let xor_reset =
      match nop, src_ops with
      | BOp Xor, [ src1; src2 ]
      | BOp Xorp, [ src1; src2 ]
      | BOp Pxor, [ src1; src2 ] -> Isa.cmp_operand src1 src2
      | _ -> false
    in
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx is_non_change_exp curr_type ~check_reg_valid:(not xor_reset)) src_ops in
    (* List.iter (fun src -> Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand src))) src_ops;
    Printf.printf "exe_nary: dest = %s\nsrcs:\n" (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand dest));
    List.iter2 (fun src src_type ->
      Printf.printf "\t%s -> %s\n"
        (Sexplib.Sexp.to_string_hum (Isa.sexp_of_operand src))
        (Sexplib.Sexp.to_string_hum (sexp_of_entry_t src_type));
    ) src_ops src_type_list; *)
    if List.length src_type_list <> n then false, curr_type else
    let dest_type, flag_update_list =
      begin match n, nop with
      | 1, UOp uop -> BasicType.exe_uop smt_ctx is_non_change_exp uop src_type_list get_src_flag_func (get_dest_op_size dest) false
      | 2, BOp bop ->
        (* let xor_reset = match bop, src_ops with
        | Isa.Xor, [ src1; src2 ]
        | Isa.Xorp, [ src1; src2 ]
        | Isa.Pxor, [ src1; src2 ] -> Isa.cmp_operand src1 src2
        | _ -> false
        in *)
        BasicType.exe_bop smt_ctx is_non_change_exp bop src_type_list get_src_flag_func (get_dest_op_size dest) xor_reset
      | 3, TOp top -> BasicType.exe_top smt_ctx is_non_change_exp top src_type_list get_src_flag_func (get_dest_op_size dest) false
      | _ -> arch_type_error "exe_nary: n and nop size do not match"
      end
    in
    let flag_update_list = if ignore_flags then [] else flag_update_list in
    let flag_update_config = if ignore_flags then [] else get_op_flag_update_config nop in
    begin match set_dest_op_type smt_ctx is_non_change_exp curr_type dest dest_type flag_update_list flag_update_config with
    | None -> false, curr_type
    | Some next_type -> true, next_type
    end

  let exe_xchg
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (dest0: Isa.operand) (dest1: Isa.operand)
      (src0: Isa.operand) (src1: Isa.operand) =
    let src_type_opt_0 = get_src_op_type smt_ctx is_non_change_exp curr_type src0 in
    let src_type_opt_1 = get_src_op_type smt_ctx is_non_change_exp curr_type src1 in
    match src_type_opt_0, src_type_opt_1 with
    | Some src_type_0, Some src_type_1 ->
      begin match set_dest_op_type smt_ctx is_non_change_exp curr_type dest0 src_type_0 [] [] with
      | None -> false, curr_type
      | Some result_type_1 ->
        begin
        match set_dest_op_type smt_ctx is_non_change_exp result_type_1 dest1 src_type_1 [] [] with
        | None -> false, curr_type
        | Some result_type_2 -> true, result_type_2
        end
      end
    | _ -> false, curr_type

  let exe_cmp
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (src0: Isa.operand)
      (src1: Isa.operand) =
    (* Note that src0 is not the true dest; in cmp, the result is discarded *)
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx is_non_change_exp curr_type) [ src0; src1 ] in
    if List.length src_type_list <> 2 then false, curr_type else
    let _, flag_list = BasicType.exe_bop smt_ctx is_non_change_exp Sub src_type_list get_src_flag_func (get_dest_op_size src0) false in 
    let flag_config = List.map (fun (x, _) -> x, false) flag_list in
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_list flag_config in
    true, { curr_type with flag_type = new_flags }
  
  let exe_test
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (src0: Isa.operand)
      (src1: Isa.operand) =
    (* Note that src0 is not the true dest; in test, the result is discarded *)
    let get_src_flag_func = FlagType.get_flag_type curr_type.flag_type in
    let src_type_list = List.filter_map (get_src_op_type smt_ctx is_non_change_exp curr_type) [ src0; src1 ] in
    if List.length src_type_list <> 2 then false, curr_type else
    let _, flag_list = BasicType.exe_bop smt_ctx is_non_change_exp And src_type_list get_src_flag_func (get_dest_op_size src0) false in 
    let flag_config = List.map (fun (x, _) -> x, false) flag_list in
    let new_flags = FlagType.set_flag_list_type curr_type.flag_type flag_list flag_config in
    true, { curr_type with flag_type = new_flags }

  let exe_push
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (src: Isa.operand)
      (memslot: MemAnno.t) =
    let src_size = Isa.get_op_size src in
    let rsp_type = shift_rsp smt_ctx is_non_change_exp curr_type (Int64.neg src_size) in
    (* src should be reg, so always return Some *)

    (* Special case: rax!!!*)
    let is_push_rax =
      match src with
      | RegOp r -> Isa.get_reg_idx r = Isa.get_reg_idx RAX
      | _ -> arch_type_error "unexpected push op"
    in
    if is_push_rax then Printf.printf "Warning: push rax happens!!!\n";

    let src_type = Option.get (get_src_op_type smt_ctx is_non_change_exp rsp_type ~check_reg_valid:(not is_push_rax) src) in
    match set_dest_op_type smt_ctx is_non_change_exp rsp_type (StOp (None, Some RSP, None, None, src_size, memslot)) src_type [] [] with
    | None -> false, curr_type
    | Some result_type -> true, result_type

  let exe_pop
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (dest: Isa.operand)
      (memslot: MemAnno.t) =
    (* Store memory taint in dest *)
    let dest_size = Isa.get_op_size dest in
    let ld_op = Isa.LdOp (None, Some RSP, None, None, dest_size, memslot) in
    match get_src_op_type smt_ctx is_non_change_exp curr_type ld_op with
    | None -> false, curr_type
    | Some ld_type ->
      (* dest should be reg, so always return Some *)
      let result_type = Option.get (set_dest_op_type smt_ctx is_non_change_exp curr_type dest ld_type [] []) in
      true, shift_rsp smt_ctx is_non_change_exp result_type dest_size

  let get_rep_acc (size: int64) : Isa.register =
    match size with
    | 8L -> AL
    | 16L -> AX
    | 32L -> EAX
    | 64L -> RAX
    | _ -> arch_type_error "Invalid size for rep instruction, must be 8L, 16L, 32L, or 64L"

  let get_rep_count_reg (size: int64) : Isa.register =
    match size with
    | 16L      -> CX
    | 64L      -> RCX
    | 8L | 32L -> ECX
    | _ -> arch_type_error "Invalid size for rep instruction, must be 8L, 16L, 32L, or 64L"

  let exe_repmovs
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (size: int64)
      (mem_dst: MemAnno.t)
      (mem_src: MemAnno.t) =
    let ctx, _ = smt_ctx in
    let src_slot, _ = mem_src in
    let src_slot = Option.get src_slot in
    let dst_slot, _ = mem_dst in
    let dst_slot = Option.get dst_slot in
    let count_reg = get_rep_count_reg size in
    let rcx_dep, rcx_taint = Option.get (get_src_op_type smt_ctx is_non_change_exp curr_type (RegOp count_reg)) in
    match rcx_dep with
    | Top _ -> false, curr_type 
    | Exp rcx_exp -> begin
      let rcx_size = Z3.BitVector.get_size (Z3.Expr.get_sort rcx_exp) in
      let entry_size = string_of_int (Int64.to_int size) in
      let addr_lower = Z3.BitVector.mk_numeral ctx "0" rcx_size in
      let addr_upper = Z3.BitVector.mk_mul ctx rcx_exp (Z3.BitVector.mk_numeral ctx entry_size rcx_size) in
      let addr_off = (addr_lower, addr_upper) in
      match MemType.get_mem_type smt_ctx is_non_change_exp curr_type.mem_type addr_off src_slot with
      | None -> false, curr_type
      | Some src_type -> begin
        match MemType.set_mem_type smt_ctx is_non_change_exp curr_type.mem_type addr_off dst_slot src_type with
        | None -> false, curr_type
        | Some result_mem_type ->
          let result_type = { curr_type with mem_type = result_mem_type } in
          let rcx_zero = RegType.set_reg_type ctx result_type.reg_type count_reg (Exp (Z3.BitVector.mk_numeral ctx "0" rcx_size), rcx_taint) in
          true, { result_type with reg_type = rcx_zero }
        end
      end

  let exe_replods
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (size: int64)
      (mem: MemAnno.t) =
    (* <TODO> Fix this! *)
    let ctx, _ = smt_ctx in
    let ld_slot, _ = mem in
    let ld_slot = Option.get ld_slot in
    let count_reg = get_rep_count_reg size in
    let rcx_dep, rcx_taint = Option.get (get_src_op_type smt_ctx is_non_change_exp curr_type (RegOp count_reg)) in
    match rcx_dep with
    | Top _ -> false, curr_type 
    | Exp rcx_exp -> begin
      let rcx_size = Z3.BitVector.get_size (Z3.Expr.get_sort rcx_exp) in
      let entry_size = string_of_int (Int64.to_int size) in
      let addr_lower = Z3.BitVector.mk_numeral ctx "0" rcx_size in
      let addr_upper = Z3.BitVector.mk_mul ctx rcx_exp (Z3.BitVector.mk_numeral ctx entry_size rcx_size) in
      let addr_off = (addr_lower, addr_upper) in
      match MemType.get_mem_type smt_ctx is_non_change_exp curr_type.mem_type addr_off ld_slot with
      | None -> false, curr_type
      | Some ld_type -> begin
        match set_dest_op_type smt_ctx is_non_change_exp curr_type (RegOp RAX) ld_type [] [] with
        | None -> false, curr_type
        | Some result_type ->
          let rcx_zero = RegType.set_reg_type ctx result_type.reg_type count_reg (Exp (Z3.BitVector.mk_numeral ctx "0" rcx_size), rcx_taint) in
          true, { result_type with reg_type = rcx_zero }
        end
      end
  
  let exe_repstos
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (size: int64)
      (mem: MemAnno.t) =
    (* <TODO> Fix this! *)
    let ctx, _ = smt_ctx in
    let st_slot, _ = mem in
    let st_slot = Option.get st_slot in
    let count_reg = get_rep_count_reg size in
    let rcx_dep, rcx_taint = Option.get (get_src_op_type smt_ctx is_non_change_exp curr_type (RegOp count_reg)) in
    match rcx_dep with
    | Top _ -> false, curr_type 
    | Exp rcx_exp -> begin
      let acc_reg = get_rep_acc size in
      let acc_type = Option.get (get_src_op_type smt_ctx is_non_change_exp curr_type (RegOp acc_reg)) in
      let rcx_size = Z3.BitVector.get_size (Z3.Expr.get_sort rcx_exp) in
      let entry_size = string_of_int (Int64.to_int size) in
      let addr_lower = Z3.BitVector.mk_numeral ctx "0" rcx_size in
      let addr_upper = Z3.BitVector.mk_mul ctx rcx_exp (Z3.BitVector.mk_numeral ctx entry_size rcx_size) in
      let addr_off = (addr_lower, addr_upper) in
      match MemType.set_mem_type smt_ctx is_non_change_exp curr_type.mem_type addr_off st_slot acc_type with
      | None -> false, curr_type
      | Some result_mem_type ->
          let result_type = { curr_type with mem_type = result_mem_type } in
          let rcx_zero = RegType.set_reg_type ctx result_type.reg_type count_reg (Exp (Z3.BitVector.mk_numeral ctx "0" rcx_size), rcx_taint) in
          true, { result_type with reg_type = rcx_zero }
      end
     
  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (inst: Isa.instruction) : bool * t =
    match inst with
    (* For inst with multiple src, we want to check if one src is top (thereby dest is top), then other src are non change exp.
      An easy impl is to check if dest is top, then src are non change exp.
      We do not need to do this for single src op (e.g., RepMovs, where dest_type is just src_type) *)
    | BInst (bop, dest, src0, src1)           -> exe_nary    smt_ctx is_non_change_exp curr_type 2 (BOp bop) dest [ src0; src1 ]
    | UInst (uop, dest, src)                  -> exe_nary    smt_ctx is_non_change_exp curr_type 1 (UOp uop) dest [ src ]
    | TInst (top, dest, [ src0; src1; src2 ]) -> exe_nary    smt_ctx is_non_change_exp curr_type 3 (TOp top) dest [ src0; src1; src2 ]
    | Xchg (dest0, dest1, src0, src1)         -> exe_xchg    smt_ctx is_non_change_exp curr_type dest0 dest1 src0 src1
    | Cmp (src0, src1)                        -> exe_cmp     smt_ctx is_non_change_exp curr_type src0 src1
    | Test (src0, src1)                       -> exe_test    smt_ctx is_non_change_exp curr_type src0 src1
    | Push (src, memslot)                     -> exe_push    smt_ctx is_non_change_exp curr_type src memslot
    | Pop (dest, memslot)                     -> exe_pop     smt_ctx is_non_change_exp curr_type dest memslot
    | RepMovs (size, mem_dst, mem_src)        -> exe_repmovs smt_ctx is_non_change_exp curr_type size mem_dst mem_src 
    | RepLods (size, mem)                     -> exe_replods smt_ctx is_non_change_exp curr_type size mem
    | RepStos (size, mem)                     -> exe_repstos smt_ctx is_non_change_exp curr_type size mem
    | Nop | Hlt | Syscall                     -> true, curr_type
    | _ -> arch_type_error "<TODO> type_prop_non_branch not implemented yet"
  
  let type_prop_uncond_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (targ_type: t)
      (br_anno: BranchAnno.t)
      : bool * t =
    if curr_type.pc + 1 < curr_type.dead_pc then begin
      Printf.printf "Warning: instr after uncond branch is not dead (%d, %d)\n" (curr_type.pc + 1) curr_type.dead_pc;
      false, curr_type
    end else

    let result =
      check_subtype smt_ctx false curr_type targ_type br_anno None && BranchAnno.is_taint_map_empty br_anno,
      curr_type (* already at the end of block, no further update *)
    in

    if (fst result) = false then
      Printf.printf "Warning: uncond branch target subtype check failed\n";
    result
  
  let type_prop_cond_branch
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (curr_type: t)
      (targ_type: t)
      (br_cond: Isa.branch_cond)
      (br_anno: BranchAnno.t)
      : bool * t =
    let z3_ctx, _ = smt_ctx in
    let get_flag (f: Isa.flag) : DepType.exp_t = FlagType.get_flag_type curr_type.flag_type f |> fst |> DepType.get_exp in
    let is_zero (x: Z3.Expr.expr) = Z3.Boolean.mk_eq z3_ctx x (Z3.Boolean.mk_false z3_ctx) in
    let is_one (x: Z3.Expr.expr) = Z3.Boolean.mk_eq z3_ctx x (Z3.Boolean.mk_true z3_ctx) in
    let equal (x: Z3.Expr.expr) (y: Z3.Expr.expr) = Z3.Boolean.mk_eq z3_ctx x y in 
    let not_equal (x: Z3.Expr.expr) (y: Z3.Expr.expr) = Z3.Boolean.mk_not z3_ctx (Z3.Boolean.mk_eq z3_ctx x y) in
    let logic_and (x_list: Z3.Expr.expr list) = Z3.Boolean.mk_and z3_ctx x_list in
    let logic_or (x_list: Z3.Expr.expr list) = Z3.Boolean.mk_or z3_ctx x_list in

    let taken = match br_cond with
    | JNe -> get_flag ZF |> is_zero
    | JE  -> get_flag ZF |> is_one
    | JL -> not_equal (get_flag SF) (get_flag OF)
    | JLe -> logic_or [ is_one (get_flag ZF); not_equal (get_flag SF) (get_flag OF) ]
    | JG -> logic_and [ is_zero (get_flag ZF); equal (get_flag SF) (get_flag OF) ]
    | JGe -> equal (get_flag SF) (get_flag OF)
    | JB -> is_one (get_flag CF)
    | JBe -> logic_or [ is_one (get_flag ZF); is_one (get_flag CF) ]
    | JA -> logic_and [ is_zero (get_flag ZF); is_zero (get_flag CF) ]
    | JAe -> is_zero (get_flag CF)
    | JOther -> Z3.Boolean.mk_const_s z3_ctx "s-top-bool"
    in

    let check_taken () : bool =
      if targ_type.pc >= targ_type.dead_pc then begin
        Printf.printf "Warning: branch target is dead";
        false
      end else begin
        SmtEmitter.push smt_ctx;
        SmtEmitter.add_assertions smt_ctx [taken];
        DepChangeCtx.add_copy_assertion smt_ctx curr_type.change_info.change_copy_map taken;
        let next_type = { curr_type with
          context = BasicType.append_ctx curr_type.context taken
        } in
        let valid = check_subtype smt_ctx false next_type targ_type br_anno None in
        SmtEmitter.pop smt_ctx 1;
        valid && BranchAnno.is_taint_map_empty br_anno
        (* Here I only check for taken side, and will not check if always not taken,
          which is slightly different from the rule Typing-Jne in the paper. But both versions should be fine. *)
      end
    in

    let check_not_taken () : bool * t =
      let not_taken = Z3.Boolean.mk_not z3_ctx taken in
      SmtEmitter.add_assertions smt_ctx [not_taken];
      DepChangeCtx.add_copy_assertion smt_ctx curr_type.change_info.change_copy_map not_taken;
      let next_type = { curr_type with
        pc = curr_type.pc + 1;
        context = BasicType.append_ctx curr_type.context not_taken
      } in
      if next_type.pc >= next_type.dead_pc then begin
        Printf.printf "Warning: instr after not taken branch is dead";
        false, next_type
      end else
        true, next_type
    in

    (* let check_non_change_cond, cond_instance = DepChangeCtx.check_non_change_exp smt_ctx curr_type.change_sub_map taken in *)
    let taken_cond_check_result = SmtEmitter.check_compliance smt_ctx [taken] in
    let check_non_change_cond = is_non_change_exp taken in
    (* Printf.printf "Check br_cond (sat = %s) is non change\n%s\n" 
      (Sexplib.Sexp.to_string_hum (SmtEmitter.sexp_of_sat_result_t (taken_cond_check_result))) 
      (Z3.Expr.to_string taken); *)
    if not check_non_change_cond then begin
      Printf.printf "Warning: non change cond branch check failed\n%s\n" (Z3.Expr.to_string taken);
      Printf.printf "Unsat model\n%s\n" (Z3.Model.to_string (Z3.Solver.get_model (snd smt_ctx) |> Option.get));
      Printf.printf "Change ctx\n%s\n" (Sexplib.Sexp.to_string_hum (DepChangeCtx.sexp_of_t curr_type.change_info));
      SmtEmitter.pp_smt_ctx 9 smt_ctx;
      failwith "Fail non change cond check\n"
    end else

    let check_pass, next_state = match taken_cond_check_result with
    | SmtEmitter.SatNo -> (* always not taken *)
      Printf.printf "Always not taken\n";
      check_not_taken ()
    | SmtEmitter.SatYes -> (* always taken *)
      Printf.printf "Always taken\n";
      if curr_type.pc + 1 < curr_type.dead_pc then begin
        Printf.printf "Warning: instr after always-taken cond branch is not dead\n";
        false, curr_type
      end else
        check_taken (), { curr_type with pc = curr_type.pc + 1 } (* update pc so block check can terminate *)
    | SmtEmitter.SatUnknown ->
      (* check_not_taken must be called later, as it adds assertions and changes the global ctx *)
      let taken_valid = check_taken() in
      let not_taken_valid, next_type = check_not_taken () in
      taken_valid && not_taken_valid, next_type
    in

    if not check_pass then
      Printf.printf "Warning: cond branch target subtype check failed\n";

    check_pass && check_non_change_cond, next_state

  let type_prop_check_one_inst
      (smt_ctx: SmtEmitter.t)
      (func_interface_list: FuncInterface.t list)
      (block_type_list: t list)
      (curr_type: t)
      (inst: Isa.instruction) : bool * t =
    Printf.printf "checking inst[%d] %s\n%!" curr_type.pc (Isa.sexp_of_instruction inst |> Sexplib.Sexp.to_string_hum);
    (* 1. Prop inst
       2. Ensure if can proceed to next inst, pc + 1 < dead_pc *)
    let find_block_helper (label: Isa.label) : t =
      List.find (fun block_type -> block_type.label = label) block_type_list
    in
    let is_non_change_exp = DepChangeCtx.check_non_change_exp smt_ctx curr_type.change_info.change_copy_map in
    match inst with
    | Directive _ -> true, { curr_type with pc = curr_type.pc + 1 }
    | Jmp (br_target, br_anno) ->
      let targ_type = find_block_helper br_target in
      type_prop_uncond_branch smt_ctx curr_type targ_type br_anno
    | Jcond (br_cond, br_target, br_anno) ->
      let targ_type = find_block_helper br_target in
      let result = type_prop_cond_branch smt_ctx is_non_change_exp curr_type targ_type br_cond br_anno in
      (* Printf.printf "cond branch met, next type:\n%s\n" (sexp_of_t (snd result) |> Sexplib.Sexp.to_string_hum); *)
      result
    | Call (callee_name, call_anno) ->
      let callee_interface = FuncInterface.get_func_interface func_interface_list callee_name in
      let curr_type = shift_rsp smt_ctx is_non_change_exp curr_type (-8L) in
      (* Printf.printf "type before call:\n%s\n" (sexp_of_t curr_type |> Sexplib.Sexp.to_string_hum); *)
      let result = FuncInterface.prop_check_call smt_ctx is_non_change_exp curr_type callee_interface call_anno in
      begin match result with
      | Some next_type ->
        let next_type = shift_rsp smt_ctx is_non_change_exp next_type 8L in
        (* Printf.printf "type after call:\n%s\n" (sexp_of_t next_type |> Sexplib.Sexp.to_string_hum); *)
        true, next_type
      | None -> false, curr_type
      end
    | _ ->
      (* Get state before executing next inst *)
      let checked, next_type = type_prop_non_branch smt_ctx is_non_change_exp curr_type inst in
      let next_type = { next_type with pc = next_type.pc + 1 } in
      (* For non-branch, it always proceed, so the next pc must be smaller than dead pc *)
      (* <TODO> Double check whether it is ok to check dead_pc here and use it for judgement in type_prop_check_one_block *)
      if checked && next_type.pc < next_type.dead_pc then
        true, next_type
      else false, next_type

  let add_block_context_to_solver
      (smt_ctx: SmtEmitter.t)
      (block_type: t) : unit =
    SmtEmitter.add_assertions smt_ctx (fst block_type.context);
    SmtEmitter.add_assertions smt_ctx (snd block_type.context);
    ()

  let type_prop_check_one_block
      (smt_ctx: SmtEmitter.t)
      (func_interface_list: FuncInterface.t list)
      (block_type_list: t list)
      (block_type: t)
      (block: Isa.instruction list) : bool =
    Printf.printf "checking block %s\n%!" block_type.label;
    (* Check block type:
       1. Check block well-formness
          1. s-val belongs to s-alloc for each mem slot
       2. Prop if dead_pc > pc and check dead_pc *)
    SmtEmitter.push smt_ctx;
    add_block_context_to_solver smt_ctx block_type;

    Printf.printf "checking valid region...\n";
    let check_valid_region = MemType.check_valid_region smt_ctx block_type.mem_type in
    Printf.printf "valid region: %b\n" check_valid_region;

    (* Printf.printf "copy block assertions\n"; *)
    DepChangeCtx.copy_all_assertions smt_ctx block_type.change_info.change_copy_map;
    DepChangeCtx.add_extra_non_change_assertions smt_ctx block_type.change_info;

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

    let result = check_valid_region && check_prop in
    Printf.printf "block %s check result: %b\n" block_type.label result;
    SmtEmitter.pop smt_ctx 1;
    result

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
    let fi = FuncInterface.get_func_interface func_interface_list func_name in

    (* temporarily add context of entry BB to do non-overlap check *)
    SmtEmitter.push smt_ctx;
    let entry_bb_type = List.hd func_type in
    add_block_context_to_solver smt_ctx entry_bb_type;
    let result_non_overlap = MemType.check_non_overlap smt_ctx entry_bb_type.mem_type in
    SmtEmitter.pop smt_ctx 1;

    (* add constraint so that each slot's initialized range is the subset of the slot *)
    FuncInterface.add_in_mem_range_in_off_constr smt_ctx fi;

    result_non_overlap &&
    (* <TODO> Check func type matches its interface *)
    true &&
    (* Check func type correctness (prop/symbolic execution) *)
    List.fold_left2 (
      fun (acc: bool) (block_type: t) (block: Isa.basic_block) ->
        if not acc then acc else
        type_prop_check_one_block smt_ctx func_interface_list func_type block_type block.insts
    ) true func_type func

end

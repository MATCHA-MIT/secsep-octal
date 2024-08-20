open Pretty_print
open Entry_type
open Reg_type_new
open Mem_type_new
open Smt_emitter
open Isa
open Single_exp
open Constraint

module CondType (Entry: EntryType) = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type entry_t = Entry.t

  type cond = 
    | Eq | Ne
    | Le | Lt (* Signed comparison *)
    | Be | Bt (* Unsigned comparison *)

  type t = cond * entry_t * entry_t

  let not_cond_type (cond: t) : t =
    match cond with
    | (Eq, l, r) -> (Ne, l, r)
    | (Ne, l, r) -> (Eq, l, r)
    | (Le, l, r) -> (Lt, r, l)
    | (Lt, l, r) -> (Le, r, l)
    | (Be, l, r) -> (Bt, r, l)
    | (Bt, l, r) -> (Be, l, r)

  let to_string (cond: t) =
    let c, l, r = cond in
    let s = match c with
    | Eq -> "Eq" | Ne -> "Ne"
    | Le -> "Le" | Lt -> "Lt"
    | Be -> "Be" | Bt -> "Bt"
    in
    Printf.sprintf "%s(%s,%s)" s (Entry.to_string l) (Entry.to_string r)
    (* s ^ "(" ^ (Entry.to_string l) ^ "," ^ (Entry.to_string r) ^ ")" *)

  let get_taken_type (cond: Isa.branch_cond) (flag: entry_t * entry_t) : t option =
    let l, r = flag in
    match cond with
    | JNe -> Some (Ne, l, r)
    | JE -> Some (Eq, l, r)
    | JL -> Some (Lt, l, r)
    | JLe -> Some (Le, l, r)
    | JG -> Some (Lt, r, l)
    | JGe -> Some (Le, r, l)
    | JB -> Some (Bt, l, r)
    | JBe -> Some (Be, l, r)
    | JA -> Some (Bt, r, l)
    | JAe -> Some (Be, r, l)
    | JOther -> None

  let to_smt_expr (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let cond, l, r = cond in
    let exp_l = Entry.to_smt_expr smt_ctx l in
    let exp_r = Entry.to_smt_expr smt_ctx r in
    match cond with
    | Ne -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx exp_l exp_r)
    | Eq -> Z3.Boolean.mk_eq ctx exp_l exp_r
    | Le -> Z3.BitVector.mk_sle ctx exp_l exp_r
    | Lt -> Z3.BitVector.mk_slt ctx exp_l exp_r
    | Be -> Z3.BitVector.mk_ule ctx exp_l exp_r
    | Bt -> Z3.BitVector.mk_ult ctx exp_l exp_r

  let pp_cond (lvl: int) (cond: t) =
    PP.print_lvl lvl "%s\n" (to_string cond)

  let pp_cond_list (lvl: int) (cond_list: t list) =
    PP.print_lvl lvl "<Cond list>\n";
    List.iter (fun x -> pp_cond (lvl + 1) x) (List.rev cond_list)

end

module ArchType (Entry: EntryType) = struct
  exception ArchTypeError of string
  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  type entry_t = Entry.t

  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  module CondType = CondType (Entry)

  type t = {
    label: Isa.label;
    pc: int;
    reg_type: RegType.t;
    mem_type: MemType.t;
    flag: entry_t * entry_t;
    branch_hist: CondType.t list;
    (* smt_ctx: SmtEmitter.t; *)
    local_var_map: Entry.local_var_map_t;
    useful_var: SingleExp.SingleVarSet.t
    (* Maybe add constraint set here!!! *)
  }

  type block_subtype_t = t * (t list)

  let init_from_layout 
      (label: Isa.label)
      (start_var: entry_t) (start_pc: int)
      (mem_layout: 'a MemType.mem_content) : entry_t * t =
    let idx0, reg_type = RegType.init_reg_type start_var in
    let idx1, mem_type = MemType.init_mem_type_from_layout idx0 mem_layout in
    idx1, {
      label = label;
      pc = start_pc;
      reg_type = reg_type;
      mem_type = mem_type;
      flag = (Entry.get_top_type, Entry.get_top_type);
      branch_hist = [];
      local_var_map = Entry.get_empty_var_map;
      useful_var = SingleExp.SingleVarSet.empty
    }

  let init_block_subtype_from_layout
      (func: Isa.func) (start_var: entry_t) (start_pc: int)
      (mem_layout: 'a MemType.mem_content) : entry_t * (block_subtype_t list) =
    let helper (acc_var: entry_t) (bb: Isa.basic_block) : entry_t * (t * (t list)) =
      let label = bb.label in
      let acc_var, block_type = (init_from_layout label acc_var start_pc mem_layout) in
      acc_var, (block_type, [])
    in
    List.fold_left_map helper start_var func.body

  let get_reg_type (curr_type: t) (r: Isa.register) : entry_t =
    RegType.get_reg_type curr_type.reg_type r

  let set_reg_type (curr_type: t) (r: Isa.register) (new_type: entry_t) : t =
    { curr_type with reg_type = (RegType.set_reg_type curr_type.reg_type r new_type) }

  let get_mem_op_type
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : entry_t =
    let base_r = if base = None then None else Some (get_reg_type curr_type (Option.get base)) in
    let index_r = if index = None then None else Some (get_reg_type curr_type (Option.get index)) in
    let scale_v = if scale = None then 1L else Isa.scale_val (Option.get scale) in
    Entry.get_mem_op_type disp base_r index_r scale_v

  let get_ld_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) :
      entry_t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    (* TODO: Add constriant: addr_type is untainted *)
    let addr_type = get_mem_op_type curr_type disp base index scale in
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let addr_exp = Entry.get_single_exp addr_type in
    let useful_vars = SingleExp.get_vars addr_exp in
    let addr_offset = 
        (addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, SingleExp.SingleConst size))) 
    in
    match MemType.get_mem_type smt_ctx curr_type.mem_type addr_offset with
    | Some (off_w, off_r, e_t) -> e_t, [ Subset (addr_offset, off_r, off_w) ] , useful_vars
    | None -> Entry.get_top_type, [ Unknown addr_offset ], useful_vars

  let set_st_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) (new_type: entry_t) :
      t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    (* TODO: Add constriant: addr_type is untainted *)
    let addr_type = get_mem_op_type curr_type disp base index scale in
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let addr_exp = Entry.get_single_exp addr_type in
    let useful_vars = SingleExp.get_vars addr_exp in
    let addr_offset = 
        (addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, SingleExp.SingleConst size))) 
    in
    match MemType.set_mem_type smt_ctx curr_type.mem_type addr_offset new_type with
    | Some (new_mem, write_constraints) -> { curr_type with mem_type = new_mem }, write_constraints, useful_vars
    | None -> curr_type, [ Unknown addr_offset ], useful_vars
    
  let get_src_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (src: Isa.operand) :
      entry_t * t * (Constraint.t list) =
    match src with
    | ImmOp imm -> (Entry.get_const_type imm, curr_type, [])
    | RegOp r -> (get_reg_type curr_type r, curr_type, [])
    | MemOp (disp, base, index, scale) ->
      (get_mem_op_type curr_type disp base index scale, curr_type, [])
    | LdOp (disp, base, index, scale, size) ->
      let src_type, src_constraint, src_useful =
        get_ld_op_type smt_ctx curr_type disp base index scale size
      in
      (src_type, { curr_type with useful_var = SingleExp.SingleVarSet.union curr_type.useful_var src_useful }, src_constraint)
    | StOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a label op")
  
  let set_dest_op_type
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (dest: Isa.operand)
      (new_type: entry_t) :
      t * (Constraint.t list) =
    match dest with
    | RegOp r -> (set_reg_type curr_type r new_type, [])
    | StOp (disp, base, index, scale, size) ->
      let next_type, dest_constraint, dest_useful =
        set_st_op_type smt_ctx curr_type disp base index scale size new_type
      in
      ({ next_type with useful_var = SingleExp.SingleVarSet.union next_type.useful_var dest_useful }, dest_constraint)
    | _ -> arch_type_error ("set_dest_op_type: dest is not reg or st op")

  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (inst: Isa.instruction) :
      t * (Constraint.t list) =
    (* We do not update pc in this function! Should update outside!!! *)
    let curr_type = { curr_type with flag = (Entry.get_top_type, Entry.get_top_type) } in
    match inst with
    | BInst (bop, dest, src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx curr_type src1 in
      let dest_type = Entry.exe_bop_inst bop src0_type src1_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, 
      src0_constraint @ src1_constraint @ dest_constraint
    (* | UInst (Mov, dest, src)
    | UInst (Lea, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map src_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint
    | UInst (MovS, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint
    | UInst (MovZ, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint *)
    | UInst (uop, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = 
        begin match uop with
        | MovS -> Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type
        | MovZ -> Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type
        | _ -> Entry.exe_uop_inst uop src_type
        end 
      in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint
    | Xchg (dest0, dest1, src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx curr_type src1 in
      let next_type, dest0_constraint = set_dest_op_type smt_ctx curr_type dest0 src1_type in
      let next_type, dest1_constraint = set_dest_op_type smt_ctx next_type dest1 src0_type in
      next_type, 
      src0_constraint @ src1_constraint @ dest0_constraint @ dest1_constraint
    | Cmp (src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx curr_type src1 in
      let src0_type = Entry.repl_local_var curr_type.local_var_map src0_type in
      let src1_type = Entry.repl_local_var curr_type.local_var_map src1_type in
      let useful_vars = 
        SingleExp.SingleVarSet.union 
          (SingleExp.get_vars (Entry.get_single_exp src0_type))
          (SingleExp.get_vars (Entry.get_single_exp src1_type))
      in
      { curr_type with flag = (src0_type, src1_type); useful_var = useful_vars },
      src0_constraint @ src1_constraint
    | Test (src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx curr_type src1 in
      let dest_type = Entry.repl_local_var curr_type.local_var_map (Entry.exe_bop_inst Isa.And src0_type src1_type) in
      let useful_vars = SingleExp.get_vars (Entry.get_single_exp dest_type) in
      { curr_type with flag = (dest_type, Entry.get_const_type (Isa.ImmNum 0L)); useful_var = useful_vars },
      src0_constraint @ src1_constraint
    | Push src ->
      let size = Isa.get_op_size src in
      let rsp_type, curr_type, _ = get_src_op_type smt_ctx curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst Isa.Sub rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let curr_type, _ = set_dest_op_type smt_ctx curr_type (Isa.RegOp Isa.RSP) new_rsp_type in
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type (Isa.StOp (None, Some Isa.RSP, None, None, size)) src_type in
      next_type, src_constraint @ dest_constraint
    | Pop dst ->
      let size = Isa.get_op_size dst in
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type (Isa.StOp (None, Some Isa.RSP, None, None, size)) in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dst src_type in
      let rsp_type, curr_type, _ = get_src_op_type smt_ctx curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst Isa.Add rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let next_type, _ = set_dest_op_type smt_ctx next_type (Isa.RegOp Isa.RSP) new_rsp_type in
      next_type, src_constraint @ dest_constraint
    | Nop | Syscall | Hlt -> curr_type, []
    | _ -> arch_type_error "inst not implemented"

  let add_block_subtype
      (label: Isa.label)
      (curr_type: t) (block_subtype: block_subtype_t list) :
      block_subtype_t list =
    List.map (
      fun (x, x_sub) ->
        if x.label = label then x, curr_type :: x_sub
        else x, x_sub
    ) block_subtype

  let update_useful_var
      (curr_type: t) (block_subtype: block_subtype_t list) :
      block_subtype_t list =
    List.map (
      fun (x, x_sub) ->
        if x.label = curr_type.label then { x with useful_var = curr_type.useful_var}, x_sub
        else x, x_sub
    ) block_subtype

  let update_branch_hist_get_not_taken_cond
      (smt_ctx: SmtEmitter.t)
      (cond: Isa.branch_cond) (curr_type: t) :
      t * t * (SmtEmitter.exp_t list) =
    let taken_type_option = CondType.get_taken_type cond curr_type.flag in
    match taken_type_option with
    | None -> curr_type, curr_type, []
    | Some taken_type -> 
      let not_taken_type = CondType.not_cond_type taken_type in
      { curr_type with branch_hist = taken_type :: curr_type.branch_hist}, 
      { curr_type with branch_hist = not_taken_type :: curr_type.branch_hist},
      [ CondType.to_smt_expr smt_ctx not_taken_type ]

  let type_prop_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (inst: Isa.instruction)
      (block_subtype: block_subtype_t list) :
      t * block_subtype_t list =
    let _ = smt_ctx in
    match inst with
    | Jmp label ->
      (* 1. Add curr_type to target block's subtype *)
      let block_subtype = (add_block_subtype label curr_type block_subtype) in
      (* 2. This is the end of the current block, so update useful vars of this block in block_subtype *)
      let block_subtype = (update_useful_var curr_type block_subtype) in
      curr_type, block_subtype
    | Jcond (cond, label) ->
      (* TODO: Add constraint: branch cond is untainted!!! *)
      let taken_type, not_taken_type, not_taken_cond = 
        update_branch_hist_get_not_taken_cond smt_ctx cond curr_type 
      in
      (* 1. Add curr_type + taken to target block's subtype *)
      let block_subtype = (add_block_subtype label taken_type block_subtype) in
      (* 2. Update ctx *)
      SmtEmitter.add_assertions smt_ctx not_taken_cond;
      (* 3. Update useful var and return curr_type + not taken *)
      let l_flag, r_flag = not_taken_type.flag in
      let useful_var = 
        SingleExp.SingleVarSet.union 
          (SingleExp.get_vars (Entry.get_single_exp l_flag))
          (SingleExp.get_vars (Entry.get_single_exp r_flag))
      in
      {not_taken_type with useful_var = useful_var}, block_subtype
    | _ -> arch_type_error (Printf.sprintf "type_prop_branch: %s not supported" (Isa.string_of_instruction inst))

  let type_prop_inst
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (inst: Isa.instruction)
      (block_subtype: block_subtype_t list) :
      t * (Constraint.t list) * block_subtype_t list =
    (* Update pc here!!! *)
    match inst with
    | Jmp _ | Jcond _ ->
      let next_type, block_subtype = type_prop_branch smt_ctx curr_type inst block_subtype in
      {next_type with pc = next_type.pc + 1}, [], block_subtype
    | Call _ ->
      Printf.printf "Warning: haven't implemented so far!";
      {curr_type with pc = curr_type.pc + 1}, [], block_subtype
    | _ ->
      let next_type, constraints = type_prop_non_branch smt_ctx curr_type inst in
      {next_type with pc = next_type.pc + 1}, constraints, block_subtype

  (* let block_subtype_to_constraints *)

end

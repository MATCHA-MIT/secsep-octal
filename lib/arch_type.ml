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
    s ^ "(" ^ (Entry.to_string l) ^ "," ^ (Entry.to_string r) ^ ")"

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
    pc: int;
    reg_type: RegType.t;
    mem_type: MemType.t;
    branch_hist: CondType.t;
    (* smt_ctx: SmtEmitter.t; *)
    local_var_map: Entry.local_var_map_t;
    useful_var: SingleExp.SingleVarSet.t
    (* Maybe add constraint set here!!! *)
  }

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
    | ImmOp imm -> (Entry.get_imm_type imm, curr_type, [])
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
    match inst with
    | Mov (dest, src)
    | Lea (dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map src_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var; pc = next_type.pc + 1}, src_constraint @ dest_constraint
    | MovS (dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var; pc = next_type.pc + 1}, src_constraint @ dest_constraint
    | MovZ (dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var; pc = next_type.pc + 1}, src_constraint @ dest_constraint
    | _ -> arch_type_error "inst not implemented"



end

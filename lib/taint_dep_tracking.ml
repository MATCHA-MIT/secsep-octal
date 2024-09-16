open Mem_offset
open Taint_dep_exp
open Reg_type
open Mem_type
open Isa
open Ir


module TaintDepState = struct
  exception TaintDepStateError of string
  let taint_dep_state_error msg = raise (TaintDepStateError ("[Taint Dep State Error] " ^ msg))

  type t = {
    reg_type: RegTaintType.t;
    mem_type: MemTaintDepType.t
  }

  let init_state_type_from_layout
      (start_taint_var: TaintExp.t)
      (start_dep_var: MemDepExp.t)
      (init_mem: MemRangeType.t) :
      TaintExp.t * MemDepExp.t * t =
    let next_taint_var, r_type = RegTaintType.init_reg_type start_taint_var in
    let (next_taint_var, next_dep_var), m_type = 
      MemTaintDepType.init_mem_type_from_layout 
        (next_taint_var, start_dep_var) init_mem.ptr_list init_mem.mem_type 
    in
    (next_taint_var, next_dep_var,
    {
      reg_type = r_type;
      mem_type = m_type
    })

  let get_op_type (curr_state: t) (op: Ir.operand) : TaintDepExp.t =
    match op with
    | RegOp r -> (List.nth curr_state.reg_type r, MemDepSet MemDepExp.PCSet.empty)
    | MemOp (base_id, offset, full_entry) -> MemTaintDepType.get_mem_type_with_key curr_state.mem_type (base_id, offset) full_entry
    | UnknownOp -> taint_dep_state_error "canont get unknown type entry"
  
  let set_op_type (curr_state: t) (op: Ir.operand) (new_type: TaintDepExp.t) : t =
    match op with
    | RegOp r ->
      let taint, _ = new_type in
      let new_r_type = RegTaintType.set_reg_idx_type curr_state.reg_type r taint in
      {curr_state with reg_type = new_r_type}
    | MemOp (base_id, offset, full_entry) ->
      let new_m_type = 
        MemTaintDepType.set_mem_type_with_key curr_state.mem_type (base_id, offset) full_entry new_type 
      in
      {curr_state with mem_type = new_m_type}
    | UnknownOp -> taint_dep_state_error "canont set unknown type entry"

  let update (curr_state: t) (pc: MemDepExp.pc) (dest: Ir.operand) (src_list: Ir.operand list) : t * ((Isa.imm_var_id * MemOffset.t * MemDepExp.t) option) =
    let src_taint_dep_list = List.map (get_op_type curr_state) src_list in
    let src_taint_list, src_dep_list = List.split src_taint_dep_list in
    let _, dest_dep = get_op_type curr_state dest in
    let new_dest_taint = TaintExp.merge_all src_taint_list in
    let curr_pc_dep = MemDepExp.MemDepSet (MemDepExp.PCSet.singleton pc) in
    let new_src_dep_list = List.map (MemDepExp.merge curr_pc_dep) src_dep_list in
    let new_src_list = List.combine src_taint_list new_src_dep_list in
    let new_dest_dep, full_st_opt = 
      match dest with
      | MemOp (full_st_base, full_st_offset, true) (* full store *) ->
        if List.find_opt (Ir.cmp_operand dest) src_list <> None then (MemDepExp.merge curr_pc_dep dest_dep, Some (full_st_base, full_st_offset))
        else (curr_pc_dep, Some (full_st_base, full_st_offset))
      | _ -> (curr_pc_dep, None)
    in
    let update_src_state = List.fold_left2 set_op_type curr_state src_list new_src_list in
    let update_dest_state = set_op_type update_src_state dest (new_dest_taint, new_dest_dep) in
    match full_st_opt with
    | Some (base, offset) -> (update_dest_state, Some (base, offset, dest_dep))
    | None -> (update_dest_state, None)

end

open Isa
open Ir
open Type_exp
open Type_full_exp
open Mem_offset
open Mem_type
open State_type
open Cond_type
open Subtype
open Pretty_print

module ProgType = struct
  exception ProgTypeError of string
  let prog_type_error msg = raise (ProgTypeError ("[Prog Type Error] " ^ msg))

  type block_type = {
    label: Isa.label;
    block_type: StateType.t;
  }

  type t = {
    prog: Isa.program;
    ir_prog: IrProgram.t;
    prog_type: block_type list;
    cond_type: CondType.t list;
    subtype_sol: SubType.t;
    constraint_set: MemOffset.ConstraintSet.t;
    ptr_set: MemKeySet.t;
    no_ptr_set: MemKeySet.t;
    next_type_var_idx: TypeExp.type_var_id;
    next_single_var_idx: Isa.imm_var_id;
  }

  (* Init / update prog type with proper type var or sym var *)
  let init_prog_type 
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (prog: Isa.program)  :
      (TypeExp.type_var_id * Isa.imm_var_id) * (block_type list) =
    let helper (acc: TypeExp.type_var_id * Isa.imm_var_id) (block: Isa.basic_block) :
        (TypeExp.type_var_id * Isa.imm_var_id) * block_type =
      let type_acc, imm_acc = acc in
      if Isa.is_label_function_entry block.label then begin
        (* let new_acc, state = StateType.init_state_type (Right imm_acc) in *)
        let new_acc, state = StateType.init_state_type (TypeSingle (SingleVar imm_acc)) in
        match new_acc with
        (* | Left _ -> prog_type_error ("init_prog_type: return idx should be imm_idx") *)
        | TypeSingle (SingleVar new_imm_acc) -> ((type_acc, new_imm_acc), { label = block.label; block_type = state }) 
        | _ -> prog_type_error ("init_prog_type: return idx should be single var imm_idx")
      end else begin
        (* let new_acc, state = StateType.init_state_type (Left type_acc) in *)
        let new_acc, state = StateType.init_state_type (TypeVar type_acc) in
        match new_acc with
        | TypeVar new_type_acc -> ((new_type_acc, imm_acc), { label = block.label; block_type = state })
        | _ ->  prog_type_error ("init_prog_type: return idx should be type var type_idx")
      end
    in
    List.fold_left_map helper (start_type_var_idx, start_single_var_idx) prog.bbs

  let update_prog_type
      (update_list: (Isa.imm_var_id * (MemOffset.t * bool) list) list)
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (old_prog_type: block_type list) :
      (TypeExp.type_var_id * Isa.imm_var_id * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t) * (block_type list) =
    let helper 
        (acc: TypeExp.type_var_id * Isa.imm_var_id * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t)
        (block: block_type) : 
        (TypeExp.type_var_id * Isa.imm_var_id * TypeExp.TypeVarSet.t * TypeExp.TypeVarSet.t) * block_type =
      let type_acc, imm_acc, n_set_acc, d_set_acc = acc in
      if Isa.is_label_function_entry block.label then begin
        let new_var, new_n_set, new_d_set, new_block_type = 
            StateType.update_state_type update_list (Right imm_acc) n_set_acc d_set_acc block.block_type in
        match new_var with
        | Left _ -> prog_type_error ("update_prog_type: return idx should be imm_idx")
        | Right new_imm_acc ->
          ((type_acc, new_imm_acc, new_n_set, new_d_set), { block with block_type = new_block_type})
      end else begin
        let new_var, new_n_set, new_d_set, new_block_type = 
            StateType.update_state_type update_list (Left type_acc) n_set_acc d_set_acc block.block_type in
        match new_var with
        | Left new_type_acc -> 
          ((new_type_acc, imm_acc, new_n_set, new_d_set), { block with block_type = new_block_type })
        | Right _ -> prog_type_error ("update_prog_type: return idx should be type_idx")
      end
    in
    List.fold_left_map 
      helper
      (start_type_var_idx, start_single_var_idx, TypeExp.TypeVarSet.empty, TypeExp.TypeVarSet.empty)
      old_prog_type

  (* Update structure and solution (if any) of subtype_sol *)
  let update_subtype
      (new_var_set: TypeExp.TypeVarSet.t) (drop_var_set: TypeExp.TypeVarSet.t)
      (tv_rel_list: SubType.t) : SubType.t =
    let t_list = SubType.remove_unused tv_rel_list drop_var_set in
    SubType.add_new t_list new_var_set

  (* Type propagation -> generate subtype relation and unknown mem access list *)
  let get_temp_sol
      (tv_rel_list: SubType.t) : (TypeExp.type_var_id * TypeFullExp.type_sol) list =
    List.filter_map
      (fun (tv_rel: SubType.type_var_rel) -> 
        if tv_rel.type_sol != TypeFullExp.SolNone then 
          Some (tv_rel.type_var_idx, tv_rel.type_sol)
        else None) 
      tv_rel_list

  let get_label_type (code_type: block_type list) (label: Isa.label) : StateType.t =
    (List.find (fun x -> label = x.label) code_type).block_type

  let prop_one_block 
      (tv_rel: SubType.t)
      (block_type: StateType.t)
      (cond_list: CondType.t list)
      (unknown_mem_list: (TypeFullExp.t * int64) list)
      (prop_constraint_list: MemOffset.ConstraintSet.t)
      (inst_list: Isa.instruction list) 
      (code_type: block_type list)
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) : 
      (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) * (Ir.t list) =
    let helper 
        (acc: SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) 
        (inst: Isa.instruction) : 
        (SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) * Ir.t  =
      let acc_tv_rel, acc_state_type, acc_cond_list, acc_u_mem_list, acc_prop_constraint = acc in
      let new_state_type, new_cond_list, u_mem_list, prop_constraint, ir_inst = 
        StateType.type_prop_inst sol acc_state_type acc_cond_list acc_u_mem_list acc_prop_constraint inst 
      in
      match inst with
      | Jcond (target_label, _) ->
        let taken_state_type = StateType.add_state_type_cond acc_state_type ((List.length new_cond_list) * 2 + 1) in
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel taken_state_type target_state_type in
        ((new_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint), ir_inst)
      | Jmp target_label ->
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel new_state_type target_state_type in
        ((new_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint), ir_inst)
      | _ -> ((acc_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint), ir_inst)
    in
    let (new_tv_rel, _, new_cond_list, new_unknown_list, new_prop_constraint), new_ir_block = 
      List.fold_left_map helper (tv_rel, block_type, cond_list, unknown_mem_list, prop_constraint_list) inst_list 
    in
    ((new_tv_rel, new_cond_list, new_unknown_list, new_prop_constraint), new_ir_block)

  let prop_all_block
      (p: Isa.program)
      (init_code_type: block_type list)
      (init_tv_rel: SubType.t) 
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) :
      (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) * IrProgram.t =
    let helper 
        (acc: (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) * IrProgram.t)
        (block_type: block_type) (block_inst: Isa.basic_block) :
        (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t) * IrProgram.t =
      if block_type.label = block_inst.label then
        let (acc_tv_rel, acc_cond_list, acc_u_mem_list, acc_constraint), old_ir_program = acc in
        let other, ir_insts =
        prop_one_block acc_tv_rel block_type.block_type acc_cond_list acc_u_mem_list acc_constraint block_inst.insts init_code_type sol in
        (other, { label = block_inst.label; insts = ir_insts } :: old_ir_program)
      else 
        prog_type_error ("prop_all_block helper: block type and block label mismatch: " ^ block_type.label ^ " != " ^ block_inst.label)
    in
    let other, ir_program = List.fold_left2 helper ((init_tv_rel, [], [], MemOffset.ConstraintSet.empty), []) init_code_type p.bbs in
    (other, List.rev ir_program)

  (* Solve subtype relation to generate new solution *)
  let solve_subtype
      (tv_rel_list: SubType.t) (cond_list: CondType.t list) (iter: int) :
      SubType.t =
    SubType.solve_vars (SubType.remove_all_var_dummy_sub tv_rel_list) cond_list iter

  (* Try to resolve known mem access with new solution *)
  (* Generate new memory layout (ptr-offset list) *)
  let get_update_list
      (old_mem_type: (Isa.imm_var_id * (MemOffset.t * TypeExp.t) list) list)
      (unknown_mem_list: (TypeFullExp.t * int64) list)
      (subtype_sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (ptr_list: MemKeySet.t) (no_ptr_list: MemKeySet.t) :
      (MemKeySet.t * MemKeySet.t) * ((Isa.imm_var_id * (MemOffset.t * bool) list) list) * MemOffset.ConstraintSet.t =
    (* MemType.pp_addr_exp 0 unknown_mem_list; *)
    let repl_mem_list = MemRangeType.repl_addr_exp unknown_mem_list subtype_sol in
    (* MemType.pp_addr_exp 0 repl_mem_list; *)
    let new_ptr_info, addr_base_range = 
      MemRangeType.get_addr_base_range ptr_list no_ptr_list repl_mem_list in
    let mem_access_list = (MemRangeType.reshape_mem_key_list addr_base_range) in
    Printf.printf "Newly resolved mem access list--------------\n";
    MemRangeType.pp_mem_key 0 mem_access_list;
    Printf.printf "Newly resolved mem access list--------------\n";
    let udpate_list, constraint_set = MemRangeType.update_offset_all_ptr old_mem_type mem_access_list in
    (new_ptr_info, udpate_list, constraint_set)

  let init
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (prog: Isa.program) : t =
    let (next_type_var_idx, next_single_var_idx), prog_type =
      init_prog_type start_type_var_idx start_single_var_idx prog in
    {
      prog = prog;
      ir_prog = [];
      prog_type = prog_type;
      cond_type = [];
      subtype_sol = SubType.init next_type_var_idx;
      constraint_set = MemOffset.ConstraintSet.empty;
      ptr_set = MemKeySet.empty;
      no_ptr_set = MemKeySet.empty;
      next_type_var_idx = next_type_var_idx;
      next_single_var_idx = next_single_var_idx;
    }

  let loop (state: t) : t =
    let temp_sol = get_temp_sol state.subtype_sol in
    let (tv_rel, cond_list, unknown_list, prop_constraint), ir_prog = 
      prop_all_block state.prog state.prog_type (SubType.clear state.subtype_sol) temp_sol in
    let sol_tv_rel = solve_subtype tv_rel cond_list 4 in
    Printf.printf "HHH-------------------\n";
    (* SubType.pp_tv_rels 0 sol_tv_rel; *)
    Printf.printf "Unknown list\n";
    MemRangeType.pp_addr_exp 0 unknown_list;
    Printf.printf "hhh-------------------\n";
    let new_sol = get_temp_sol sol_tv_rel in
    let old_mem_type = (List.hd state.prog_type).block_type.mem_type.mem_type in
    MemRangeType.pp_mem_type 0 old_mem_type;
    Printf.printf "ggg-------------------\n";
    let (ptr_list, no_ptr_list), update_list, update_constraint = get_update_list old_mem_type unknown_list new_sol state.ptr_set state.no_ptr_set in
    let (next_type_var, next_imm_var, new_type_var_list, drop_type_var_list), new_prog_type =
      update_prog_type update_list state.next_type_var_idx state.next_single_var_idx state.prog_type in
    let new_subtype = update_subtype new_type_var_list drop_type_var_list sol_tv_rel in
    {
      prog = state.prog;
      ir_prog = ir_prog;
      prog_type = new_prog_type;
      cond_type = cond_list;
      subtype_sol = new_subtype;
      constraint_set = MemOffset.constraint_union [state.constraint_set; prop_constraint; update_constraint];
      ptr_set = ptr_list;
      no_ptr_set = no_ptr_list;
      next_type_var_idx = next_type_var;
      next_single_var_idx = next_imm_var;
    }

  let pp_prog_type (lvl: int) (state: t) =
    Printf.printf "===================================================\n";
    IrProgram.pp_ir_program lvl state.ir_prog;
    PP.print_lvl 0 "\n";
    PP.print_lvl lvl "Prog type\n";
    List.iter (fun x ->
      PP.print_lvl lvl "<Block \"%s\">\n" x.label;
      StateType.pp_state_type (lvl + 1) x.block_type;
      Printf.printf "\n"
    ) state.prog_type;
    CondType.pp_cond_list lvl state.cond_type;
    SubType.pp_tv_rels lvl state.subtype_sol;
    MemOffset.pp_constraint_set lvl state.constraint_set;
    Printf.printf "\nNext type var %d\nNext single var %d\n" state.next_type_var_idx state.next_single_var_idx;
    Printf.printf "===================================================\n"

  let gen
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (prog: Isa.program)
      (iter: int) : t =
    let init_prog = init start_type_var_idx start_single_var_idx prog in
    let rec helper (prog: t) (iter: int) : t =
      (* pp_prog_type 0 prog; *)
      if iter = 0 then begin 
        pp_prog_type 0 prog;
        prog 
      end else 
        helper (loop prog) (iter - 1)
    in
    helper init_prog iter

end

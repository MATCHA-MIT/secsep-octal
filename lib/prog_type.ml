open Isa
open Single_exp
open Type_exp
open Type_full_exp
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
    prog_type: block_type list;
    cond_type: CondType.t list;
    subtype_sol: SubType.t;
    ptr_set: MemType.MemKeySet.t;
    no_ptr_set: MemType.MemKeySet.t;
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
        let new_acc, state = StateType.init_state_type (Right imm_acc) in
        match new_acc with
        | Left _ -> prog_type_error ("init_prog_type: return idx should be imm_idx")
        | Right new_imm_acc -> ((type_acc, new_imm_acc), { label = block.label; block_type = state }) 
      end else begin
        let new_acc, state = StateType.init_state_type (Left type_acc) in
        match new_acc with
        | Left new_type_acc -> ((new_type_acc, imm_acc), { label = block.label; block_type = state })
        | Right _ ->  prog_type_error ("init_prog_type: return idx should be type_idx")
      end
    in
    List.fold_left_map helper (start_type_var_idx, start_single_var_idx) prog.bbs

  let update_prog_type
      (update_list: (Isa.imm_var_id * (SingleExp.t * SingleExp.t * bool) list) list)
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
          ((type_acc, new_type_acc, new_n_set, new_d_set), { block with block_type = new_block_type})
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
      (inst_list: Isa.instruction list) 
      (code_type: block_type list)
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) : 
      SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) =
    let helper 
        (acc: SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list)) 
        (inst: Isa.instruction) : 
        SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list)  =
      let acc_tv_rel, acc_state_type, acc_cond_list, old_u_mem_list = acc in
      let new_state_type, new_cond_list, u_mem_list = StateType.type_prop_inst sol acc_state_type acc_cond_list old_u_mem_list inst in
      match inst with
      | Jcond (target_label, _) ->
        let taken_state_type = StateType.add_state_type_cond acc_state_type ((List.length new_cond_list) * 2 + 1) in
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel taken_state_type target_state_type in
        (new_tv_rel, new_state_type, new_cond_list, u_mem_list)
      | Jmp target_label ->
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type acc_tv_rel new_state_type target_state_type in
        (new_tv_rel, new_state_type, new_cond_list, u_mem_list)
      | _ -> (acc_tv_rel, new_state_type, new_cond_list, u_mem_list)
    in
    let new_tv_rel, _, new_cond_list, new_unknown_list = List.fold_left helper (tv_rel, block_type, cond_list, unknown_mem_list) inst_list in
    (new_tv_rel, new_cond_list, new_unknown_list)

  let prop_all_block
      (p: Isa.program)
      (init_code_type: block_type list)
      (init_tv_rel: SubType.t) 
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) :
      SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) =
    let helper 
        (acc: SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list))
        (block_type: block_type) (block_inst: Isa.basic_block) :
        SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) =
      if block_type.label = block_inst.label then
        let acc_tv_rel, acc_cond_list, acc_u_mem_list = acc in
        prop_one_block acc_tv_rel block_type.block_type acc_cond_list acc_u_mem_list block_inst.insts init_code_type sol
      else 
        prog_type_error ("prop_all_block helper: block type and block label mismatch: " ^ block_type.label ^ " != " ^ block_inst.label)
    in
    List.fold_left2 helper (init_tv_rel, [], []) init_code_type p.bbs

  (* Solve subtype relation to generate new solution *)
  let solve_subtype
      (tv_rel_list: SubType.t) (cond_list: CondType.t list) (iter: int) :
      SubType.t =
    SubType.solve_vars (SubType.remove_all_var_dummy_sub tv_rel_list) cond_list iter

  (* Try to resolve known mem access with new solution *)
  (* Generate new memory layout (ptr-offset list) *)
  let get_update_list
      (old_mem_type: (Isa.imm_var_id * (SingleExp.t * SingleExp.t * TypeExp.t) list) list)
      (unknown_mem_list: (TypeFullExp.t * int64) list)
      (subtype_sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (ptr_list: MemType.MemKeySet.t) (no_ptr_list: MemType.MemKeySet.t) :
      (MemType.MemKeySet.t * MemType.MemKeySet.t) * ((Isa.imm_var_id * (SingleExp.t * SingleExp.t * bool) list) list) =
    (* MemType.pp_addr_exp 0 unknown_mem_list; *)
    let repl_mem_list = MemType.repl_addr_exp unknown_mem_list subtype_sol in
    (* MemType.pp_addr_exp 0 repl_mem_list; *)
    let new_ptr_info, addr_base_range = 
      MemType.get_addr_base_range ptr_list no_ptr_list repl_mem_list in
    (new_ptr_info, MemType.update_offset_all_ptr old_mem_type (MemType.reshape_mem_key_list addr_base_range))

  let init
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (prog: Isa.program) : t =
    let (next_type_var_idx, next_single_var_idx), prog_type =
      init_prog_type start_type_var_idx start_single_var_idx prog in
    {
      prog = prog;
      prog_type = prog_type;
      cond_type = [];
      subtype_sol = SubType.init next_type_var_idx;
      ptr_set = MemType.MemKeySet.empty;
      no_ptr_set = MemType.MemKeySet.empty;
      next_type_var_idx = next_type_var_idx;
      next_single_var_idx = next_single_var_idx;
    }

  let loop (state: t) : t =
    let temp_sol = get_temp_sol state.subtype_sol in
    let tv_rel, cond_list, unknown_list = 
      prop_all_block state.prog state.prog_type (SubType.clear state.subtype_sol) temp_sol in
    let sol_tv_rel = solve_subtype tv_rel cond_list 4 in
    let new_sol = get_temp_sol sol_tv_rel in
    let old_mem_type = (List.hd state.prog_type).block_type.mem_type.mem_type in
    let (ptr_list, no_ptr_list), update_list = get_update_list old_mem_type unknown_list new_sol state.ptr_set state.no_ptr_set in
    let (next_type_var, next_imm_var, new_type_var_list, drop_type_var_list), new_prog_type =
      update_prog_type update_list state.next_type_var_idx state.next_single_var_idx state.prog_type in
    let new_subtype = update_subtype new_type_var_list drop_type_var_list sol_tv_rel in
    {
      prog = state.prog;
      prog_type = new_prog_type;
      cond_type = cond_list;
      subtype_sol = new_subtype;
      ptr_set = ptr_list;
      no_ptr_set = no_ptr_list;
      next_type_var_idx = next_type_var;
      next_single_var_idx = next_imm_var;
    }

  let pp_prog_type (lvl: int) (state: t) =
    Printf.printf "===================================================\n";
    PP.print_lvl lvl "Prog type\n";
    List.iter (fun x ->
      PP.print_lvl lvl "<Block \"%s\">\n" x.label;
      StateType.pp_state_type (lvl + 1) x.block_type;
      Printf.printf "\n"
    ) state.prog_type;
    CondType.pp_cond_list lvl state.cond_type;
    SubType.pp_tv_rels lvl state.subtype_sol;
    Printf.printf "\nNext type var %d\nNext single var %d\n" state.next_type_var_idx state.next_single_var_idx;
    Printf.printf "===================================================\n"

  let gen
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (prog: Isa.program)
      (iter: int) : t =
    let init_prog = init start_type_var_idx start_single_var_idx prog in
    let rec helper (prog: t) (iter: int) : t =
      pp_prog_type 0 prog;
      if iter = 0 then prog else helper (loop prog) (iter - 1)
    in
    helper init_prog iter

end

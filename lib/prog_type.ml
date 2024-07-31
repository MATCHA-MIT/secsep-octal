open Isa
open Ir
open Type_exp
open Type_full_exp
open Mem_offset
open Mem_type
open State_type
open Cond_type
open Subtype
open Smt_emitter
(* open Single_exp *)
open Pretty_print

module ProgType = struct
  exception ProgTypeError of string
  let prog_type_error msg = raise (ProgTypeError ("[Prog Type Error] " ^ msg))

  type block_type = {
    label: Isa.label;
    block_type: StateType.t;
    block_pc_idx: TypeExp.type_var_id;
  }

  type t = {
    prog: Isa.program;
    ir_prog: IrProgram.t;
    prog_type: block_type list;
    cond_type: CondType.t list;
    subtype_sol: SubType.t;
    useful_set: TypeExp.TypeVarSet.t;
    ptr_set: MemKeySet.t;
    no_ptr_set: MemKeySet.t;
    next_type_var_idx: TypeExp.type_var_id;
    next_single_var_idx: Isa.imm_var_id;
    smt_ctx: SmtEmitter.t
  }

  (* Init / update prog type with proper type var or sym var *)
  let init_prog_type 
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (init_mem: MemRangeType.t)
      (prog: Isa.program)  :
      (TypeExp.type_var_id * Isa.imm_var_id) * (block_type list) =
    (* Only update ptr val in base, but type single var idx are still not correct in init_mem *)
    let init_mem = MemRangeType.sort_mem_type init_mem in
    let init_mem = MemRangeType.update_mem_entry_base_id init_mem start_single_var_idx in
    let init_mem = MemRangeType.to_absolute_offset init_mem in
    let helper (acc: TypeExp.type_var_id * Isa.imm_var_id * TypeExp.type_var_id) (block: Isa.basic_block) :
        (TypeExp.type_var_id * Isa.imm_var_id * TypeExp.type_var_id) * block_type =
      let type_acc, imm_acc, start_pc_dest_idx = acc in
      if Isa.is_label_function_entry block.label then begin
        (* let new_acc, state = StateType.init_state_type (Right imm_acc) in *)
        let new_acc, state = StateType.init_state_type_from_layout (TypeSingle (SingleVar imm_acc)) init_mem in
        match new_acc with
        (* | Left _ -> prog_type_error ("init_prog_type: return idx should be imm_idx") *)
        | TypeSingle (SingleVar new_imm_acc) -> ((type_acc, new_imm_acc, start_pc_dest_idx - List.length block.insts), { label = block.label; block_type = state; block_pc_idx = start_pc_dest_idx; }) 
        | _ -> prog_type_error ("init_prog_type: return idx should be single var imm_idx")
      end else begin
        (* let new_acc, state = StateType.init_state_type (Left type_acc) in *)
        let new_acc, state = StateType.init_state_type_from_layout (TypeVar type_acc) init_mem in
        match new_acc with
        | TypeVar new_type_acc -> ((new_type_acc, imm_acc, start_pc_dest_idx - List.length block.insts), { label = block.label; block_type = state; block_pc_idx = start_pc_dest_idx; })
        | _ ->  prog_type_error ("init_prog_type: return idx should be type var type_idx")
      end
    in
    let (type_var_idx, imm_var_idx, _), init_type = List.fold_left_map helper (start_type_var_idx, start_single_var_idx, -1) prog.bbs in
    (type_var_idx, imm_var_idx), init_type

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
      (smt_ctx: SmtEmitter.t)
      (tv_rel: SubType.t)
      (block_type: StateType.t)
      (cond_list: CondType.t list)
      (unknown_mem_list: (TypeFullExp.t * int64) list)
      (prop_constraint_list: MemOffset.ConstraintSet.t)
      (prop_useful_var: TypeExp.TypeVarSet.t)
      (block_start_pc_idx: TypeExp.type_var_id)
      (inst_list: Isa.instruction list) 
      (code_type: block_type list)
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) : 
      (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * (Ir.t list) =
    let helper 
        (acc: SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t * TypeExp.type_var_id) 
        (inst: Isa.instruction) : 
        (SubType.t * StateType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t * TypeExp.type_var_id) * Ir.t  =
      let acc_tv_rel, acc_state_type, acc_cond_list, acc_u_mem_list, acc_prop_constraint, acc_useful_set, acc_pc_dest_idx = acc in
      let new_state_type, new_cond_list, u_mem_list, prop_constraint, new_useful_var, new_var_type_opt, ir_inst = 
        StateType.type_prop_inst smt_ctx sol acc_state_type acc_cond_list acc_u_mem_list acc_prop_constraint acc_useful_set acc_pc_dest_idx inst 
      in
      let new_state_type = StateType.set_default_cond_type new_state_type inst in
      let new_tv_rel = SubType.add_type_var_rel_opt acc_tv_rel acc_pc_dest_idx new_var_type_opt in
      match inst with
      | Jcond (target_label, _) ->
        let taken_state_type = StateType.add_state_type_cond acc_state_type ((List.length new_cond_list) * 2 + 1) in
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type new_tv_rel taken_state_type target_state_type in
        ((new_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint, new_useful_var, acc_pc_dest_idx - 1), ir_inst) (* acc_pc_dest_idx is -pc, so need to -1 *)
      | Jmp target_label ->
        let target_state_type = get_label_type code_type target_label in
        let new_tv_rel = SubType.add_sub_state_type new_tv_rel new_state_type target_state_type in
        ((new_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint, new_useful_var, acc_pc_dest_idx - 1), ir_inst)
      | _ -> ((new_tv_rel, new_state_type, new_cond_list, u_mem_list, prop_constraint, new_useful_var, acc_pc_dest_idx - 1), ir_inst) (* acc_pc_dest_idx is -pc, so need to -1 *)
    in
    let (new_tv_rel, _, new_cond_list, new_unknown_list, new_prop_constraint, new_prop_useful_var, _), new_ir_block = 
      List.fold_left_map helper (tv_rel, block_type, cond_list, unknown_mem_list, prop_constraint_list, prop_useful_var, block_start_pc_idx) inst_list 
    in
    ((new_tv_rel, new_cond_list, new_unknown_list, new_prop_constraint, new_prop_useful_var), new_ir_block)

  let prop_all_block
      (smt_ctx: SmtEmitter.t)
      (p: Isa.program)
      (init_code_type: block_type list)
      (init_tv_rel: SubType.t)
      (sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list) :
      (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * IrProgram.t =
    let helper 
        (acc: (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * IrProgram.t)
        (block_type: block_type) (block_inst: Isa.basic_block) :
        (SubType.t * (CondType.t list) * ((TypeFullExp.t * int64) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * IrProgram.t =
      if block_type.label = block_inst.label then
        let (acc_tv_rel, acc_cond_list, acc_u_mem_list, acc_constraint, acc_useful_set), old_ir_program = acc in
        let other, ir_insts =
        prop_one_block smt_ctx acc_tv_rel block_type.block_type acc_cond_list acc_u_mem_list acc_constraint acc_useful_set block_type.block_pc_idx block_inst.insts init_code_type sol in
        (other, { label = block_inst.label; insts = ir_insts } :: old_ir_program)
      else 
        prog_type_error ("prop_all_block helper: block type and block label mismatch: " ^ block_type.label ^ " != " ^ block_inst.label)
    in
    let other, ir_program = List.fold_left2 helper ((init_tv_rel, [], [], MemOffset.ConstraintSet.empty, TypeExp.TypeVarSet.empty), []) init_code_type p.bbs in
    (other, List.rev ir_program)

  (* A dirty fix: replace itermediate vars (with idx < 0) with its subtype expression (should be unique)
     so that we can derive loop invariance easily *)
  let fix_cond_list (cond_list: CondType.t list) (tv_rel_list: SubType.t) : CondType.t list =
    let helper (acc: CondType.t list) (tv_rel: SubType.type_var_rel) : CondType.t list =
      if tv_rel.type_var_idx < 0 then
        match tv_rel.subtype_list with
        | (hd, _) :: [] -> List.map (fun x -> CondType.repl_type_var_type_exp x (tv_rel.type_var_idx, hd)) acc
        | [] -> acc (* This case is for unused neg vars *)
        | _ -> 
          prog_type_error ("fix_cond_list: neg type var " ^ (string_of_int tv_rel.type_var_idx) ^ " has more than one subtype!!!")
      else
        acc
    in
    List.fold_left helper cond_list tv_rel_list

  let get_cond_list_useful (cond_list: CondType.t list) : TypeExp.TypeVarSet.t =
    let helper (acc: TypeExp.TypeVarSet.t) (cond: CondType.t) : TypeExp.TypeVarSet.t =
      TypeExp.TypeVarSet.union acc (CondType.get_vars cond)
    in
    List.fold_left helper TypeExp.TypeVarSet.empty cond_list

  (* Solve subtype relation to generate new solution *)
  let solve_subtype
      (smt_ctx: SmtEmitter.t) (tv_rel_list: SubType.t) (cond_list: CondType.t list) (useful_var: TypeExp.TypeVarSet.t) (iter: int) :
      SubType.t * (CondType.t list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t =
    SubType.solve_vars smt_ctx (SubType.remove_all_var_dummy_sub tv_rel_list) cond_list useful_var iter

  (* Try to resolve known mem access with new solution *)
  (* Generate new memory layout (ptr-offset list) *)
  let get_update_list
      (smt_ctx: SmtEmitter.t)
      (old_mem_type: (Isa.imm_var_id * (MemOffset.t * TypeExp.t) list) list)
      (unknown_mem_list: (TypeFullExp.t * int64) list)
      (subtype_sol: (TypeExp.type_var_id * TypeFullExp.type_sol) list)
      (ptr_list: MemKeySet.t) (no_ptr_list: MemKeySet.t) :
      (MemKeySet.t * MemKeySet.t) * ((Isa.imm_var_id * (MemOffset.t * bool) list) list) * MemOffset.ConstraintSet.t =
    let helper_merge (mem: (Isa.imm_var_id * (MemOffset.t list)) list) (new_rec: (Isa.imm_var_id * MemOffset.t) list) =
      let rec helper
          (acc: (Isa.imm_var_id * (MemOffset.t list)) list)
          (new_rec: (Isa.imm_var_id * MemOffset.t) list)
          : (Isa.imm_var_id * (MemOffset.t list)) list =
        match new_rec with
        | [] -> acc
        | (base_id, offset) :: tl ->
          if List.exists (fun (x, _) -> x = base_id) acc then
            let new_acc = List.map (fun (x, y) -> if x = base_id then (x, offset :: y) else (x, y)) acc in
            helper new_acc tl
          else
            prog_type_error ("get_update_list: base id " ^ (string_of_int base_id) ^ " claimed that base found, but it does not exist in mem list")
      in
      List.map (fun (x, y) -> (x, List.rev y)) (helper mem new_rec)
    in
    (* MemType.pp_addr_exp 0 unknown_mem_list; *)
    let repl_mem_list = MemRangeType.repl_addr_exp unknown_mem_list subtype_sol in
    let new_ptr_info, addr_base_range = 
      MemRangeType.get_addr_base_range ptr_list no_ptr_list repl_mem_list in
    let access_with_base, access_without_base = (MemRangeType.reshape_mem_key_list addr_base_range) in
    Printf.printf "Newly resolved mem access list--------------\n";
    MemRangeType.pp_mem_key 0 (access_with_base, []);
    Printf.printf "Newly resolved mem access list--------------\n";
    let ib_base_found, ib_base_not_found = MemRangeType.match_base_for_ptr_without_base smt_ctx old_mem_type access_without_base in
    let update_list, constraint_set, undeter_access =
      MemRangeType.update_offset_all_ptr smt_ctx old_mem_type (helper_merge access_with_base ib_base_found)
    in
    Printf.printf "Still unresolved mem access list--------------\n";
    MemRangeType.pp_mem_key 0 (MemRangeType.reshape_mem_key_list (ib_base_not_found @ undeter_access));
    Printf.printf "Still unresolved mem access list--------------\n";
    (new_ptr_info, update_list, constraint_set)

  let init
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (init_mem: MemRangeType.t)
      (prog: Isa.program) : t =
    let (next_type_var_idx, next_single_var_idx), prog_type =
      init_prog_type start_type_var_idx start_single_var_idx init_mem prog in
    (* initial information for z3 solver about the interface *)
    let smt_ctx = SmtEmitter.init_smt_ctx () in
    (* TODO: acquire extra info from interface rather than this dirty assertion *)
    (* let (z3_ctx, z3_solver) = smt_ctx in
    Z3.Solver.add z3_solver [
      Z3.BitVector.mk_slt z3_ctx (SmtEmitter.expr_of_single_exp smt_ctx (SingleExp.SingleVar 6)) (SmtEmitter.mk_numeral smt_ctx 0L)
    ]; *)
    {
      prog = prog;
      ir_prog = [];
      prog_type = prog_type;
      cond_type = [];
      subtype_sol = SubType.init next_type_var_idx;
      useful_set = TypeExp.TypeVarSet.empty;
      ptr_set = MemKeySet.empty;
      no_ptr_set = MemKeySet.empty;
      next_type_var_idx = next_type_var_idx;
      next_single_var_idx = next_single_var_idx;
      smt_ctx = smt_ctx;
    }

  let loop (state: t) : t =
    let z3_ctx, z3_solver = state.smt_ctx in
    let result = Z3.Solver.check z3_solver [] in
    if result = Z3.Solver.UNSATISFIABLE then begin
      Printf.printf "FATAL: SMT Unsatisfiable\n";
      Printf.printf "%s\n" (Z3.Solver.to_string z3_solver);
      Printf.printf "loop quitted\n";
      prog_type_error "FATAL error in loop"
    end;
    let temp_sol = get_temp_sol state.subtype_sol in
    let (tv_rel, cond_list, unknown_list, prop_constraint, prop_useful_var), ir_prog = 
      prop_all_block state.smt_ctx state.prog state.prog_type (SubType.clear state.subtype_sol) temp_sol in
    (* Printf.printf "!!!SubType--------------------------------------------\n";
    SubType.pp_tv_rels 0 tv_rel;
    Printf.printf "!!!SubType--------------------------------------------\n"; *)
    let cond_list = fix_cond_list cond_list tv_rel in
    let cond_list_useful = get_cond_list_useful cond_list in
    let sol_tv_rel, sol_cond_list, sol_constraint, sol_useful_var = 
      solve_subtype state.smt_ctx tv_rel cond_list (TypeExp.var_union [state.useful_set; prop_useful_var; cond_list_useful]) 5 in
    Printf.printf "HHH-------------------\n";
    (* SubType.pp_tv_rels 0 sol_tv_rel; *)
    Printf.printf "Unknown list\n";
    MemRangeType.pp_addr_exp 0 unknown_list;
    Printf.printf "hhh-------------------\n";
    let new_sol = get_temp_sol sol_tv_rel in
    let old_mem_type = (List.hd state.prog_type).block_type.mem_type.mem_type in
    MemRangeType.pp_mem_type 0 old_mem_type;
    Printf.printf "ggg-------------------\n";
    let (ptr_list, no_ptr_list), update_list, update_constraint = get_update_list state.smt_ctx old_mem_type unknown_list new_sol state.ptr_set state.no_ptr_set in
    let (next_type_var, next_imm_var, new_type_var_list, drop_type_var_list), new_prog_type =
      update_prog_type update_list state.next_type_var_idx state.next_single_var_idx state.prog_type in
    let new_subtype = update_subtype new_type_var_list drop_type_var_list sol_tv_rel in
    let new_smt_assertions = 
      MemOffset.constraint_set_to_solver_exps
        state.smt_ctx
        (MemOffset.constraint_union [prop_constraint; sol_constraint; update_constraint])
    in
    (* print all assertions *)
    Printf.printf "New SMT assertions-------------------------------------\n";
    List.iter (fun x -> Printf.printf "%s\n" (Z3.Expr.to_string x)) new_smt_assertions;
    Printf.printf "New SMT assertions-------------------------------------\n";
    Z3.Solver.add z3_solver new_smt_assertions;
    {
      prog = state.prog;
      ir_prog = ir_prog;
      prog_type = new_prog_type;
      cond_type = sol_cond_list;
      subtype_sol = new_subtype;
      useful_set = sol_useful_var;
      ptr_set = ptr_list;
      no_ptr_set = no_ptr_list;
      next_type_var_idx = next_type_var;
      next_single_var_idx = next_imm_var;
      smt_ctx = (z3_ctx, z3_solver);
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
    Printf.printf "CondType-------------------------------------------\n";
    CondType.pp_cond_list lvl state.cond_type;
    Printf.printf "SubType--------------------------------------------\n";
    SubType.pp_tv_rels lvl state.subtype_sol;
    Printf.printf "UsefulVar------------------------------------------\n";
    TypeExp.pp_type_var_set lvl state.useful_set;
    Printf.printf "\nNext type var %d\nNext single var %d\n" state.next_type_var_idx state.next_single_var_idx;
    Printf.printf "===================================================\n"

  let gen
      (start_type_var_idx: TypeExp.type_var_id)
      (start_single_var_idx: Isa.imm_var_id)
      (init_mem: MemRangeType.t)
      (prog: Isa.program)
      (iter: int) : t =
    let init_prog = init start_type_var_idx start_single_var_idx init_mem prog in
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

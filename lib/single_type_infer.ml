open Isa
open Single_exp
open Single_entry_type
open Mem_offset_new
open Constraint
(* open Constraint *)
open Single_subtype
open Smt_emitter

module SingleTypeInfer = struct
  exception SingleTypeInferError of string

  let single_type_infer_error msg = raise (SingleTypeInferError ("[Single Type Infer Error] " ^ msg))

  module ArchType = SingleSubtype.ArchType

  type t = {
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    single_subtype: SingleSubtype.t;
    next_var: SingleEntryType.t;
    input_var_set: SingleEntryType.SingleVarSet.t;
    smt_ctx: SmtEmitter.t
  }

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let init
      (prog: Isa.prog)
      (func_name: string)
      (func_mem_interface: ArchType.MemType.t) : t =
    let global_var_list = List.map (fun (_, x) -> x) (Isa.StrM.to_list prog.imm_var_map) in
    let min_global_var =
      List.fold_left (
        fun acc x -> if x < acc then x else acc
      ) 0 global_var_list
    in
    let start_pc = 1 - min_global_var in
    (* I am still not sure whether to use offset or ptr+offset in MemType, but just add base here :) *)
    let func_mem_interface = ArchType.MemType.add_base_to_offset func_mem_interface in
    let max_var =
      ArchType.MemType.fold_left (
        fun acc (x: SingleEntryType.t) ->
          match x with
          | SingleVar v -> if v > acc then v else acc
          | SingleTop -> acc
          | _ -> single_type_infer_error "[init] func_mem_interface contains dep type exp"
      ) (Isa.total_reg_num) func_mem_interface
    in
    let start_var = SingleEntryType.SingleVar (max_var + 1) in
    let input_var_set = 
      SingleEntryType.SingleVarSet.of_list 
      (List.init (max_var - min_global_var + 1) (fun x -> x + min_global_var)) 
    in
    let func_body = (Isa.get_func prog func_name).body in
    let (_, next_var), arch_type_list =
      List.fold_left_map (
        fun (start_pc, start_var) (bb: Isa.basic_block) ->
          let next_pc = start_pc + List.length bb.insts in
          if bb.label = func_name then 
            ((next_pc, start_var), 
            ArchType.init_func_input_from_layout bb.label (SingleVar 0) start_pc func_mem_interface)
          else 
            let next_var, arch_type = ArchType.init_from_layout bb.label start_var start_pc func_mem_interface in
            ((next_pc, next_var), arch_type)
      ) (start_pc, start_var) func_body
    in
    ArchType.pp_arch_type_list 0 arch_type_list;
    {
      func = func_body;
      func_type = arch_type_list;
      single_subtype = [];
      next_var = next_var;
      input_var_set = input_var_set;
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks (infer_state: t) (solver_iter: int) : t =
    let ctx, solver = infer_state.smt_ctx in
    let helper (block_subtype: ArchType.block_subtype_t list) (block: Isa.basic_block) (block_type: ArchType.t) : ArchType.block_subtype_t list =
      Z3.Solver.push solver;
      SingleSubtype.update_block_smt_ctx (ctx, solver) infer_state.single_subtype block_type;
      Printf.printf "Block %s solver \n%s\n" block.label (Z3.Solver.to_string solver);
      (* Printf.printf "type_prop_block %s\n" block.label; *)
      let _, block_subtype = ArchType.type_prop_block (ctx, solver) block_type block.insts block_subtype in
      Z3.Solver.pop solver 1;
      block_subtype
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    Printf.printf "func len %d, type len %d\n" (List.length infer_state.func) (List.length infer_state.func_type);
    let block_subtype = List.fold_left2 helper block_subtype infer_state.func infer_state.func_type in
    Printf.printf "block_subtype len %d\n" (List.length block_subtype);
    (* List.iter (
      fun (x: ArchType.block_subtype_t) ->
        let x, _ = x in Printf.printf "Constraint len %d\n" (List.length x.constraint_list)
    ) block_subtype; *)
    let single_subtype, block_subtype = SingleSubtype.init block_subtype in
    Printf.printf "2\n";
    let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype infer_state.input_var_set solver_iter in
    { infer_state with 
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type; 
      single_subtype = single_subtype 
    }

  let update_mem (infer_state: t) : t =
    let update_list = ArchType.MemType.init_stack_update_list (List.nth infer_state.func_type 0).mem_type in
    let helper (acc: (MemOffset.t * bool) list) (a_type: ArchType.t) : (MemOffset.t * bool) list =
      let unknown_list = Constraint.get_unknown a_type.constraint_list in
      (* MemOffset.pp_unknown_list 0 unknown_list; *)
      let unknown_range = 
        List.map (
          fun ((l, r), pc) -> 
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (l, pc),
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (r, pc)
        ) unknown_list
      in
      let new_offset_list = List.filter_map MemOffset.from_range unknown_range in
      (* Printf.printf "update_mem\n";
      MemOffset.pp_off_list 0 new_offset_list;  *)
      MemOffset.insert_new_offset_list infer_state.smt_ctx acc new_offset_list
    in
    let update_list = List.fold_left helper update_list infer_state.func_type in
    let next_var, func_type =
      List.fold_left_map (
        fun (acc: SingleEntryType.t) (a_type: ArchType.t) ->
          let acc, new_mem = ArchType.MemType.update_mem acc a_type.mem_type update_list in
          acc, { a_type with mem_type = new_mem }
      ) infer_state.next_var infer_state.func_type
    in
    let local_var_set = 
      List.fold_left (
        fun acc entry -> SingleExp.SingleVarSet.union acc (ArchType.get_local_var_set entry)
      ) SingleExp.SingleVarSet.empty func_type
    in
    let single_subtype = SingleSubtype.filter_entry infer_state.single_subtype local_var_set in
    { infer_state with
      func_type = func_type;
      single_subtype = single_subtype;
      next_var = next_var
    }
    (* TODO: Also remove removed vars in single_subtype *)

  let clean_up_func_type (infer_state: t) : t =
    { infer_state with
      func_type = List.map ArchType.clean_up infer_state.func_type
    }

  let infer
      (prog: Isa.prog)
      (func_name: string)
      (func_mem_interface: ArchType.MemType.t)
      (iter: int)
      (solver_iter: int) : t =
    let init_infer_state = init prog func_name func_mem_interface in
    let rec helper (state: t) (iter_left: int) : t =
      if iter_left = 0 then
        state
      else begin
        Printf.printf "Infer iter %d type_prop_all_blocks\n" (iter - iter_left + 1);
        let state = type_prop_all_blocks state solver_iter in
        Printf.printf "After infer, single subtype\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype;
        Printf.printf "After infer, unknown list:\n";
        List.iter (
          fun (x: ArchType.t) -> MemOffset.pp_unknown_list 0 (Constraint.get_unknown x.constraint_list)
        ) state.func_type;
        Printf.printf "Infer iter %d update_mem\n" (iter - iter_left + 1);
        let state = update_mem state in
        Printf.printf "After update_mem\n";
        pp_func_type 0 state;
        helper (clean_up_func_type state) (iter_left - 1)
      end
    in
    helper init_infer_state iter

end

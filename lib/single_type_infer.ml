open Isa
open Single_entry_type
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

  let type_prop_all_blocks (infer_state: t) : t =
    let ctx, solver = infer_state.smt_ctx in
    let helper (block_subtype: ArchType.block_subtype_t list) (block: Isa.basic_block) (block_type: ArchType.t) : ArchType.block_subtype_t list =
      Z3.Solver.push solver;
      SingleSubtype.update_block_smt_ctx (ctx, solver) infer_state.single_subtype block_type;
      let _, block_subtype = ArchType.type_prop_block (ctx, solver) block_type block.insts block_subtype in
      Z3.Solver.pop solver 1;
      block_subtype
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    Printf.printf "func len %d, type len %d\n" (List.length infer_state.func) (List.length infer_state.func_type);
    let block_subtype = List.fold_left2 helper block_subtype infer_state.func infer_state.func_type in
    Printf.printf "block_subtype %d\n" (List.length block_subtype);
    let single_subtype, block_subtype = SingleSubtype.init block_subtype in
    Printf.printf "2\n";
    let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype infer_state.input_var_set 5 in
    { infer_state with 
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type; 
      single_subtype = single_subtype 
    }


end

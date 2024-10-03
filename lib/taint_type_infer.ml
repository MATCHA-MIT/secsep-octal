open Isa
open Taint_exp
open Single_entry_type
open Mem_offset_new
open Constraint
open Single_subtype
open Taint_subtype
open Single_type_infer
open Smt_emitter
open Full_mem_anno

module TaintTypeInfer = struct
  exception TaintTypeInferError of string

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno)

  let taint_type_infer_error msg = raise (TaintTypeInferError ("[Taint Type Infer Error] " ^ msg))

  module TaintEntryType = TaintSubtype.TaintEntryType
  module ArchType = TaintSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    single_sol: SingleSubtype.t;
    input_single_var_set: SingleEntryType.SingleVarSet.t;
    taint_sol: TaintExp.local_var_map_t;
    smt_ctx: SmtEmitter.t;
  }

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let init (single_infer_state: SingleTypeInfer.t) : t =
    let start_var = TaintExp.TaintVar 1 in
    let helper_code
        (acc: TaintExp.t) (block: Isa.basic_block) :
        TaintExp.t * Isa.basic_block =
      let inner_helper
          (acc: TaintExp.t) (inst: Isa.instruction) :
          TaintExp.t * Isa.instruction =
        TaintExp.next_var acc, Isa.update_inst_taint (fun anno -> MemAnno.update_taint anno acc) inst
      in
      let acc, new_insts = List.fold_left_map inner_helper acc block.insts in
      acc, { block with insts = new_insts }
    in
    let helper_arch
        (acc: TaintExp.t) (block_single_type: SingleTypeInfer.ArchType.t) :
        TaintExp.t * ArchType.t =
      let inner_helper = fun acc single_entry -> TaintExp.next_var acc, (single_entry, acc) in
      let acc, new_reg_type =
        List.fold_left_map inner_helper acc block_single_type.reg_type
      in
      let acc, new_mem_type =
        ArchType.MemType.fold_left_map inner_helper acc block_single_type.mem_type
      in
      acc, {
        label = block_single_type.label;
        pc = block_single_type.pc;
        reg_type = new_reg_type;
        mem_type = new_mem_type;
        flag = (TaintEntryType.get_top_untaint_type (), TaintEntryType.get_top_untaint_type ());
        branch_hist = [];
        full_not_taken_hist = [];
        constraint_list = [];
        local_var_map = TaintEntryType.get_empty_var_map;
        useful_var = block_single_type.useful_var;
        global_var = block_single_type.global_var;
        prop_mode = ArchType.TypeInferTaint;
      }
    in
    let next_var, func_type = List.fold_left_map helper_arch start_var single_infer_state.func_type in
    let _, func = List.fold_left_map helper_code next_var single_infer_state.func in
    {
      func = func;
      func_type = func_type;
      single_sol = single_infer_state.single_subtype;
      input_single_var_set = single_infer_state.input_var_set;
      taint_sol = [];
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks
      (func_interface_list: FuncInterface.t list)
      (infer_state: t) : t * (ArchType.block_subtype_t list) =
    let ctx, solver = infer_state.smt_ctx in
    let helper 
        (block_subtype: ArchType.block_subtype_t list) 
        (block: Isa.basic_block) 
        (block_type: ArchType.t) : ArchType.block_subtype_t list =
      (* Prepare SMT context for the current block *)
      Z3.Solver.push solver;
      SingleSubtype.update_block_smt_ctx (ctx, solver) infer_state.single_sol block_type.useful_var;
      let (_, block_subtype), _ =
        ArchType.type_prop_block (ctx, solver) 
          (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_sol infer_state.input_single_var_set) 
          func_interface_list block_type block.insts block_subtype
      in
      (* Printf.printf "After prop block %s\n" block.label; *)
      Z3.Solver.pop solver 1;
      block_subtype
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    let block_subtype = List.fold_left2 helper block_subtype infer_state.func infer_state.func_type in
    { infer_state with func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type },
    block_subtype

  let infer_one_func
      (func_interface_list: FuncInterface.t list)
      (single_infer_state: SingleTypeInfer.t) : t =

    let state = init single_infer_state in
    Printf.printf "Before infer, func\n";
    let buf = Buffer.create 1000 in
    Isa.pp_ocaml_block_list 0 buf state.func;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    Printf.printf "Before infer, func_type\n";
    ArchType.pp_arch_type_list 0 state.func_type;
  
    (* 1. Type prop *)
    (* Prepare SMT context *)
    let solver = snd state.smt_ctx in
    Z3.Solver.push solver;
    ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type;
    let state, block_subtype = type_prop_all_blocks func_interface_list state in
    Z3.Solver.pop solver 1;

    Printf.printf "After infer, unknown list:\n";
    List.iter (
      fun (x: ArchType.t) -> 
        Printf.printf "%s\n" x.label;
        MemOffset.pp_unknown_list 0 (Constraint.get_unknown x.constraint_list)
    ) state.func_type;


    (* 2. Taint type infer *)
    let subtype_list = TaintSubtype.get_taint_constraint block_subtype in

    (* Get input var *)
    let input_arch = List.hd state.func_type in
    let merge_helper = (
      fun acc (_, entry) -> TaintExp.TaintVarSet.union acc (TaintExp.get_var_set entry)
    ) in
    let input_var = List.fold_left merge_helper TaintExp.TaintVarSet.empty input_arch.reg_type in
    let input_var = ArchType.MemType.fold_left merge_helper input_var input_arch.mem_type in
    Printf.printf "Input var: %s\n" (TaintExp.to_string (TaintExp.TaintExp input_var));

    let taint_sol = TaintSubtype.solve_subtype_list input_var subtype_list in
    let update_taint = TaintExp.repl_context_var_no_error taint_sol in
    let update_entry = fun (entry: TaintEntryType.t) -> let single, taint = entry in single, update_taint taint in
    let func = Isa.update_block_list_taint (fun t ->
      match MemAnno.get_taint t with
      | None -> t
      | Some taint -> MemAnno.update_taint t (update_taint taint)
    ) state.func in
    let func_type = List.map (ArchType.update_reg_mem_type update_entry) state.func_type in

    Printf.printf "After infer, func\n";
    let buf = Buffer.create 1000 in
    Isa.pp_ocaml_block_list 0 buf func;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    Printf.printf "After infer, func_type\n";
    ArchType.pp_arch_type_list 0 func_type;

    { state with 
      func = func;
      func_type = func_type;
      taint_sol = taint_sol }

end

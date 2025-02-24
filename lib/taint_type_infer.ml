open Isa
open Taint_exp
open Single_entry_type
open Taint_entry_type
open Mem_offset_new
open Stack_spill_info
open Single_context
open Constraint
open Func_interface
open Single_subtype
open Taint_subtype
open Taint_unknown_infer
open Range_type_infer
open Smt_emitter
open Full_mem_anno
open Branch_anno
open Call_anno
open Taint_api
open Sexplib.Std

module TaintTypeInfer = struct
  exception TaintTypeInferError of string

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno) (BranchAnno) (CallAnno)

  let taint_type_infer_error msg = raise (TaintTypeInferError ("[Taint Type Infer Error] " ^ msg))

  module ArchType = TaintSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func_name: Isa.label;
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    stack_spill_info: StackSpillInfo.t;
    single_sol: SingleSubtype.t;
    input_single_var_set: SingleEntryType.SingleVarSet.t;
    context: SingleContext.t list;
    taint_sol: TaintExp.local_var_map_t;
    taint_context: TaintExp.sub_t list;
    ret_subtype_list: (Isa.imm_var_id * (SingleEntryType.t list)) list;
    smt_ctx: SmtEmitter.t;
    alive: bool option;
  }
  [@@deriving sexp]

  let state_list_to_file (filename: string) (infer_result: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t infer_result)

  let state_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let init (range_infer_state: RangeTypeInfer.t) (taint_api_list: TaintApi.t) : t =
    let start_var = TaintExp.TaintVar 1 in
    let helper_code
        (acc: TaintExp.t) (block: Isa.basic_block) :
        TaintExp.t * Isa.basic_block =
      let inner_helper
          (acc: TaintExp.t) (inst: Isa.instruction) :
          TaintExp.t * Isa.instruction =
        TaintExp.next_var acc, Isa.update_inst_taint (fun anno -> MemAnno.update_taint anno acc) (fun x -> x) inst
      in
      let acc, new_insts = List.fold_left_map inner_helper acc block.insts in
      acc, { block with insts = new_insts }
    in
    let helper_arch
        (acc: TaintExp.t) (block_single_type: RangeTypeInfer.ArchType.t) :
        TaintExp.t * ArchType.t =
      (* let inner_helper = fun acc single_entry -> TaintExp.next_var acc, (single_entry, acc) in
      let acc, new_reg_type =
        List.fold_left_map inner_helper acc block_single_type.reg_type
      in
      let acc, new_mem_type =
        ArchType.MemType.fold_left_map inner_helper acc block_single_type.mem_type
      in *)
      let acc, new_reg_type, new_mem_type = TaintApi.get_taint_arch_type taint_api_list acc block_single_type in
      acc, {
        label = block_single_type.label;
        pc = block_single_type.pc;
        dead_pc = block_single_type.dead_pc;
        reg_type = new_reg_type;
        mem_type = new_mem_type;
        context = block_single_type.context;
        blk_br_context = [];
        tmp_context = [];
        flag = (TaintEntryType.get_top_untaint_type (), TaintEntryType.get_top_untaint_type ());
        branch_hist = [];
        full_not_taken_hist = [];
        constraint_list = [];
        extra_call_context_map_list = block_single_type.extra_call_context_map_list;
        extra_call_subtype_list = block_single_type.extra_call_subtype_list;
        extra_call_context_hist = block_single_type.extra_call_context_hist;
        local_var_map = TaintEntryType.get_empty_var_map;
        useful_var = block_single_type.useful_var;
        global_var = block_single_type.global_var;
        prop_mode = ArchType.TypeInferTaint;
      }
    in
    let next_var, func_type = List.fold_left_map helper_arch start_var range_infer_state.func_type in
    let _, func = List.fold_left_map helper_code next_var range_infer_state.func in
    {
      func_name = range_infer_state.func_name;
      func = func;
      func_type = func_type;
      stack_spill_info = range_infer_state.stack_spill_info;
      single_sol = range_infer_state.single_sol;
      input_single_var_set = range_infer_state.input_single_var_set;
      context = range_infer_state.context;
      taint_sol = [];
      taint_context = [];
      ret_subtype_list = range_infer_state.ret_subtype_list;
      smt_ctx = SmtEmitter.init_smt_ctx ();
      alive = None;
    }

  let type_prop_all_blocks
      (func_interface_list: FuncInterface.t list)
      (infer_state: t) : t * (ArchType.block_subtype_t list) =
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let helper 
        (block_subtype: ArchType.block_subtype_t list)
        (block_block_type: Isa.basic_block * ArchType.t) : ArchType.block_subtype_t list * Isa.basic_block =
      let block, block_type = block_block_type in
      if block_type.pc >= block_type.dead_pc then block_subtype, block else begin
      (* Prepare SMT context for the current block *)
      SmtEmitter.push infer_state.smt_ctx;
      SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_sol block_type.useful_var;
      ArchType.add_assertions infer_state.smt_ctx block_type;
      let (_, block_subtype), new_block =
        ArchType.type_prop_block infer_state.smt_ctx 
          (* (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_sol infer_state.input_single_var_set) *)
          (SingleSubtype.sub_sol_single_to_offset_opt 
            (SingleEntryType.eval_align ptr_align_list)
            infer_state.single_sol infer_state.input_single_var_set)
          (SingleSubtype.sub_sol_single_to_offset_list
            (SingleEntryType.eval_align ptr_align_list)
            infer_state.single_sol infer_state.input_single_var_set)
          (StackSpillInfo.is_spill infer_state.stack_spill_info)
          func_interface_list block_type block.insts block_subtype
      in
      (* Printf.printf "After prop block %s\n" block.label; *)
      SmtEmitter.pop infer_state.smt_ctx 1;
      block_subtype, { block with insts = new_block }
      end
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    let block_subtype, new_func = List.fold_left_map helper block_subtype (List.combine infer_state.func infer_state.func_type) in
    { infer_state with 
      func = new_func;
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type },
    block_subtype

  let get_func_interface
      (infer_state: t) : FuncInterface.t =
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let sub_sol =
      SingleSubtype.sub_sol_single_to_single_func_interface
        (SingleEntryType.eval_align ptr_align_list)
        infer_state.single_sol infer_state.input_single_var_set
    in
    let sub_sol_for_taint (pc: int) (taint_entry: TaintEntryType.t) : TaintEntryType.t =
      let single, taint = taint_entry in
      sub_sol pc single, taint
    in
    let func_interface = 
      ArchType.get_func_interface
        infer_state.smt_ctx
        infer_state.func_name
        infer_state.func_type
        infer_state.context
        infer_state.ret_subtype_list
        sub_sol_for_taint
    in

    (* Note: our taint typing rules already constrain that input output mem has the same taint.
       For the simpilicity of taint infer of other functions on calling this function,
       we manually set its out_mem taint with its in_mem taint. *)
    let simplified_out_mem =
      ArchType.MemType.map2 true TaintEntryType.set_taint_with_other func_interface.out_mem func_interface.in_mem
    in
    { func_interface with
      in_taint_context = infer_state.taint_context;
      out_mem = simplified_out_mem }

  let update_with_taint_sol
      (taint_sol: TaintExp.local_var_map_t)
      (state: t) : t =
    let update_taint = TaintExp.repl_context_var_no_error taint_sol in
    let update_entry = fun (entry: TaintEntryType.t) -> let single, taint = entry in single, update_taint taint in
    let func = 
      Isa.update_block_list_taint (fun t ->
        match MemAnno.get_taint t with
        | None -> t
        | Some taint -> MemAnno.update_taint t (update_taint taint)
      ) (CallAnno.update_taint update_taint) state.func 
    in
    let func_type = List.map (ArchType.update_reg_mem_type update_entry) state.func_type in
    let has_invalid_unknown_taint =
      List.find_opt (
        fun (a_type: ArchType.t) ->
          ArchType.MemType.fold_left_full (
            fun (acc: bool) (slot: ArchType.entry_t ArchType.MemType.mem_slot) ->
              if acc then acc else
              let _, range, (_, taint) = slot in
              match taint with
              | TaintUnknown ->
                begin match range with
                | RangeConst [] -> acc
                | _ -> true
                end
              | _ -> acc
          ) false a_type.mem_type
      ) func_type
    in
    if has_invalid_unknown_taint <> None then taint_type_infer_error "has invalid unknown taint" else
    { state with func = func; func_type = func_type }
  

  let infer_one_func
      (func_interface_list: FuncInterface.t list)
      (taint_api_list: TaintApi.t)
      (range_infer_state: RangeTypeInfer.t) : t =
    let state = init range_infer_state taint_api_list in
    Printf.printf "Infer func %s\n" range_infer_state.func_name;
    (* Printf.printf "Before infer, func\n";
    let buf = Buffer.create 1000 in
    Isa.pp_ocaml_block_list 0 buf state.func;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    Printf.printf "Before infer, func_type\n";
    ArchType.pp_arch_type_list 0 state.func_type; *)
  
    (* 1. Type prop *)
    (* Prepare SMT context *)
    SmtEmitter.push state.smt_ctx;
    (* ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type; *)
    SingleContext.add_assertions state.smt_ctx state.context;
    let state, block_subtype = type_prop_all_blocks func_interface_list state in
    SmtEmitter.pop state.smt_ctx 1;

    Printf.printf "After infer, unknown list:\n";
    List.iter (
      fun (x: ArchType.t) -> 
        Printf.printf "%s\n" x.label;
        MemOffset.pp_unknown_list 0 (Constraint.get_unknown x.constraint_list)
    ) state.func_type;

    (* 2. Get unknown variable *)
    let unknown_var_set = TaintUnknownInfer.get_unknown_var_set block_subtype in
    let unknown_sol = List.map (fun (x: TaintExp.taint_var_id) -> x, TaintExp.TaintUnknown) (TaintExp.TaintVarSet.to_list unknown_var_set) in
    Printf.printf "TaintUnknown list\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_int (TaintExp.TaintVarSet.to_list unknown_var_set)));

    (* 2. Taint type infer *)
    Printf.printf "!!!\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_block_subtype_t block_subtype));
    let subtype_list = TaintSubtype.get_taint_constraint unknown_var_set block_subtype in

    (* Get input var *)
    let input_arch = List.hd state.func_type in
    let merge_helper = (
      fun acc (_, entry) -> TaintExp.TaintVarSet.union acc (TaintExp.get_var_set entry)
    ) in
    let input_var = List.fold_left merge_helper TaintExp.TaintVarSet.empty input_arch.reg_type in
    SmtEmitter.push state.smt_ctx;
    SingleContext.add_assertions state.smt_ctx state.context;
    let input_var = 
      ArchType.MemType.fold_left merge_helper input_var 
        (ArchType.MemType.merge_local_mem_quick_cmp state.smt_ctx input_arch.mem_type) 
    in
    SmtEmitter.pop state.smt_ctx 1;
    Printf.printf "Input var: %s\n" (TaintExp.to_string (TaintExp.TaintExp input_var));

    let taint_sol, taint_context = TaintSubtype.solve input_var subtype_list in
    let taint_sol = taint_sol @ unknown_sol in
    update_with_taint_sol taint_sol { state with taint_sol = taint_sol; taint_context = taint_context }
    (* let update_taint = TaintExp.repl_context_var_no_error taint_sol in
    let update_entry = fun (entry: TaintEntryType.t) -> let single, taint = entry in single, update_taint taint in
    let func = 
      Isa.update_block_list_taint (fun t ->
        match MemAnno.get_taint t with
        | None -> t
        | Some taint -> MemAnno.update_taint t (update_taint taint)
      ) (CallAnno.update_taint update_taint) state.func 
    in
    let func_type = List.map (ArchType.update_reg_mem_type update_entry) state.func_type in

    Printf.printf "Taint Sol\n";
    TaintExp.pp_local_var 0 taint_sol;
    Printf.printf "After infer, func\n";
    let buf = Buffer.create 1000 in
    Isa.pp_ocaml_block_list 0 buf func;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf));
    Printf.printf "After infer, func_type\n";
    ArchType.pp_arch_type_list 0 func_type;

    { state with 
      func = func;
      func_type = func_type;
      taint_sol = taint_sol } *)

  let infer 
      (range_infer_state_list: RangeTypeInfer.t list)
      (general_func_interface_list: FuncInterfaceConverter.TaintFuncInterface.t list)
      (taint_api_list: TaintApi.t) : 
      (FuncInterface.t list) * (t list) =
    let helper
        (acc: FuncInterface.t list) (entry: RangeTypeInfer.t) :
        (FuncInterface.t list) * t =
      let infer_state = infer_one_func acc taint_api_list entry in
      let func_interface = get_func_interface infer_state in
      Printf.printf "Infer state of func %s\n" infer_state.func_name;
      (* pp_func_type 0 infer_state; *)
      FuncInterface.pp_func_interface 0 func_interface;
      func_interface :: acc, infer_state
    in
    (* List.fold_left_map helper [] range_infer_state_list *)
    let func_interface_list, infer_result = List.fold_left_map helper general_func_interface_list range_infer_state_list in
    (* Printf.printf "=========================\n";
    Sexp.output_hum stdout (sexp_of_list sexp_of_t infer_result); *)
    List.rev func_interface_list, infer_result

end

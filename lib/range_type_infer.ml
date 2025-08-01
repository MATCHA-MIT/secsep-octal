open Isa
open Single_entry_type
open Mem_offset_new
open Stack_spill_info
open Constraint
(* open Single_context *)
open Single_subtype
open Range_subtype
open Range_subset
open Range_useless_infer
open Func_interface
open Single_type_infer
open Smt_emitter
open Full_mem_anno
open Branch_anno
open Call_anno
open Set_sexp
open Sexplib.Std
(* open Sexplib *)

module RangeTypeInfer = struct
  exception RangeTypeInferError of string

  module MemAnno = FullMemAnno
  module Isa = Isa (MemAnno) (BranchAnno) (CallAnno)

  let range_type_infer_error msg = raise (RangeTypeInferError ("[Range Type Infer Error] " ^ msg))

  module ArchType = RangeSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func_name: Isa.label;
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    stack_spill_info: StackSpillInfo.t;
    single_sol: SingleSubtype.t;
    input_single_var_set: SingleEntryType.SingleVarSet.t;
    (* state_context: SingleContext.t list; *)
    (* range_sol: MemRange.local_var_map_t; *)
    ret_subtype_list: (Isa.imm_var_id * (SingleEntryType.t list)) list;
    smt_ctx: SmtEmitter.t;
  }
  [@@deriving sexp]

  let state_list_to_file (filename: string) (infer_result: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t infer_result);
    close_out channel

  let state_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp
  
  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let init (single_infer_state: SingleTypeInfer.t) : t =
    let func_name = single_infer_state.func_name in
    let start_var = MemRange.RangeVar 1 in
    let helper
        (acc: MemRange.t) (block_single_type: SingleTypeInfer.ArchType.t) :
        MemRange.t * ArchType.t =
      if block_single_type.label = func_name then
        acc, 
        { block_single_type with prop_mode = ArchType.TypeInferInit }
      else
        let acc, mem_type =
          List.fold_left_map (
            fun (acc: MemRange.t) (ptr, part_mem) ->
              let acc, part_mem =
                List.fold_left_map (
                  fun (acc: MemRange.t) (off, range, entry) ->
                    match range with
                    | MemRange.RangeConst [ range_off ] ->
                      if MemOffset.cmp off range_off = 0 then
                        acc, (off, range, entry)
                      else
                        MemRange.next_var acc, (off, acc, entry)
                    | _ -> MemRange.next_var acc, (off, acc, entry)
                ) acc part_mem
              in
              acc, (ptr, part_mem)
          ) acc block_single_type.mem_type
        in
        acc,
        { block_single_type with 
          mem_type = mem_type;
          prop_mode = ArchType.TypeInferInit }
    in
    let _, func_type = List.fold_left_map helper start_var single_infer_state.func_type in
    {
      func_name = single_infer_state.func_name;
      func = single_infer_state.func;
      func_type = func_type;
      stack_spill_info = single_infer_state.stack_spill_info;
      single_sol = single_infer_state.single_subtype;
      input_single_var_set = single_infer_state.input_var_set;
      (* state_context = single_infer_state.state_context; *)
      (* range_sol = []; *)
      ret_subtype_list = single_infer_state.ret_subtype_list;
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks
      (func_interface_list: FuncInterface.t list)
      (infer_state: t) : t * (ArchType.block_subtype_t list) =
    (* TODO: This is the same as the one in TaintTypeInfer. Consider to remove duplicate code. *)
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let helper 
        (block_subtype: ArchType.block_subtype_t list) 
        (block: Isa.basic_block) 
        (block_type: ArchType.t) : ArchType.block_subtype_t list =
      if block_type.pc >= block_type.dead_pc then block_subtype else begin
      (* Prepare SMT context for the current block *)
      SmtEmitter.push infer_state.smt_ctx;
      (* SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_sol block_type.useful_var; *)
      ArchType.add_assertions infer_state.smt_ctx block_type;
      Printf.printf "Infer %s %s init smt_ctx\n" infer_state.func_name block_type.label;
      SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx;
      let (_, block_subtype), _ =
        ArchType.type_prop_block infer_state.smt_ctx 
          (* (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_sol infer_state.input_single_var_set)  *)
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
      block_subtype
      end
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    let block_subtype = List.fold_left2 helper block_subtype infer_state.func infer_state.func_type in
    { infer_state with func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type },
    block_subtype

  let get_func_interface
      (infer_state: t) : FuncInterface.t =
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let sub_sol =
      SingleSubtype.sub_sol_single_to_single_func_interface
        (SingleEntryType.eval_align ptr_align_list)
        infer_state.single_sol infer_state.input_single_var_set
    in
    ArchType.get_func_interface
      infer_state.smt_ctx
      infer_state.func_name
      infer_state.func_type
      (* infer_state.state_context *)
      infer_state.ret_subtype_list
      sub_sol sub_sol

  let infer_one_func
      (func_interface_list: FuncInterface.t list)
      (single_infer_state: SingleTypeInfer.t) : t =
    (* Printf.printf "Single infer func_type\n";
    ArchType.pp_arch_type_list 0 single_infer_state.func_type; *)
    let state = init single_infer_state in
    (* Printf.printf "Before infer, func\n";
    let buf = Buffer.create 1000 in
    Isa.pp_ocaml_block_list 0 buf state.func;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)
    Printf.printf "Before infer, func_type\n";
    ArchType.pp_arch_type_list 0 state.func_type;

    (* Prepare SMT context *)
    SmtEmitter.push state.smt_ctx;

    (* 1. Type prop *)
    (* ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type; *)
    (* SingleContext.add_assertions state.smt_ctx state.state_context; *)
    let state, block_subtype = type_prop_all_blocks func_interface_list state in
    Printf.printf "block_subtype:\n%s\n"
      (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_block_subtype_t block_subtype));

    Printf.printf "After infer, unknown list:\n";
    List.iter (
      fun (x: ArchType.t) -> 
        if x.pc >= x.dead_pc then () else begin
          Printf.printf "%s\n" x.label;
          MemOffset.pp_unknown_list 0 (Constraint.get_unknown x.constraint_list)
        end
    ) state.func_type;

    (* 2. Range type infer *)
    let subtype_list = RangeSubtype.get_range_constraint block_subtype in
    let subtype_list =
      List.filter (
        fun (x: RangeSubtype.type_rel) -> not (List.is_empty x.subtype_list)
      ) subtype_list
    in
    let constraint_list = List.concat_map (fun (a_type: ArchType.t) -> a_type.constraint_list) state.func_type in

    (* If a range var of spill slot is overwritten and not used, then we can just mark it as empty. *)
    let useless_var_set = RangeUselessInfer.get_useless_range_vars (StackSpillInfo.is_spill state.stack_spill_info) block_subtype in
    let subtype_list = RangeSubtype.apply_empty_sol subtype_list useless_var_set in

    let subtype_list = Constraint.get_range_subset constraint_list |> RangeSubtype.get_read_constraint subtype_list in
    let subtype_list = RangeSubset.update_subtype_list_equal_set subtype_list in
    let subtype_list = RangeSubtype.get_equal_var_constraint subtype_list in
    (* let single_sol_repl_helper = SingleSubtype.substitute_one_exp_subtype_list state.single_sol in *)
    Printf.printf "block_subtype_label_list\n%s\n" 
      (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_block_subtype_label_t (ArchType.get_block_subtype_label block_subtype)));
    Printf.printf "================1\n";
    RangeSubtype.pp_range_subtype 0 subtype_list;
    (* let range_get_block_var (r: MemRange.t) : SingleEntryType.SingleVarSet.t =
      SingleEntryType.SingleVarSet.diff (MemRange.get_vars r) state.input_single_var_set
    in *)
    let subtype_list = 
      RangeSubtype.solve 
        state.smt_ctx 
        state.single_sol
        state.input_single_var_set
        (* (MemRange.is_val state.input_single_var_set)  *)
        (* range_get_block_var  *)
        block_subtype 
        subtype_list 100 
    in
    Printf.printf "Range subtype of func %s\n" state.func_name;
    RangeSubtype.pp_range_subtype 0 subtype_list;
    (* TODO: Write a function to check whether all solutions are found *)

    let unresolved_var_set = RangeSubtype.get_unresolved_var subtype_list in
    Printf.printf "Unresolved var of func %s\n%s\n" state.func_name (Sexplib.Sexp.to_string_hum (IntSet.sexp_of_t unresolved_var_set));
    if IntSet.cardinal unresolved_var_set = 0 then
      Printf.printf "Successfully resolve func %s\n" state.func_name;

    (* 3. Substitute solution back to func type *)
    let func_type =
      List.map (RangeSubtype.repl_sol_arch_type subtype_list useless_var_set) state.func_type
    in

    SmtEmitter.pop state.smt_ctx 1;
    { state with func_type = func_type }
    
  let infer 
      (single_infer_state_list: SingleTypeInfer.t list)
      (general_func_interface_list: FuncInterfaceConverter.TaintFuncInterface.t list) : t list =
    let helper
        (acc: FuncInterface.t list) (entry: SingleTypeInfer.t) :
        (FuncInterface.t list) * t =
      Printf.printf "Inferring func %s\n" entry.func_name;
      let infer_state = infer_one_func acc entry in
      let func_interface = get_func_interface infer_state in
      Printf.printf "Infer state of func %s\n" infer_state.func_name;
      pp_func_type 0 infer_state;
      (* FuncInterface.pp_func_interface 0 func_interface; *)
      func_interface :: acc, infer_state
    in
    let general_func_interface_list = FuncInterfaceConverter.get_single_func_interface general_func_interface_list in
    let _, infer_result = List.fold_left_map helper general_func_interface_list single_infer_state_list in
    (* Printf.printf "=========================\n";
    Sexp.output_hum stdout (sexp_of_list sexp_of_t infer_result); *)
    infer_result
    

end


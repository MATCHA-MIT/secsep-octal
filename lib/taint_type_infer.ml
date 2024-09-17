open Isa
open Taint_exp
open Single_subtype
open Taint_subtype
open Single_type_infer
open Smt_emitter

module TaintTypeInfer = struct
  exception TaintTypeInferError of string

  let taint_type_infer_error msg = raise (TaintTypeInferError ("[Taint Type Infer Error] " ^ msg))

  module TaintEntryType = TaintSubtype.TaintEntryType
  module ArchType = TaintSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    single_sol: SingleSubtype.t;
    taint_subtype: TaintSubtype.t;
    smt_ctx: SmtEmitter.t;
  }

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let init
      (prog: Isa.prog)
      (func_name: string)
      (func_single_type: SingleTypeInfer.ArchType.t list)
      (single_sol: SingleSubtype.t) : t =
    let start_var = TaintExp.TaintVar 1 in
    let helper 
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
        flag = (TaintEntryType.get_top_type, TaintEntryType.get_top_type);
        branch_hist = [];
        full_not_taken_hist = [];
        constraint_list = [];
        local_var_map = TaintEntryType.get_empty_var_map;
        useful_var = block_single_type.useful_var;
        global_var = block_single_type.global_var
      }
    in
    let _, func_type = List.fold_left_map helper start_var func_single_type in
    {
      func = (Isa.get_func prog func_name).body;
      func_type = func_type;
      single_sol = single_sol;
      taint_subtype = [];
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

end

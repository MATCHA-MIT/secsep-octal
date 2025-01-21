open Type.Isa
open Type.Set_sexp
open Type.Smt_emitter
(* open Z3_sexp *)
open Basic_type
open Mem_anno
open Stack_spill_info
open Reg_type
open Flag_type
open Mem_type
(* open Z3 *)
open Sexplib.Std

module ArchTypeBasic = struct
  exception ArchTypeError of string

  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  module Isa = Isa (MemAnno)

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = {
    label: Isa.label;
    pc: int;
    dead_pc: int;
    reg_type: RegType.t;
    flag_type: FlagType.t;
    mem_type: MemType.t;
    context: BasicType.ctx_t;

    stack_spill_info: StackSpillInfo.t;

    global_var: IntSet.t;
    input_var: IntSet.t;
    local_var: IntSet.t;
  }
  [@@deriving sexp]

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_a_type: t) (sup_a_type: t)
      (ctx_map: BasicType.map_t)
      (mem_map_opt: MemAnno.slot_t MemType.mem_content option) : bool =
    let ctx, _ = smt_ctx in
    (* 1. subsitute sup_a_type with ctx_map *)
    let dep_ctx_map, taint_ctx_map = ctx_map in
    let dep_sub_helper = DepType.substitute_exp_t ctx dep_ctx_map in
    let dep_exp_sub_func = DepType.substitute_exp_exp dep_sub_helper in
    let dep_sub_func = DepType.substitute dep_sub_helper in
    let taint_sub_func = TaintType.substitute ctx taint_ctx_map in
    let basic_sub_func = BasicType.substitute dep_sub_func taint_sub_func in

    let sup_reg_type = List.map basic_sub_func sup_a_type.reg_type in
    let sup_flag_type = List.map basic_sub_func sup_a_type.flag_type in
    (* Note: We do not substitute ptr since
       1. ptr is checked if slot offset checked
       2. ptr overlap/non-overlap is checked byu checking context *)
    let sup_mem_type =
      MemType.map_full (
        fun (off, range, entry) ->
          MemOffset.substitute dep_exp_sub_func off,
          MemRange.substitute dep_exp_sub_func range,
          basic_sub_func entry
      ) sup_a_type.mem_type
    in
    let sup_dep_context = List.map dep_exp_sub_func (fst sup_a_type.context) in
    let sup_taint_context = List.map taint_sub_func (snd sup_a_type.context) in

    (* 2. check subtype relation of each reg/flag/mem slot *)
    RegType.check_subtype smt_ctx sub_a_type.reg_type sup_reg_type &&
    FlagType.check_subtype smt_ctx sub_a_type.flag_type sup_flag_type &&
    MemType.check_subtype 
      smt_ctx 
      (StackSpillInfo.is_spill sub_a_type.stack_spill_info) 
      sub_a_type.mem_type sup_mem_type mem_map_opt &&
    SmtEmitter.check_compliance smt_ctx sup_dep_context = SatYes &&
    SmtEmitter.check_compliance smt_ctx sup_taint_context = SatYes

end

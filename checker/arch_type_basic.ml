open Type.Isa
open Type.Set_sexp
open Type.Smt_emitter
(* open Z3_sexp *)
open Basic_type
open Mem_anno
open Branch_anno
open Call_anno
open Stack_spill_info
open Reg_type
open Flag_type
open Mem_type
(* open Z3 *)
open Sexplib.Std

module ArchTypeBasic = struct
  exception ArchTypeError of string

  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  module Isa = Isa (MemAnno) (BranchAnno) (CallAnno)

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

  let arch_list_to_file (filename: string) (arch_list: t list) : unit =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (Std.sexp_of_list sexp_of_t arch_list)

  let arch_list_list_to_file (filename: string) (arch_list_list: t list list) : unit =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (Std.sexp_of_list (Std.sexp_of_list sexp_of_t) arch_list_list)

  let sub_ctx_map_helper
      (ctx: Z3.context)
      (a_type: t) (ctx_map: BasicType.map_t) : 
      RegType.t * FlagType.t * MemType.t * BasicType.ctx_t =
    let dep_ctx_map, taint_ctx_map = ctx_map in
    let dep_sub_helper = DepType.substitute_exp_t ctx dep_ctx_map in
    let dep_exp_sub_func = DepType.substitute_exp_exp dep_sub_helper in
    let dep_sub_func = DepType.substitute dep_sub_helper in
    let taint_sub_func = TaintType.substitute ctx taint_ctx_map in
    let basic_sub_func = BasicType.substitute dep_sub_func taint_sub_func in

    let reg_type = List.map basic_sub_func a_type.reg_type in
    let flag_type = List.map basic_sub_func a_type.flag_type in
    (* Note: We do not substitute ptr since
        1. ptr is checked if slot offset checked
        2. ptr overlap/non-overlap is checked by checking context *)
    let mem_type =
      MemType.map_full (
        fun (off, range, entry) ->
          MemOffset.substitute dep_exp_sub_func off,
          MemRange.substitute dep_exp_sub_func range,
          basic_sub_func entry
      ) a_type.mem_type
    in
    let dep_context = List.map dep_exp_sub_func (fst a_type.context) in
    let taint_context = List.map taint_sub_func (snd a_type.context) in
    reg_type, flag_type, mem_type, (dep_context, taint_context)

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_a_type: t) (sup_a_type: t)
      (ctx_map: BasicType.map_t)
      (mem_map_opt: MemAnno.slot_t MemType.mem_content option) : bool =
    let ctx, _ = smt_ctx in
    (* 1. subsitute sup_a_type with ctx_map *)
    let sup_reg_type, sup_flag_type, sup_mem_type, (sup_dep_context, sup_taint_context) =
      sub_ctx_map_helper ctx sup_a_type ctx_map
    in
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

open Type.Isa
open Type.Set_sexp
open Type.Smt_emitter
(* open Z3_sexp *)
open Basic_type
open Mem_anno
open Branch_anno
open Call_anno
(* open Stack_spill_info *)
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

    (* stack_spill_info: StackSpillInfo.t; *)

    global_var: IntSet.t;
    input_var: IntSet.t;
    local_var: IntSet.t;
  }
  [@@deriving sexp]

  let sub_ctx_map_helper
      (smt_ctx: SmtEmitter.t)
      (a_type: t) (ctx_map: BasicType.map_t) : 
      RegType.t * FlagType.t * MemType.t * BasicType.ctx_t =
    let ctx, _ = smt_ctx in
    let dep_ctx_map, taint_ctx_map = ctx_map in
    (*
    Printf.printf "dep_ctx_map:\n%s\n" (DepType.sexp_of_map_t dep_ctx_map |> Sexplib.Sexp.to_string_hum);
    Printf.printf "taint_ctx_map:\n%s\n" (TaintType.sexp_of_map_t taint_ctx_map |> Sexplib.Sexp.to_string_hum);
    *)
    let dep_sub_helper = DepType.substitute_exp_t ctx dep_ctx_map in
    let dep_exp_sub_func = DepType.substitute_exp_exp dep_sub_helper in
    let dep_exp_sub_opt_func = DepType.substitute_exp_exp_opt dep_sub_helper in
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
        fun (off, slot_forget, range, entry) ->
          MemOffset.substitute dep_exp_sub_func off,
          slot_forget,
          MemRange.substitute smt_ctx dep_exp_sub_func range,
          basic_sub_func entry
      ) a_type.mem_type
    in
    let dep_context = List.filter_map dep_exp_sub_opt_func (fst a_type.context) in
    let taint_context = List.map taint_sub_func (snd a_type.context) in
    reg_type, flag_type, mem_type, (dep_context, taint_context)

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_a_type: t) (sup_a_type: t)
      (ctx_map: BasicType.map_t)
      (mem_map_opt: MemAnno.slot_t MemType.mem_content option) : bool =
    (* 1. subsitute sup_a_type with ctx_map *)
    let sup_reg_type, sup_flag_type, sup_mem_type, (sup_dep_context, sup_taint_context) =
      sub_ctx_map_helper smt_ctx sup_a_type ctx_map
    in
    
    (*
    Printf.printf "subbed types:\n";
    Printf.printf "reg sup:\n%s\nreg sub:\n%s\n"
      (RegType.sexp_of_t sup_reg_type |> Sexplib.Sexp.to_string_hum)
      (RegType.sexp_of_t sub_a_type.reg_type |> Sexplib.Sexp.to_string_hum);
    Printf.printf "flag sup:\n%s\nflag sub:\n%s\n"
      (FlagType.sexp_of_t sup_flag_type |> Sexplib.Sexp.to_string_hum)
      (FlagType.sexp_of_t sub_a_type.flag_type |> Sexplib.Sexp.to_string_hum);
    Printf.printf "mem sup:\n%s\nmem sub:\n%s\n"
      (MemType.sexp_of_t sup_mem_type |> Sexplib.Sexp.to_string_hum)
      (MemType.sexp_of_t sub_a_type.mem_type |> Sexplib.Sexp.to_string_hum);
    Printf.printf "ctx sup:\n%s\nctx sub:\n%s\n"
      (BasicType.sexp_of_ctx_t (sup_dep_context, sup_taint_context) |> Sexplib.Sexp.to_string_hum)
      (BasicType.sexp_of_ctx_t sub_a_type.context |> Sexplib.Sexp.to_string_hum);
    *)

    (* 2. check subtype relation of each reg/flag/mem slot *)
    let reg_check = RegType.check_subtype smt_ctx sub_a_type.reg_type sup_reg_type in
    let flag_check = FlagType.check_subtype smt_ctx sub_a_type.flag_type sup_flag_type in
    let mem_check = MemType.check_subtype 
      smt_ctx 
      sub_a_type.mem_type sup_mem_type mem_map_opt in
    let dep_ctx_check = SmtEmitter.check_compliance smt_ctx sup_dep_context = SatYes in
    if not dep_ctx_check then begin
      Printf.printf "dep ctx check failed, examining each sup's context entry\n";
      List.iter (fun e ->
        let result = SmtEmitter.check_compliance smt_ctx [e] in
        Printf.printf "examining %s\nresult:%s\n"
          (Z3.Expr.to_string e)
          (SmtEmitter.sexp_of_sat_result_t result |> Sexplib.Sexp.to_string_hum);
      ) sup_dep_context;
      Printf.printf "%s\n" (SmtEmitter.to_string smt_ctx);
      Printf.printf "orignal sup's context:\n%s\n" (sup_a_type.context |> BasicType.sexp_of_ctx_t |> Sexplib.Sexp.to_string_hum);
    end;
    let taint_ctx_check = SmtEmitter.check_compliance smt_ctx sup_taint_context = SatYes in

    Printf.printf "ArchType.check_subtype: reg:%b flag:%b mem:%b dep_ctx:%b taint_ctx:%b\n"
      reg_check flag_check mem_check dep_ctx_check taint_ctx_check;
    reg_check && flag_check && mem_check && dep_ctx_check && taint_ctx_check

end

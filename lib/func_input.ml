open Single_entry_type
open Taint_exp
open Constraint
open Isa_basic
open Reg_type_new
open Mem_type_new
open Smt_emitter
open Sexplib.Std

module FuncInputEntry = struct

  (* symbolic expression, taint info *)
  type t = SingleEntryType.t * (bool option)
  [@@deriving sexp]

  type ext_t = 
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)

  type flag_t = t * t

  type local_var_map_t = SingleEntryType.local_var_map_t
  [@@deriving sexp]

  let panic () = failwith "invalid for FuncInputEntry"

  let get_empty_var_map = SingleEntryType.get_empty_var_map

  let get_empty_var_map_from_init_single_var_map = SingleEntryType.get_empty_var_map_from_init_single_var_map

  let get_taint_var_map (_: local_var_map_t) : TaintExp.local_var_map_t option =
    panic ()

  let partial_read_val (e: t) : t =
    let se, taint = e in
    SingleEntryType.partial_read_val se, taint

  let partial_write_val (_: t) (_: t) : t =
    panic ()

  let get_single_exp (e: t) : SingleEntryType.t =
    let se, _ = e in
    SingleEntryType.get_single_exp se

  let get_taint_exp (e: t) : TaintExp.t option =
    match e with
    | _, None -> None (* caller should create TaintVar for each None returned *)
    | _, Some taint -> Some (TaintConst taint)

  let next_var (_: t) : t = panic ()
  let to_string (e: t) : string =
    sexp_of_t e |> Sexplib.Sexp.to_string_hum
  let to_ocaml_string (_ : t) : string = panic ()
  let empty_var_map_to_ocaml_string = panic ()
  let cmp (_: t) (_: t) : int = panic ()
  let read_val (_: int64) (_: int64) (_: t) : t = panic ()
  let mem_partial_read_val (_: t) : t = panic ()
  let mem_partial_write_val (_: t) (_: t) : t = panic ()
  let ext_val (_: ext_t) (_: int64) (_: int64) (_: t) : t = panic ()
  let get_eq_taint_constraint (_: t) (_: t) : Constraint.t list = panic ()
  let get_sub_taint_constraint (_: t) (_: t) : Constraint.t list = panic ()
  let get_untaint_constraint (_: t) : Constraint.t list = panic ()
  let get_overwritten_taint_constraint (_: t) : Constraint.t list = panic ()
  let get_must_known_taint_constraint (_: t) : Constraint.t list = panic ()
  let update_ld_taint_constraint (_: t) (_: TaintExp.t option) : Constraint.t list = panic ()
  let update_st_taint_constraint (_: t) (_: TaintExp.t option) : t * Constraint.t list = panic ()
  let exe_bop_inst (_: bool) (_: IsaBasic.bop) (_: t) (_: t) (_: flag_t) (_: bool) : t * flag_t = panic ()
  let exe_uop_inst (_: IsaBasic.uop) (_: t) (_: flag_t) : t * flag_t = panic ()
  let exe_top_inst (_: IsaBasic.top) (_: t list) (_: flag_t) : t * flag_t = panic ()
  let get_single_taint_exp (_: t) : (SingleEntryType.t * TaintExp.t) = panic ()
  let set_taint_with_other (_: t) (_: t) : t = panic ()
  let get_single_var_map (_: local_var_map_t) : SingleEntryType.local_var_map_t = panic ()
  let get_const_type (_: IsaBasic.immediate) : t = panic ()
  let get_top_type () : t = panic ()
  let get_top_untaint_type () : t = panic ()
  let get_top_taint_type () : t = panic ()
  let get_unknown_taint_type () : t = panic ()
  let get_mem_op_type (_: IsaBasic.immediate option) (_: t option) (_: t option) (_: int64) : t = panic ()
  let update_local_var (_: local_var_map_t) (_: t) (_: int) : (local_var_map_t * t) = panic ()
  let add_local_var (_: local_var_map_t) (_: t) (_: t) : local_var_map_t = panic ()
  let add_local_global_var (_: local_var_map_t) (_: SingleEntryType.SingleVarSet.t) : local_var_map_t = panic ()
  let pp_local_var (_: int) (_: local_var_map_t) : unit = panic ()
  let add_context_map (_: bool) (_: SingleEntryType.t -> SingleEntryType.t) (_: local_var_map_t) (_: t) (_: t) : local_var_map_t = panic ()
  let repl_local_var (_: local_var_map_t) (_: t) : t = panic ()
  let repl_context_var (_: local_var_map_t) (_: t) : t = panic ()
  let is_val2 (_: local_var_map_t) (_: t) : bool = panic ()
  let to_smt_expr (_: SmtEmitter.t) (_: t) : SmtEmitter.exp_t = panic ()
  let split_val (_: t) (_: (SingleEntryType.t * SingleEntryType.t) list) : t list = panic ()

end

module FuncInput = struct

  module RegType = RegType(FuncInputEntry)
  module MemType = MemType(FuncInputEntry)

  type t = {
    func_name: IsaBasic.label;
    reg_type: RegType.t;
    mem_type: MemType.t;
  }
  [@@deriving sexp]

end

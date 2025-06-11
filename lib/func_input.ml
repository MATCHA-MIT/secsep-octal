open Single_entry_type
open Taint_exp
open Constraint
open Isa_basic
open Isa
open Ptr_info
open Reg_type_new
open Mem_type_new
open Smt_emitter
open Stack_spill_info
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
  let empty_var_map_to_ocaml_string = ""
  let cmp (_: t) (_: t) : int = panic ()
  let read_val (_: int64 option) (_: int64) (_: int64) (_: t) : t = panic ()
  let write_gpr_partial (_: int64) (_: int64) (_: t) (_: t) : t = panic ()
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
  let exe_bop_inst (_: IsaBasic.bop) (_: t) (_: t) (_: flag_t) (_: bool) : t * flag_t = panic ()
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
  let to_smt_expr ?(get_var_size: (int -> int option) option = None) (_: SmtEmitter.t) (_: t) : SmtEmitter.exp_t =
    let _ = get_var_size in panic ()
  let split_val (_: t) (_: (SingleEntryType.t * SingleEntryType.t) list) : t list = panic ()

end

module GlobalSymbolLayout = struct
  exception GlobalSymbolLayout of string
  let global_symbol_layout_error msg = raise (GlobalSymbolLayout ("[Global Symbol Layout Error] " ^ msg))

  type symbol_layout_t = IsaBasic.label * ((FuncInputEntry.t MemTypeBasic.mem_slot) list)
  [@@deriving sexp]

  type t = symbol_layout_t list
  [@@deriving sexp]

  let get_func_global_mem
      (all_global: t) 
      (imm_var_map: IsaBasic.imm_var_map) 
      (used_symbols: IsaBasic.label list) : FuncInputEntry.t MemTypeBasic.mem_content =
    let helper (symbol: IsaBasic.label) : FuncInputEntry.t MemTypeBasic.mem_part =
      let var_id = IsaBasic.StrM.find symbol imm_var_map in
      let layout_opt =
        List.find_map (
          fun (label, layout) ->
            if label = symbol then
              Some (
                (* If symbol start with "."*)
                PtrInfo.get_read_write_info var_id true (not (IsaBasic.is_compiler_gen_rom symbol)),
                layout
              )
            else None
        ) all_global
      in
      match layout_opt with
      | None -> global_symbol_layout_error (Printf.sprintf "cannot find layout of global symbol %s\n" symbol)
      | Some layout -> layout
    in
    List.map helper used_symbols

end

module FuncInput = struct

  module RegType = RegType(FuncInputEntry)
  module MemType = MemType(FuncInputEntry)
  module Isa = Isa (Full_mem_anno.FullMemAnno) (Branch_anno.BranchAnno) (Call_anno.CallAnno)

  type entry_t = {
    func_name: IsaBasic.label;
    reg_type: RegType.t;
    mem_type: MemType.t;
    stack_spill_info: StackSpillInfo.t;
  }
  [@@deriving sexp]

  type t = (entry_t list) * GlobalSymbolLayout.t
  [@@deriving sexp]

  type single_infer_interface_t = IsaBasic.label

  let parse (source: string) : t =
    t_of_sexp (Sexplib.Sexp.of_string source)

  let update_global_symbol_layout 
      (prog: Isa.prog) (func_input: t) : entry_t list =
    let func_interface_list, all_global = func_input in
    let helper (func_interface: entry_t) : entry_t = 
      let func_opt = List.find_opt (fun (x: Isa.func) -> x.name = func_interface.func_name) prog.funcs in
      match func_opt with
      | None -> func_interface
      | Some func ->
        { func_interface with 
          mem_type = func_interface.mem_type @ 
            (GlobalSymbolLayout.get_func_global_mem all_global prog.imm_var_map func.related_gsyms)  
        }
    in
    List.map helper func_interface_list

end

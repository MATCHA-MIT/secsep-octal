open Type.Isa_basic
open Type.Smt_emitter
open Basic_type
open Z3
open Sexplib.Std

module RegType = struct
  exception RegTypeError of string

  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let get_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) : entry_t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let entry_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    BasicType.get_start_len ctx off size entry_type

  let set_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) (new_type: entry_t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let old_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    let new_type = 
      BasicType.set_start_len ctx (IsaBasic.is_partial_update_reg_set_default_zero r) off size old_type new_type
    in
    List.mapi (
      fun i entry_type -> if i = reg_idx then new_type else entry_type
    ) reg_type

  let set_reg_mult_type 
      (ctx: context) (reg_type: t) 
      (r_list: IsaBasic.register list) (new_type: entry_t) : t =
    let helper
        (acc: int64 * t) (r: IsaBasic.register) : int64 * t =
      let acc_off, acc_reg_type = acc in
      let reg_size = IsaBasic.get_reg_size r in
      let acc_end = Int64.add acc_off reg_size in
      let write_type = BasicType.get_start_end ctx acc_off acc_end new_type in
      acc_end,
      set_reg_type ctx acc_reg_type r write_type
    in
    let acc_off, reg_type = List.fold_left helper (0L, reg_type) r_list in
    if DepType.get_bit_size (fst new_type) = (Int64.to_int acc_off) * 8 then
      reg_type
    else reg_type_error "set_reg_mult_type: new_type size and dest size unmatched"

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    List.fold_left2 (
      fun (acc: bool) (sub_entry: entry_t) (sup_entry: entry_t) ->
        acc &&
        BasicType.check_subtype smt_ctx false sub_entry sup_entry
    ) true sub_r_type sup_r_type

  let check_taint_eq
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    List.fold_left2 (
      fun (acc: bool) (_, sub_taint) (_, sup_taint) ->
        acc &&
        TaintType.check_subtype smt_ctx true  sub_taint sup_taint
    ) true sub_r_type sup_r_type

end

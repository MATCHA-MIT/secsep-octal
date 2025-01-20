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

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    List.fold_left2 (
      fun (acc: bool) (sub_entry: entry_t) (sup_entry: entry_t) ->
        acc &&
        BasicType.check_subtype smt_ctx false sub_entry sup_entry
    ) true sub_r_type sup_r_type

end

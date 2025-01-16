open Type.Isa_basic
open Basic_type
open Z3
open Sexplib.Std

module RegType = struct
  exception RegTypeError of string

  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type t = BasicType.t list
  [@@deriving sexp]

  let get_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) : BasicType.t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let entry_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    BasicType.get_start_len ctx off size entry_type

  let set_reg_type (ctx: context) (reg_type: t) (r: IsaBasic.register) (new_type: BasicType.t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    let old_type = List.nth reg_type reg_idx in
    let off, size = IsaBasic.get_reg_offset_size r in
    let new_type = 
      BasicType.set_start_len ctx (IsaBasic.is_partial_update_reg_set_default_zero r) off size old_type new_type
    in
    List.mapi (
      fun i entry_type -> if i = reg_idx then new_type else entry_type
    ) reg_type

end

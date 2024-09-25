open Isa_basic
open Type_exp
open Taint_dep_exp

module type RegEntryType = sig
  type t
  val next_var : t -> t
end

(* module type RegType = sig
  exception RegTypeError of string
  val reg_type_error : string -> 'a
  type entry_t
  type t
  val get_reg_type : t -> IsaBasic.register -> entry_t
  val set_reg_type : t -> IsaBasic.register -> entry_t -> t
  val init_reg_type : entry_t -> entry_t * t
end *)

module RegType (Entry: RegEntryType) = struct
  exception RegTypeError of string
  let reg_type_error msg = raise (RegTypeError ("[Reg Type Error] " ^ msg))

  type entry_t = Entry.t
  type t = entry_t list

  let get_reg_type (reg_type: t) (r: IsaBasic.register) : entry_t =
    let reg_idx = IsaBasic.get_reg_idx r in
    List.nth reg_type reg_idx

  let set_reg_idx_type (reg_type: t) (reg_idx: int) (new_type: entry_t) : t =
    List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

  let set_reg_type (reg_type: t) (r: IsaBasic.register) (new_type: entry_t) : t =
    let reg_idx = IsaBasic.get_reg_idx r in
    List.mapi (fun idx r -> if idx = reg_idx then new_type else r) reg_type

  let init_reg_type (start_var: entry_t) : entry_t * t =
    let rec helper (var: entry_t) (r_type: t) (idx: int) : entry_t * t =
      if idx = IsaBasic.total_reg_num then (var, r_type)
      else helper (Entry.next_var var) (var :: r_type) (idx + 1)
    in
    let next_var, reg_type = helper start_var [] 0 in
    (next_var, List.rev reg_type)
end

module RegRangeType = RegType (TypeExp)

module RegTaintType = RegType (TaintExp)

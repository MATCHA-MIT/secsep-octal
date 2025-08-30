open Type.Isa_basic
open Type.Smt_emitter
open Basic_type
open Reg_type_basic
open Sexplib.Std

module FlagType = struct
include RegTypeBasic
  exception FlagTypeError of string

  let flag_type_error msg = raise (FlagTypeError ("[Flag Type Error] " ^ msg))

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = (bool, entry_t) reg_content
  [@@deriving sexp]

  (* NOTE: we may consider to remove BasicType.get_flag and BasicType.set_flag *)
  let get_flag_idx_type (f_type: t) ?(check_valid=true) (flag_idx: int) : entry_t =
    if IsaBasic.is_valid_flag_idx flag_idx then
      let flag_valid, flag = List.nth f_type flag_idx in
      if not check_valid || flag_valid then flag
      else flag_type_error "get_flag_idx_type invalid flag"
    else flag_type_error "get_flag_idx_type invalid flag_idx"
    |> BasicType.get_flag

  let get_flag_type (f_type: t) ?(check_valid=true) (flag: IsaBasic.flag) : entry_t =
    get_flag_idx_type f_type ~check_valid:check_valid (IsaBasic.get_flag_idx flag)

  let set_one_flag_type 
      (f_type: t) (flag_type: IsaBasic.flag * entry_t) 
      (flag_config: IsaBasic.flag * bool) : t =
    let flag, new_type = flag_type in
    let flag_c, need_old_val = flag_config in
    if flag <> flag_c then flag_type_error "flag update list and flag config mismatch" else
    let flag_idx = IsaBasic.get_flag_idx flag in
    List.mapi (
      fun i (valid, entry) ->
        if i = flag_idx then
          (* if not need old val, then the flag is valid after update 
            if need old val, while the flag was not originally valid, 
              then we still mark it as invalid after update *) 
          (not need_old_val || valid), BasicType.set_flag entry new_type 
        else (valid, entry)
    ) f_type
    
  let set_flag_list_type 
      (f_type: t) (flag_type_list: (IsaBasic.flag * entry_t) list)
      (flag_update_config: (IsaBasic.flag * bool) list) : t =
    List.fold_left2 set_one_flag_type f_type flag_type_list flag_update_config

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (sub_r_type: t) (sup_r_type: t) : bool =
    List.fold_left2 (
      fun (acc: bool) (sub_valid, sub_entry) (sup_valid, sup_entry) ->
        acc &&
        (not sup_valid || sub_valid) &&
        BasicType.check_subtype smt_ctx false sub_entry sup_entry
    ) true sub_r_type sup_r_type

end

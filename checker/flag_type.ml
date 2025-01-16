open Type.Isa_basic
open Basic_type
open Sexplib.Std

module FlagType = struct
  exception FlagTypeError of string

  let flag_type_error msg = raise (FlagTypeError ("[Flag Type Error] " ^ msg))

  type t = BasicType.t list
  [@@deriving sexp]

  (* NOTE: we may consider to remove BasicType.get_flag and BasicType.set_flag *)
  let get_flag_idx_type (f_type: t) (flag_idx: int) : BasicType.t =
    if IsaBasic.is_valid_flag_idx flag_idx then
      List.nth f_type flag_idx
    else flag_type_error "get_flag_idx_type invalid flag_idx"
    |> BasicType.get_flag

  let get_flag_type (f_type: t) (flag: IsaBasic.flag) : BasicType.t =
    get_flag_idx_type f_type (IsaBasic.get_flag_idx flag)

  let set_one_flag_type (f_type: t) (flag_type: IsaBasic.flag * BasicType.t) : t =
    let flag, new_type = flag_type in
    let flag_idx = IsaBasic.get_flag_idx flag in
    List.mapi (
      fun i entry ->
        if i = flag_idx then BasicType.set_flag entry new_type else entry
    ) f_type
    
  let set_flag_list_type (f_type: t) (flag_type_list: (IsaBasic.flag * BasicType.t) list) : t =
    List.fold_left set_one_flag_type f_type flag_type_list

end

open Isa_basic
open Set_sexp
open Sexplib.Std

module PtrInfo = struct
  exception PtrInfoError of string

  let ptr_info_error msg = raise (PtrInfoError ("[Ptr Info Error] " ^ msg))

  (* Might overlap ptr set, can read, can write *)
  type t = IsaBasic.imm_var_id * (IntSet.t * bool * bool)
  [@@deriving sexp]

  let get_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, (IntSet.empty, true, true)

  let get_const_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, (IntSet.empty, true, false)

  let get_read_write_info (ptr: IsaBasic.imm_var_id) (read: bool) (write: bool) : t = ptr, (IntSet.empty, read, write)

  let get_input_info (ptr: IsaBasic.imm_var_id) (overlap_list: IsaBasic.imm_var_id list) (write: bool) : t =
    ptr, (IntSet.of_list overlap_list, true, write)

end

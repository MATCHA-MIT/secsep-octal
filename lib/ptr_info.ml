open Isa_basic
open Set_sexp
open Sexplib.Std

module PtrInfo = struct
  exception PtrInfoError of string

  let ptr_info_error msg = raise (PtrInfoError ("[Ptr Info Error] " ^ msg))

  (* Might overlap ptr set, can read, can write *)
  type info_t = IntSet.t * bool * bool
  [@@deriving sexp]

  type t = IsaBasic.imm_var_id * info_t
  [@@deriving sexp]

  let get_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, (IntSet.empty, true, true)

  let get_const_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, (IntSet.empty, true, false)

  let get_read_write_info (ptr: IsaBasic.imm_var_id) (read: bool) (write: bool) : t = ptr, (IntSet.empty, read, write)

  let get_input_info (ptr: IsaBasic.imm_var_id) (overlap_list: IsaBasic.imm_var_id list) (write: bool) : t =
    ptr, (IntSet.of_list overlap_list, true, write)

  let invalidate_on_write (ptr: IsaBasic.imm_var_id) (info: t) : t =
    (* ptr is the pointer that we write to, and we invalidate info if ptr might be overlapped with it *)
    (* Printf.printf "invalidate_on_write: %d\n%s\n" ptr (Sexplib.Sexp.to_string_hum (sexp_of_t info)); *)
    let info_ptr, (info_set, info_read, info_write) = info in
    if ptr = info_ptr then info
    else if not info_read then info
    else if IntSet.mem ptr info_set then
      info_ptr, (info_set, false, info_write)
    else
      info

  let invalidate (invalid_set: IntSet.t) (info: t) : t =
    let info_ptr, (info_set, _, info_write) = info in
    if IntSet.mem info_ptr invalid_set then
      info_ptr, (info_set, false, info_write)
    else info

  let can_read_info (info: info_t) : bool =
    let _, read, _ = info in read
  
  let can_read (info: t) : bool =
    can_read_info (snd info)

  let can_write_info (info: info_t) : bool =
    let _, read, write = info in read && write

  let can_write (info: t) : bool =
    can_write_info (snd info)

end

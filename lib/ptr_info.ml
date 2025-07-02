open Isa_basic
open Set_sexp
open Sexplib.Std

module PtrInfo = struct
  exception PtrInfoError of string

  let ptr_info_error msg = raise (PtrInfoError ("[Ptr Info Error] " ^ msg))

  module OverlapInfo = struct
    type t =
      | All
      | Slots of IntSet.t
    [@@deriving sexp]
  end

  module InfoMap = IntMapSexp (OverlapInfo)

  (* Might overlap ptr set, can read, can write *)
  type one_info_t = InfoMap.t * bool * bool
  [@@deriving sexp]

  type info_t =
  | AllInfo of one_info_t
  | SlotsInfo of one_info_t list
  [@@deriving sexp]

  type t = IsaBasic.imm_var_id * info_t
  [@@deriving sexp]

  let get_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, AllInfo (InfoMap.empty, true, true)

  let get_const_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, AllInfo (InfoMap.empty, true, false)

  let get_read_write_info (ptr: IsaBasic.imm_var_id) (read: bool) (write: bool) : t = ptr, AllInfo (InfoMap.empty, read, write)

  let get_input_info (ptr: IsaBasic.imm_var_id) (overlap_list: IsaBasic.imm_var_id list) (write: bool) : t =
    ptr, AllInfo (List.map (fun x -> x, OverlapInfo.All) overlap_list |> InfoMap.of_list, true, write)

  let invalidate_on_write (ptr: IsaBasic.imm_var_id) (off_idx: int) (info: t) : t =
    (* ptr is the pointer that we write to, and we invalidate info if ptr might be overlapped with it *)
    (* Printf.printf "invalidate_on_write: %d\n%s\n" ptr (Sexplib.Sexp.to_string_hum (sexp_of_t info)); *)
    let invalidate_one_info (one_info: one_info_t) : one_info_t =
      let info_map, info_read, info_write = one_info in
      if not info_read then one_info
      else
        match InfoMap.find_opt ptr info_map with
        | Some All -> info_map, false, info_write
        | Some (Slots overlap_slot_idx_set) ->
          if IntSet.mem off_idx overlap_slot_idx_set then info_map, false, info_write
          else one_info
        | None -> one_info
    in
    let info_ptr, info_info = info in 
    match info_info with
    | AllInfo one_info -> info_ptr, AllInfo (invalidate_one_info one_info)
    | SlotsInfo one_info_list -> info_ptr, SlotsInfo (List.map invalidate_one_info one_info_list)
    (* let info_ptr, (info_set, info_read, info_write) = info in
    if ptr = info_ptr then info
    else if not info_read then info
    else if IntSet.mem ptr info_set then
      info_ptr, (info_set, false, info_write)
    else
      info *)

  let invalidate (invalid_map: IntSet.t IntMap.t) (info: t) : t =
    let invalidate_one_info (one_info: one_info_t) : one_info_t =
      let info_map, _, info_write = one_info in
      info_map, false, info_write
    in
    let info_ptr, info_info = info in
    match IntMap.find_opt info_ptr invalid_map with
    | Some invalid_off_list ->
      begin match info_info with
      | AllInfo one_info -> info_ptr, AllInfo (invalidate_one_info one_info)
      | SlotsInfo one_info_list ->
        info_ptr, SlotsInfo (
          List.mapi (
            fun i one_info ->
              if IntSet.mem i invalid_off_list then invalidate_one_info one_info
              else one_info
          ) one_info_list
        )
      end
    | None -> info

    (* let info_ptr, (info_set, _, info_write) = info in
    if IntSet.mem info_ptr invalid_set then
      info_ptr, (info_set, false, info_write)
    else info *)

  let can_read_info (off_idx: int) (info: info_t) : bool =
    match info with
    | AllInfo (_, read, _) -> read
    | SlotsInfo one_info_list ->
      let _, read, _ = List.nth one_info_list off_idx in read
    (* let _, read, _ = info in read *)
  
  let can_read (off_idx: int) (info: t) : bool =
    can_read_info off_idx (snd info)

  let can_write_info (off_idx: int) (info: info_t) : bool =
    match info with
    | AllInfo (_, read, write) -> read && write
    | SlotsInfo one_info_list ->
      let _, read, write = List.nth one_info_list off_idx in read && write
    (* let _, read, write = info in read && write *)

  let can_write (off_idx: int) (info: t) : bool =
    can_write_info off_idx (snd info)

end

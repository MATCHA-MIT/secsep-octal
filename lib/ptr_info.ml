open Isa_basic
open Set_sexp
open Sexplib.Std

module PtrInfo = struct
  exception PtrInfoError of string

  let ptr_info_error msg = raise (PtrInfoError ("[Ptr Info Error] " ^ msg))

  module OverlapInfo = struct
    type t =
    | All (* overlap with the entire range of target pointer *)
    | SlotSet of IntSet.t (* overlap with some slots of target pointer *)
    [@@deriving sexp]
  end

  module InfoMap = IntMapSexp (OverlapInfo)

  (* Might overlap ptr set, can read, can write *)
  type one_info_t = InfoMap.t * bool * bool
  [@@deriving sexp]

  type info_t =
  | Unified of one_info_t
  | Separate of one_info_t list
  [@@deriving sexp]

  type t = IsaBasic.imm_var_id * info_t
  [@@deriving sexp]

  let get_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, Unified (InfoMap.empty, true, true)

  let get_const_default_info (ptr: IsaBasic.imm_var_id) : t = ptr, Unified (InfoMap.empty, true, false)

  let get_read_write_info (ptr: IsaBasic.imm_var_id) (read: bool) (write: bool) : t = ptr, Unified (InfoMap.empty, read, write)

  let get_input_info (ptr: IsaBasic.imm_var_id) (overlap_list: IsaBasic.imm_var_id list) (write: bool) : t =
    ptr, Unified (List.map (fun x -> x, OverlapInfo.All) overlap_list |> InfoMap.of_list, true, write)

  let invalid_one_write_helper
      (ptr: IsaBasic.imm_var_id)
      (* ptr is the pointer that we write to, and we invalidate info if write range under ptr might be overlapped with it *)
      (ptr_write_overlap: IntSet.t -> bool) 
      (* for the current write, take set of ptr's off idx that info might be overlapped with ptr's slots, 
        return with the write will write to these overlap slots, i.e., whether info's slots hould be invalidated on this write *)
      (info: t) =
    let invalidate_one_info (one_info: one_info_t) : one_info_t =
      let info_map, info_read, info_write = one_info in
      if not info_read then one_info
      else
        match InfoMap.find_opt ptr info_map with
        | Some All -> info_map, false, info_write
        | Some (SlotSet overlap_slot_idx_set) ->
          if ptr_write_overlap overlap_slot_idx_set then info_map, false, info_write
          else one_info
        | None -> one_info
    in
    let info_ptr, info_info = info in 
    match info_info with
    | Unified one_info -> info_ptr, Unified (invalidate_one_info one_info)
    | Separate one_info_list -> info_ptr, Separate (List.map invalidate_one_info one_info_list)

  let invalid_one_write_slot (ptr: IsaBasic.imm_var_id) (slot_idx: int) (info: t) : t =
    let ptr_write_overlap (overlap_set: IntSet.t) : bool =
      IntSet.mem slot_idx overlap_set
    in
    invalid_one_write_helper ptr ptr_write_overlap info

  let invalidate_on_write_slot_range (ptr: IsaBasic.imm_var_id) (slot_idx: int) (slot_num: int) (info: t) : t =
    let ptr_write_overlap (overlap_set: IntSet.t) : bool =
      IntSet.exists (fun i -> i >= slot_idx && i < slot_idx + slot_num) overlap_set
    in
    invalid_one_write_helper ptr ptr_write_overlap info

  let invalidate_on_write_slot_set (ptr: IsaBasic.imm_var_id) (slot_idx_set: IntSet.t) (info: t) : t =
    let ptr_write_overlap (overlap_set: IntSet.t) : bool =
      IntSet.inter slot_idx_set overlap_set |> IntSet.is_empty |> not
    in
    invalid_one_write_helper ptr ptr_write_overlap info

  let invalidate (invalid_map: (IntSet.t option) IntMap.t) (info: t) : t =
    let invalidate_one_info (one_info: one_info_t) : one_info_t =
      let info_map, _, info_write = one_info in
      info_map, false, info_write
    in
    let info_ptr, info_info = info in
    match IntMap.find_opt info_ptr invalid_map, info_info with
    | None, _ -> info
    | Some (None), Unified one_info -> 
      (* Unified -> invalid_map[info_ptr] = None -> all slots are uniformly invalidated *)
      info_ptr, Unified (invalidate_one_info one_info)
    | Some (Some invalid_off_list), Separate one_info_list ->
      (* Separate -> invalid_map[info_ptr] = set of slot idx to be invalidated *)
      info_ptr, 
      Separate (
        List.mapi (
          fun i one_info ->
            if IntSet.mem i invalid_off_list then invalidate_one_info one_info
            else one_info
        ) one_info_list
      )
    | _ -> ptr_info_error "unexpected cases"
    (* | Some invalid_off_list ->
      begin match info_info with
      | Unified one_info -> info_ptr, Unified (invalidate_one_info one_info)
      | Separate one_info_list ->
        info_ptr, Separate (
          List.mapi (
            fun i one_info ->
              if IntSet.mem i invalid_off_list then invalidate_one_info one_info
              else one_info
          ) one_info_list
        )
      end
    | None -> info *)

    (* let info_ptr, (info_set, _, info_write) = info in
    if IntSet.mem info_ptr invalid_set then
      info_ptr, (info_set, false, info_write)
    else info *)

  let check_read_permission (info: one_info_t) : bool =
    let _, read, _ = info in read
  
  let check_write_permission (info: one_info_t) : bool =
    let _, read, write = info in read && write

  let check_slot_permission_info 
      (check_helper: one_info_t -> bool)
      (slot_idx: int) (info: info_t) : bool =
    match info with
    | Unified one_info -> check_helper one_info
    | Separate one_info_list ->
      List.nth one_info_list slot_idx |> check_helper

  let check_mult_slot_permission_info
      (check_helper: one_info_t -> bool)
      (slot_filter: int -> 'a -> bool)
      (slot_num: int)
      (info: info_t) : bool =
    match info with
    | Unified one_info -> check_helper one_info
    | Separate one_info_list ->
      let check_info_list = List.filteri slot_filter one_info_list in
      if List.length check_info_list <> slot_num then
        ptr_info_error "check_idx_set_permission_info cannot find all idx in idx set"
      else
        List.fold_left (
          fun acc one_info ->
            acc && check_helper one_info
        ) true check_info_list

  let check_slot_range_permission_info
      (check_helper: one_info_t -> bool)
      (slot_idx: int) (slot_num: int) (info: info_t) : bool =
    let slot_filter =
      fun i _ -> i >= slot_idx && i < slot_idx + slot_num
    in
    if slot_num = 0 then ptr_info_error "check_slot_range_permission_info slot_num = 0";
    check_mult_slot_permission_info check_helper slot_filter slot_num info

  let check_slot_set_permission_info
      (check_helper: one_info_t -> bool)
      (slot_idx_set: IntSet.t) (info: info_t) : bool =
    let slot_filter =
      fun i _ -> IntSet.mem i slot_idx_set
    in
    if IntSet.is_empty slot_idx_set then true else
    check_mult_slot_permission_info check_helper slot_filter (IntSet.cardinal slot_idx_set) info

  let check_slot_permission
      (check_helper: one_info_t -> bool)
      (slot_idx: int) (info: t) : bool =
    check_slot_permission_info check_helper slot_idx (snd info)

  let check_slot_range_permission
      (check_helper: one_info_t -> bool)
      (slot_idx: int) (slot_num: int) (info: t) : bool =
    check_slot_range_permission_info check_helper slot_idx slot_num (snd info)

  let check_slot_set_permission
      (check_helper: one_info_t -> bool)
      (slot_idx_set: IntSet.t) (info: t) : bool =
    check_slot_set_permission_info check_helper slot_idx_set (snd info)

  let can_read_info = check_slot_permission_info check_read_permission
    (* let _, read, _ = info in read *)
  
  let can_read_slot_range_info = check_slot_range_permission_info check_read_permission

  let can_read_slot_set_info = check_slot_set_permission_info check_read_permission

  let can_read = check_slot_permission check_read_permission

  let can_read_slot_range = check_slot_range_permission check_read_permission

  let can_read_slot_set = check_slot_set_permission check_read_permission

  let can_write_info = check_slot_permission_info check_write_permission
    (* let _, read, write = info in read && write *)

  let can_write_slot_range_info = check_slot_range_permission_info check_write_permission

  let can_write_slot_set_info = check_slot_set_permission_info check_write_permission

  let can_write = check_slot_permission check_write_permission

  let can_write_slot_range = check_slot_range_permission check_write_permission

  let can_write_slot_set = check_slot_set_permission check_write_permission

  let filter_permission_info
      (check_helper: one_info_t -> bool)
      (info: info_t) (mem_slots: 'a list) : 'a list =
    match info with
    | Unified one_info ->
      let permission = check_helper one_info in
      if permission then mem_slots
      else []
    | Separate one_info_list ->
      List.map2 (
        fun one_info x ->
          if check_helper one_info then Some x
          else None
      ) one_info_list mem_slots |> List.filter_map (fun x -> x)

  let filter_write_permission_info
      (info: info_t) (mem_slots: 'a list) : 'a list =
    filter_permission_info check_write_permission info mem_slots

  let filter_write_permission
      (info: t) (mem_slots: 'a list) : 'a list =
    filter_write_permission_info (snd info) mem_slots

  let assoc_permission_info
      (check_helper: one_info_t -> bool)
      (info: info_t) (mem_slots: 'a list) : (bool * 'a) list =
    match info with
    | Unified one_info ->
      let permission = check_helper one_info in
      List.map (fun x -> permission, x) mem_slots
    | Separate one_info_list ->
      List.map2 (fun one_info x -> check_helper one_info, x) one_info_list mem_slots

  let assoc_write_permission_info
      (info: info_t) (mem_slots: 'a list) : (bool * 'a) list =
    assoc_permission_info check_write_permission info mem_slots

  let check_subtype_info
      (skip_check_might_overlap_map: bool)
      (sub: info_t) (sup: info_t) : bool =
    let check_subtype_overlap_map
        (sub: InfoMap.t) (sup: InfoMap.t) : bool =
      InfoMap.equal (
        fun (o1: OverlapInfo.t) (o2: OverlapInfo.t) ->
          match o1, o2 with
          | All, All -> true
          | SlotSet s1, SlotSet s2 -> IntSet.equal s1 s2
          | _, _ -> false (* Here we choose an simplified impl which overconstrains the overlap info *)
      ) sub sup
    in
    let imply_helper (sub: bool) (sup: bool) : bool =
      (* sub => sup *)
      (not sub) || sup
    in
    let check_subtype_one_info
      (sub: one_info_t) (sup: one_info_t) : bool =
      let sub_might_overlap, sub_read, sub_write = sub in
      let sup_might_overlap, sup_read, sup_write = sup in
      (skip_check_might_overlap_map || check_subtype_overlap_map sub_might_overlap sup_might_overlap) &&
      imply_helper sup_read sub_read &&
      imply_helper sup_write sub_write 
    in
    match sub, sup with
    | Unified sub_info, Unified sup_info ->
      check_subtype_one_info sub_info sup_info
    | Unified sub_info, Separate sup_info_list ->
      List.fold_left (
        fun acc sup_info ->
          acc && (check_subtype_one_info sub_info sup_info)
      ) true sup_info_list
    | Separate sub_info_list, Unified sup_info ->
      List.fold_left (
        fun acc sub_info ->
          acc && (check_subtype_one_info sub_info sup_info)
      ) true sub_info_list
    | Separate sub_info_list, Separate sup_info_list ->
      List.fold_left2 (
        fun acc sub_info sup_info ->
          acc && (check_subtype_one_info sub_info sup_info)
      ) true sub_info_list sup_info_list

  let get_nth_slot_one_info (info: info_t) (idx: int) : one_info_t =
    match info with
    | Unified one_info -> one_info
    | Separate info_list -> List.nth info_list idx

  let group_slot_one_info (orig_info: info_t) (one_info_list: one_info_t list) : info_t =
    match orig_info with
    | Unified _ -> Unified (List.hd one_info_list)
    | Separate _ -> Separate one_info_list

end

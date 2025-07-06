open Set_sexp
open Isa_basic
open Ptr_info
open Entry_type
open Arch_type
open Sexplib.Std

module MemAlive (Entry: EntryType) = struct
  exception MemAliveError of string

  let mem_alive_error msg = raise (MemAliveError ("[Mem Alive] " ^ msg))

  module ArchType = ArchType (Entry)

  module DeadSlotInfo = struct
    exception DeadSlotInfoError of string
    
    let dead_slot_info_error msg = raise (DeadSlotInfoError ("[Dead Slot Info] " ^ msg))

    type t = IntSet.t option
    [@@deriving sexp]
    (* 
       Some int_set: dead slots for ptr with separate info
       None: if a ptr with unified info has dead slots, then all slots are dead and we do not need to record idx set
    *)

    let union (info1: t) (info2: t) : t =
      match info1, info2 with
      | None, None -> None
      | Some dead_set1, Some dead_set2 -> Some (IntSet.union dead_set1 dead_set2)
      | _ -> dead_slot_info_error "union two info does not match"

    let subset (info1: t) (info2: t) : bool =
      match info1, info2 with
      | None, None -> true
      | Some dead_set1, Some dead_set2 -> IntSet.subset dead_set1 dead_set2
      | _ -> dead_slot_info_error "subset two info does not match"

  end

  module DeadSlotInfoMap = struct
  include IntMapSexp (DeadSlotInfo)

    let dead_slot_union (map1: t) (map2: t) : t =
      union (fun _ info1 info2 -> Some (DeadSlotInfo.union info1 info2)) map1 map2

    let dead_slot_subset (map1: t) (map2: t) : bool =
      (* check whether map1 contains more dead slots than map2 *)
      let helper (ptr: int) (map1_dead_info: DeadSlotInfo.t) : bool =
        match find_opt ptr map2 with
        | None -> true
        | Some map2_dead_info -> not (DeadSlotInfo.subset map1_dead_info map2_dead_info)
      in
      not (exists helper map1)

  end

  type entry_t = IsaBasic.label * (DeadSlotInfoMap.t) (* block label, not alive ptr->slot set map *)
  [@@deriving sexp]

  type subtype_t = entry_t * (entry_t list)
  [@@deriving sexp]

  type t = subtype_t list

  let init (block_subtype_list: ArchType.block_subtype_t list) : t =
    let helper (a_type: ArchType.t) : entry_t = 
      a_type.label,
      List.fold_left (
        fun (acc: DeadSlotInfoMap.t) ((ptr, ptr_info), _) ->
          match ptr_info with
          | PtrInfo.Unified (_, read, _) ->
            if read then acc
            else DeadSlotInfoMap.add ptr None acc 
            (* for unified info we can simply use an empty set, it's a simple but dirty and confusing impl *)
          | Separate one_info_list ->
            let dead_idx_set =
              List.mapi (
                fun i (_, read, _) ->
                  if not read then Some i else None
              ) one_info_list |> List.filter_map (fun x -> x) |> IntSet.of_list
            in
            if IntSet.is_empty dead_idx_set then acc
            else DeadSlotInfoMap.add ptr (Some dead_idx_set) acc
          (* if not read then IntSet.add ptr acc
          else acc *)
      ) DeadSlotInfoMap.empty a_type.mem_type
    in
    List.map (
      fun (block_subtype: ArchType.block_subtype_t) ->
        let target, branch_list = block_subtype in
        helper target,
        List.map helper branch_list
    ) block_subtype_list

  let get_block_not_alive (mem_alive: t) (label: IsaBasic.label) : DeadSlotInfoMap.t =
    List.find_map (
      fun ((entry_label, entry_set), _) ->
        if label = entry_label then Some entry_set
        else None
    ) mem_alive |> Option.get

  let solve_one_entry (mem_alive: t) (find_new: bool) (block_subtype: subtype_t) : bool * subtype_t =
    let (block_label, orig_not_alive_set), sub_list = block_subtype in
    let new_not_alive_set =
      List.fold_left (
        fun (acc: DeadSlotInfoMap.t) (sub_block: entry_t) ->
          let sub_label, sub_not_alive_set = sub_block in
          DeadSlotInfoMap.dead_slot_union acc (DeadSlotInfoMap.dead_slot_union sub_not_alive_set (get_block_not_alive mem_alive sub_label))
      ) DeadSlotInfoMap.empty sub_list
    in
    if DeadSlotInfoMap.dead_slot_subset new_not_alive_set orig_not_alive_set then
      find_new, block_subtype
    else
      true, ((block_label, DeadSlotInfoMap.dead_slot_union orig_not_alive_set new_not_alive_set), sub_list)

  let rec solve_iter (mem_alive: t) : t =
    let find_new, mem_alive = List.fold_left_map (solve_one_entry mem_alive) false mem_alive in
    if find_new then
      solve_iter mem_alive
    else mem_alive

  let solve (block_subtype_list: ArchType.block_subtype_t list) (func_type: ArchType.t list) : ArchType.t list =
    let mem_alive = init block_subtype_list in
    let mem_alive = solve_iter mem_alive in
    List.map2 (
      fun (a_type: ArchType.t) (block_subtype: subtype_t) ->
        let (block_label, block_not_alive), _ = block_subtype in
        if a_type.label = block_label then
          let new_mem_type =
            List.map (
              fun (ptr_info, part_mem) ->
              PtrInfo.invalidate block_not_alive ptr_info,
              part_mem
            ) a_type.mem_type
          in
          { a_type with mem_type = new_mem_type }
        else
          mem_alive_error "Func_type label does not match"
    ) func_type mem_alive
  
end

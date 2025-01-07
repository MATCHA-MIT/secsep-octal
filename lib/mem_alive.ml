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

  type entry_t = IsaBasic.label * IntSet.t (* block label, not alive ptr set *)
  [@@deriving sexp]

  type subtype_t = entry_t * (entry_t list)
  [@@deriving sexp]

  type t = subtype_t list

  let init (block_subtype_list: ArchType.block_subtype_t list) : t =
    let helper (a_type: ArchType.t) : entry_t = 
      a_type.label,
      List.fold_left (
        fun (acc: IntSet.t) ((ptr, (_, read, _)), _) ->
          if not read then IntSet.add ptr acc
          else acc
      ) IntSet.empty a_type.mem_type
    in
    List.map (
      fun (block_subtype: ArchType.block_subtype_t) ->
        let target, branch_list = block_subtype in
        helper target,
        List.map helper branch_list
    ) block_subtype_list

  let get_block_not_alive (mem_alive: t) (label: IsaBasic.label) : IntSet.t =
    List.find_map (
      fun ((entry_label, entry_set), _) ->
        if label = entry_label then Some entry_set
        else None
    ) mem_alive |> Option.get

  let solve_one_entry (mem_alive: t) (find_new: bool) (block_subtype: subtype_t) : bool * subtype_t =
    let (block_label, orig_not_alive_set), sub_list = block_subtype in
    let new_not_alive_set =
      List.fold_left (
        fun (acc: IntSet.t) (sub_block: entry_t) ->
          let sub_label, sub_not_alive_set = sub_block in
          IntSet.union acc (IntSet.union sub_not_alive_set (get_block_not_alive mem_alive sub_label))
      ) IntSet.empty sub_list
    in
    if IntSet.subset new_not_alive_set orig_not_alive_set then
      find_new, block_subtype
    else
      true, ((block_label, IntSet.union orig_not_alive_set new_not_alive_set), sub_list)

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

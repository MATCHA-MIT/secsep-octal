open Set_sexp
open Isa_basic
open Entry_type
open Arch_type
open Sexplib.Std

module BlockAlive (Entry: EntryType) = struct
  exception BlockAliveError of string
  
  let block_alive_error msg = raise (BlockAliveError ("[Block Alive] " ^ msg))

  module ArchType = ArchType (Entry)

  type entry_t = IsaBasic.label * StringSet.t
  [@@deriving sexp]

  type t = entry_t list
  [@@deriving sexp]

  let init (block_subtype_list: ArchType.block_subtype_t list) : t =
    List.map (
      fun (block_subtype: ArchType.block_subtype_t) ->
        let target, branch_list = block_subtype in
        target.label,
        StringSet.of_list (List.map (fun (x: ArchType.t) -> x.label) branch_list)
    ) block_subtype_list

  let solve_one_iter (alive_block_set: StringSet.t) (control_flow_list: t) : StringSet.t =
    List.fold_left (
      fun (acc: StringSet.t) (entry: entry_t) ->
        let block_label, block_entry_set = entry in
        if StringSet.mem block_label acc then acc
        else if StringSet.is_empty (StringSet.inter acc block_entry_set) then acc
        else StringSet.add block_label acc
    ) alive_block_set control_flow_list

  let solve (func_name: IsaBasic.label) (block_subtype_list: ArchType.block_subtype_t list) : StringSet.t =
    let control_flow_list = init block_subtype_list in
    let init_set = StringSet.singleton func_name in
    let rec helper (alive_set: StringSet.t) =
      let new_set = solve_one_iter alive_set control_flow_list in
      if StringSet.cardinal new_set = StringSet.cardinal alive_set then new_set
      else helper new_set
    in
    helper init_set

end


open Arch_type
open Constraint
open Mem_offset_new
open Single_entry_type
open Set_sexp

module RangeUselessInfer = struct

  exception RangeUselessInferError of string

  module ArchType = ArchType (SingleEntryType)
  module VarSet = IntSet

  let range_useless_infer_error msg = raise (RangeUselessInferError ("[Range Useless Infer Error] " ^ msg))

  let get_all_spill_vars
      (is_spill_func: ArchType.Isa.imm_var_id -> MemOffset.t -> bool)
      (block_subtype_list: ArchType.block_subtype_t list)
      : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let block_type = fst block_subtype in
        ArchType.MemType.fold_left_ptr_full (
          fun acc ptr slot ->
            let ptr_var, _ = ptr in
            let off, range, _ = slot in
            if is_spill_func ptr_var off then (
              match MemRange.get_range_var range with
              | Some var -> VarSet.add var acc
              | None -> acc
            ) else acc
        ) acc block_type.mem_type
    ) VarSet.empty block_subtype_list

  let get_block_must_knowns
      (block_subtype_list: ArchType.block_subtype_t list)
      : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let block_type = fst block_subtype in
        let must_known_vars = Constraint.get_range_must_known block_type.constraint_list in
        VarSet.union acc must_known_vars
    ) VarSet.empty block_subtype_list
  
  let propagate_must_known
      (block_subtype_list: ArchType.block_subtype_t list)
      (must_known_vars: VarSet.t)
      : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let sup_block, sub_blocks = block_subtype in
        List.fold_left (
          fun (acc: VarSet.t) (sub_block: ArchType.t) ->
            ArchType.MemType.fold_left2_full (
              fun acc sup_slot sub_slot ->
              let _, sup_range, _ = sup_slot in
              let _, sub_range, _ = sub_slot in
              match sup_range, sub_range with
              | RangeVar sup_var, RangeVar sub_var
              | RangeVar sup_var, RangeExp (sub_var, _)
              | RangeExp (sup_var, _), RangeVar sub_var
              | RangeExp (sup_var, _), RangeExp (sub_var, _) ->
                (* sub_var --> sup_var, i.e., sup_var needs sub_var *)
                if VarSet.mem sup_var must_known_vars then
                  VarSet.add sub_var acc
                else
                  acc
              | _ -> acc
            ) acc sup_block.mem_type sub_block.mem_type
        ) acc sub_blocks
    ) must_known_vars block_subtype_list

  let get_useless_range_vars
      (is_spill_func: ArchType.Isa.imm_var_id -> MemOffset.t -> bool)
      (block_subtype_list: ArchType.block_subtype_t list)
      : VarSet.t =
    let all_vars_in_spills = get_all_spill_vars is_spill_func block_subtype_list in
    Printf.printf "range_useless_infer: all spill vars: %s\n" (IntSet.sexp_of_t all_vars_in_spills |> Sexplib.Sexp.to_string);

    (* Propagate must known variables until no more changes *)
    (* This is a fixed point iteration *)
    (* Start with the must known variables in the blocks *)

    let rec iter_prop (acc: VarSet.t) : VarSet.t =
      let len = VarSet.cardinal acc in
      let new_acc = propagate_must_known block_subtype_list acc in
      if VarSet.cardinal new_acc = len then
        new_acc
      else
        iter_prop new_acc
    in
    let must_known_vars = iter_prop (get_block_must_knowns block_subtype_list) in
    Printf.printf "range_useless_infer: must knowns: %s\n" (IntSet.sexp_of_t must_known_vars |> Sexplib.Sexp.to_string);

    let useless_vars = VarSet.diff all_vars_in_spills must_known_vars in
    Printf.printf "range_useless_infer: useless vars: %s\n" (IntSet.sexp_of_t useless_vars |> Sexplib.Sexp.to_string);
    useless_vars

end

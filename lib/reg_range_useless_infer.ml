open Isa_basic
open Arch_type
open Constraint
open Single_entry_type
open Reg_range
open Set_sexp

module RegRangeUselessInfer = struct
  exception RegRangeUselessInferError of string

  module ArchType= ArchType (SingleEntryType)
  module VarSet = IntSet

  let get_block_must_knowns
      (get_callee_at_call: bool)
      (init_set: IntSet.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let block_type = fst block_subtype in
        let must_known_vars = Constraint.get_reg_range_must_known block_type.constraint_list in
        let must_known_vars =
          if get_callee_at_call then IntSet.union must_known_vars (Constraint.get_reg_range_callee_at_call block_type.constraint_list)
          else must_known_vars
        in
        let must_known_vars =
          (* Handle rsp as a special case since it is silently updated and may not be tracked by constraints *)
          match List.nth block_type.reg_type IsaBasic.rsp_idx |> fst with
          | RangeVar v | RangeExp (v, _) -> IntSet.add v must_known_vars
          | _ -> must_known_vars
        in
        (* let callee_vars = 
          (* Need to handle callee reg (and rsp) specially since
            (1) even unused callee saved reg should be valid and
            (2) rsp is silently updated and may not be tracked by constraints  *)
          List.filteri (fun i _ -> IsaBasic.is_reg_idx_callee_saved i) block_type.reg_type
          |> List.filter_map (
            fun (valid, _) ->
              match valid with
              | RegRange.RangeVar v | RangeExp (v, _) -> Some v
              | _ -> None
          ) |> VarSet.of_list
        in *)
        VarSet.union acc must_known_vars
    ) init_set block_subtype_list

  let propagate_must_known
      (block_subtype_list: ArchType.block_subtype_t list)
      (must_known_vars: VarSet.t)
      : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let sup_block, sub_blocks = block_subtype in
        List.fold_left (
          fun (acc: VarSet.t) (sub_block: ArchType.t) ->
            List.fold_left2 (
              fun acc (sup_range, _) (sub_range, _) ->
              match sup_range, sub_range with
              | RegRange.RangeVar sup_var, RegRange.RangeVar sub_var
              | RangeVar sup_var, RangeExp (sub_var, _)
              | RangeExp (sup_var, _), RangeVar sub_var
              | RangeExp (sup_var, _), RangeExp (sub_var, _) ->
                (* sub_var --> sup_var, i.e., sup_var needs sub_var *)
                if VarSet.mem sup_var must_known_vars then
                  VarSet.add sub_var acc
                else
                  acc
              | _ -> acc
            ) acc sup_block.reg_type sub_block.reg_type
        ) acc sub_blocks
    ) must_known_vars block_subtype_list

    let get_useful_range_vars
        (get_callee_at_call: bool)
        (init_set: IntSet.t)
        (block_subtype_list: ArchType.block_subtype_t list)
        : VarSet.t =
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
      let must_known_vars = iter_prop (get_block_must_knowns get_callee_at_call init_set block_subtype_list) in
      Printf.printf "range_useless_infer: must knowns: %s\n" (IntSet.sexp_of_t must_known_vars |> Sexplib.Sexp.to_string);

      must_known_vars

end

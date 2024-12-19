open Taint_exp
open Taint_entry_type
open Constraint
open Arch_type
open Sexplib.Std

module TaintUnknownInfer = struct
  exception TaintUnknownInferError of string

  let taint_unknown_infer_error msg = raise (TaintUnknownInferError ("[Taint Uknown Infer Error] " ^ msg))

  module ArchType = ArchType (TaintEntryType)
  module MemType = ArchType.MemType
  module VarSet = TaintExp.TaintVarSet

  let get_unknown_unused_var_one_block (a_type: ArchType.t) : VarSet.t * VarSet.t =
    (* Unknown var set, unused var set *)
    let mem_var_set =
      List.fold_left (
        fun (acc: VarSet.t) (part_mem: TaintEntryType.t MemType.mem_part) ->
          let _, entry_list = part_mem in
          List.fold_left (
            fun (acc: VarSet.t) (entry: TaintEntryType.t MemType.mem_slot) ->
              let _, _, (_, taint) = entry in
              VarSet.union acc (TaintExp.get_var_set taint)
          ) acc entry_list
      ) VarSet.empty a_type.mem_type
    in
    let overwritten_var_set = Constraint.get_taint_overwritten a_type.constraint_list in
    let must_known_var_set, invalid_pc_list = Constraint.get_taint_must_known a_type.constraint_list in
    if List.is_empty invalid_pc_list then begin
      VarSet.diff (VarSet.inter mem_var_set overwritten_var_set) must_known_var_set,
      VarSet.diff mem_var_set must_known_var_set
    end else begin
      Printf.printf "get_direct_unknown_var find invalid pc list that uses unknown taint\n%s\n" 
        (Sexplib.Sexp.to_string (sexp_of_list sexp_of_int invalid_pc_list));
      taint_unknown_infer_error "get_direct_unknown_var get invalid pc"
    end

  let get_unknown_unused_var (block_subtype_list: ArchType.block_subtype_t list) : VarSet.t * VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t * VarSet.t) (block_subtype: ArchType.block_subtype_t) ->
        let acc_unknown_var_set, acc_unused_var_set = acc in
        let new_unknown_var_set, new_unused_var_set = get_unknown_unused_var_one_block (fst block_subtype) in
        VarSet.union acc_unknown_var_set new_unknown_var_set,
        VarSet.union acc_unused_var_set new_unused_var_set
    ) (VarSet.empty, VarSet.empty) block_subtype_list

  let update_unknown_one_pair
      (unused_var_set: VarSet.t) (sup_block: ArchType.t)
      (unknown_var_set: VarSet.t)
      (sub_block: ArchType.t) : VarSet.t =
    let helper_inner
        (acc_unknown_var_set: VarSet.t) 
        (sub_entry: TaintEntryType.t MemType.mem_slot)
        (sup_entry: TaintEntryType.t MemType.mem_slot) : VarSet.t =
      let _, _, (_, sub) = sub_entry in
      let _, _, (_, sup) = sup_entry in
      match sub, sup with
      | TaintVar sub_var, TaintVar sup_var ->
        if VarSet.mem sub_var unknown_var_set then
          if VarSet.mem sup_var unused_var_set then
            VarSet.add sup_var acc_unknown_var_set
          else 
            taint_unknown_infer_error (Printf.sprintf "Unknown var %d is used" sup_var)
        else if VarSet.mem sub_var unused_var_set && VarSet.mem sup_var acc_unknown_var_set then
          VarSet.add sub_var acc_unknown_var_set
        else acc_unknown_var_set
      | TaintVar sub_var, TaintUnknown -> VarSet.add sub_var acc_unknown_var_set
      | _ -> acc_unknown_var_set
    in
    let helper_outer
        (acc: VarSet.t)
        (sub_part_mem: TaintEntryType.t MemType.mem_part)
        (sup_part_mem: TaintEntryType.t MemType.mem_part) : VarSet.t =
      let _, sub_part = sub_part_mem in
      let _, sup_part = sup_part_mem in
      List.fold_left2 helper_inner acc sub_part sup_part
    in
    List.fold_left2 helper_outer unknown_var_set sub_block.mem_type sup_block.mem_type

  let update_unknown_one_iter
      (unused_var_set: VarSet.t) (unknown_var_set: VarSet.t)
      (block_subtype_list: ArchType.block_subtype_t list) : VarSet.t =
    List.fold_left (
      fun (acc: VarSet.t) (block_subtype: ArchType.block_subtype_t) : VarSet.t ->
        let sup, sub_list = block_subtype in
        List.fold_left (update_unknown_one_pair unused_var_set sup) acc sub_list
    ) unknown_var_set block_subtype_list

  let get_unknown_var_set
      (block_subtype_list: ArchType.block_subtype_t list) : VarSet.t =
    let init_unknown_var_set, unused_var_set = get_unknown_unused_var block_subtype_list in
    let rec helper (unknown_var_set: VarSet.t) : VarSet.t =
      let orig_size = VarSet.cardinal unknown_var_set in
      let unknown_var_set = update_unknown_one_iter unused_var_set unknown_var_set block_subtype_list in
      if orig_size = VarSet.cardinal unknown_var_set then
        unknown_var_set
      else helper unknown_var_set
    in
    helper init_unknown_var_set

end

open Single_entry_type
open Taint_exp
open Taint_entry_type
open Constraint
open Arch_type

module TaintSubtype = struct
  exception TaintSubtypeError of string

  let taint_subtype_error msg = raise (TaintSubtypeError ("[Taint Subtype Error] " ^ msg))

  (* type type_rel = {
    var_idx: TaintExp.taint_var_id;
    sol: TaintExp.t;
    subtype: TaintExp.t;
    suptype: TaintExp.t
  } *)

  type t = (TaintExp.taint_var_id * TaintExp.t) list

  module TaintEntryType = TaintEntryType (SingleEntryType)
  module ArchType = ArchType (TaintEntryType)

  type sub_t = TaintExp.t * TaintExp.t

  let get_one_block_subtype
      (subtype_list: sub_t list)
      (block_subtype: ArchType.block_subtype_t) : sub_t list =
    let sup_block, sub_block_list = block_subtype in
    let subtype_list = Constraint.get_taint_sub sup_block.constraint_list @ subtype_list in
    let helper_entry 
        (subtype_list: sub_t list) 
        (sub_entry: TaintEntryType.t) (sup_entry: TaintEntryType.t) : sub_t list =
      let _, sub_taint = sub_entry in
      let _, sup_taint = sup_entry in
      (sub_taint, sup_taint) :: (sup_taint, sub_taint) :: subtype_list
    in
    let helper
        (sup_block: ArchType.t)
        (subtype_list: sub_t list)
        (sub_block: ArchType.t) : sub_t list =
      let subtype_list = List.fold_left2 helper_entry subtype_list sub_block.reg_type sup_block.reg_type in
      ArchType.MemType.fold_left2 helper_entry subtype_list sub_block.mem_type sup_block.mem_type
    in
    List.fold_left (helper sup_block) subtype_list sub_block_list    


  let get_taint_constraint 
      (block_subtype_list: ArchType.block_subtype_t list) : 
      sub_t list =
    List.fold_left get_one_block_subtype [] block_subtype_list

  let solve_subtype_list (subtype_list: sub_t list) : t =
    let add_untaint (* for sub -> false *)
        (untaint_var: TaintExp.TaintVarSet.t)
        (sub: TaintExp.t) : TaintExp.TaintVarSet.t =
      match sub with
      | TaintConst false -> untaint_var
      | TaintConst true -> taint_subtype_error "add_untaint find unsat constraint ture -> false"
      | TaintVar v -> TaintExp.TaintVarSet.add v untaint_var
      | TaintExp e -> TaintExp.TaintVarSet.union e untaint_var
    in
    let remove_untaint (* replace untaint var with false in sub of sub -> sup, if sub = false, return None *)
        (untaint_var: TaintExp.TaintVarSet.t)
        (subtype: sub_t) : sub_t option =
      let sub, _ = subtype in
      let sub = TaintExp.repl_untaint untaint_var sub in
      match sub with
      | TaintConst false -> None
      | _ -> Some subtype
    in
    let filter_untaint_helper
        (untaint_var: TaintExp.TaintVarSet.t)
        (subtype: sub_t) :
        TaintExp.TaintVarSet.t * (sub_t option) =
      let sub, sup = subtype in
      match TaintExp.repl_untaint untaint_var sup with
      | TaintConst false -> 
        add_untaint untaint_var sub, None
      | TaintConst true -> untaint_var, None (* This subtype relation implies nothing, so remove *)
      | _ ->
        untaint_var, remove_untaint untaint_var subtype
    in
    let add_taint (* for true -> sup *)
        (taint_var: TaintExp.TaintVarSet.t)
        (sup: TaintExp.t) : TaintExp.TaintVarSet.t * (sub_t option) =
      match sup with
      | TaintConst false -> taint_subtype_error "add_taint find unsat constraint true -> false"
      | TaintConst true -> taint_var, None
      | TaintVar v -> TaintExp.TaintVarSet.add v taint_var, None
      | _ -> taint_var, Some (TaintConst true, sup)
    in
    let filter_taint_helper
        (taint_var: TaintExp.TaintVarSet.t)
        (subtype: sub_t) :
        TaintExp.TaintVarSet.t * (sub_t option) =
      let sub, sup = subtype in
      let new_sup = TaintExp.repl_taint taint_var sup in
      match TaintExp.repl_taint taint_var sub with
      | TaintConst false -> taint_var, None
      | TaintConst true -> add_taint taint_var new_sup
      | new_sub -> taint_var, Some (new_sub, new_sup)
    in
    let rec filter
        (filter_helper: TaintExp.TaintVarSet.t -> sub_t -> TaintExp.TaintVarSet.t * (sub_t option))
        (var_set: TaintExp.TaintVarSet.t)
        (subtype_list: sub_t list) :
        TaintExp.TaintVarSet.t * (sub_t list) =
      let old_num = TaintExp.TaintVarSet.cardinal var_set in
      let var_set, subtype_opt_list = List.fold_left_map filter_helper var_set subtype_list in
      let subtype_list = List.filter_map (fun x -> x) subtype_opt_list in
      if TaintExp.TaintVarSet.cardinal var_set > old_num then
        filter filter_helper var_set subtype_list
      else
        var_set, subtype_list
    in
    let untaint_var, subtype_list = filter filter_untaint_helper TaintExp.TaintVarSet.empty subtype_list in
    let taint_var, subtype_list = filter filter_taint_helper TaintExp.TaintVarSet.empty subtype_list in
    []
  
end
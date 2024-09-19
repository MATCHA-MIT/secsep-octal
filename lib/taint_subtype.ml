open Single_entry_type
open Taint_exp
open Taint_entry_type
open Constraint
open Arch_type
open Pretty_print

module TaintSubtype = struct
  exception TaintSubtypeError of string

  let taint_subtype_error msg = raise (TaintSubtypeError ("[Taint Subtype Error] " ^ msg))

  type type_rel = {
    var_idx: TaintExp.taint_var_id;
    subtype: TaintExp.t;
    suptype: TaintExp.TaintVarSet.t;
  }

  type t = type_rel list

  module TaintEntryType = TaintEntryType (SingleEntryType)
  module ArchType = ArchType (TaintEntryType)

  type sub_t = TaintExp.t * TaintExp.t

  let pp_sub_t_list (lvl: int) (subtype_list: sub_t list) =
    PP.print_lvl lvl "<Subtype list>\n";
    List.iter (
      fun (x, y) -> PP.print_lvl (lvl + 1) "%s -> %s\n" (TaintExp.to_string x) (TaintExp.to_string y)
    ) subtype_list

  let pp_subtype (lvl: int) (tv_rel_list: t) =
    PP.print_lvl lvl "<SubtypeRel list>\n";
    List.iter (
      fun (x: type_rel) -> PP.print_lvl (lvl + 1) "%d: sub = %s; sup = %s\n" x.var_idx (TaintExp.to_string x.subtype) (TaintExp.to_string (TaintExp x.suptype))
    ) tv_rel_list

  let get_one_block_subtype
      (subtype_list: sub_t list)
      (block_subtype: ArchType.block_subtype_t) : sub_t list =
    let sup_block, sub_block_list = block_subtype in
    let subtype_list = Constraint.get_taint_sub sup_block.constraint_list @ subtype_list in
    let subtype_list =
      List.fold_left (
        fun acc (idx, te) ->
          (TaintExp.TaintVar idx, te) :: (te, TaintExp.TaintVar idx) :: acc
      ) subtype_list (snd sup_block.local_var_map)
    in
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

  let find_or_add_entry (tv_rel: t) (var_idx: TaintExp.taint_var_id) : t * type_rel =
    let find_entry = List.find_opt (fun x -> x.var_idx = var_idx) tv_rel in
    match find_entry with
    | Some entry -> tv_rel, entry
    | None ->
      let entry = {
        var_idx = var_idx;
        subtype = TaintVar var_idx;
        suptype = TaintExp.TaintVarSet.empty;
      } in
      entry :: tv_rel, entry

  let add_one_sub (tv_rel_list: t) (sub: TaintExp.t) (sup_id: TaintExp.taint_var_id) : t =
    List.map 
      (fun entry -> if entry.var_idx = sup_id then { entry with subtype = TaintExp.merge sub entry.subtype } else entry) 
      tv_rel_list

  let add_one_sub_super (sup_id: TaintExp.taint_var_id) (tv_rel_list: t) (sub_id: TaintExp.taint_var_id) : t =
    if sub_id = sup_id then tv_rel_list
    else
      List.map (
        fun (entry: type_rel) ->
          if entry.var_idx = sub_id then { entry with suptype = TaintExp.TaintVarSet.add sup_id entry.suptype }
          else if entry.var_idx = sup_id then { entry with subtype = TaintExp.merge (TaintVar sub_id) entry.subtype }
          else entry
      ) tv_rel_list

  let add_subid (sup_id: TaintExp.taint_var_id) (tv_rel_list: t) (sub_id: TaintExp.taint_var_id) : t =
    let tv_rel_list, sub_tv = find_or_add_entry tv_rel_list sub_id in
    let tv_rel_list =
      List.fold_left 
        (add_one_sub_super sup_id) tv_rel_list 
        (TaintExp.TaintVarSet.to_list (TaintExp.TaintVarSet.add sub_id (TaintExp.get_var_set sub_tv.subtype))) 
    in
    add_one_sub tv_rel_list sub_tv.subtype sup_id

  let add_sub (sub: TaintExp.t) (tv_rel_list: t) (sup_id: TaintExp.taint_var_id) : t =
    match sub with
    | TaintConst _ ->
      taint_subtype_error "add_sub should not have TaintConst subtype!!!"
      (* add_one_sub tv_rel_list sub sup_id *)
    | _ ->
      List.fold_left (add_subid sup_id) tv_rel_list (TaintExp.TaintVarSet.to_list (TaintExp.get_var_set sub))

  let update (tv_rel_list: t) (subtype: sub_t) : t * (sub_t option) =
    let sub, sup = subtype in
    match sup with
    | TaintVar sup_id ->
      let tv_rel_list, sup_tv_rel = find_or_add_entry tv_rel_list sup_id in
      let sup_list = TaintExp.TaintVarSet.add sup_id sup_tv_rel.suptype in
      List.fold_left (add_sub sub) tv_rel_list (TaintExp.TaintVarSet.to_list sup_list), None
    | TaintConst false -> taint_subtype_error (Printf.sprintf "update find %s -> TaintConst false" (TaintExp.to_string sub))
    | _ -> tv_rel_list, Some subtype

  let find_var_sol (tv_rel_list: t) : TaintExp.TaintVarSet.t * TaintExp.local_var_map_t =
    (* We solve all vars following their dependency order:
      1. If x -> y, then y's solution depends on x, then solver x first.
      2. If x = y, then we need to decide the solution to be either {x: x, y: x} or {x: y, y: y}
        1. Use positive vars to represent negative ones (since neg vars are local vars).
        2. If both positive, use the smaller one to represent the larger one (since smaller ones are likely to be input vars). *)
    let compare_idx (idx1: int) (idx2: int) : int =
      if idx1 >= 0 && idx2 < 0 then -1
      else if idx1 < 0 && idx2 >= 0 then 1
      else compare idx1 idx2
    in
    let tv_rel_list =
      List.sort (
        fun (x: type_rel) (y: type_rel) ->
          let x_sub_y = TaintExp.TaintVarSet.mem y.var_idx x.suptype in
          let y_sub_x = TaintExp.TaintVarSet.mem x.var_idx y.suptype in
          if x_sub_y && y_sub_x then compare_idx x.var_idx y.var_idx
          else if x_sub_y then -1
          else if y_sub_x then 1
          else compare_idx x.var_idx y.var_idx
      ) tv_rel_list
    in
    let helper 
        (acc: TaintExp.TaintVarSet.t) (tv_rel: type_rel) : 
        TaintExp.TaintVarSet.t * (TaintExp.taint_var_id * TaintExp.t) =
      match tv_rel.subtype with
      | TaintConst _ -> 
        taint_subtype_error (Printf.sprintf "find_var_sol get const subtype taint for %s -> %d" (TaintExp.to_string tv_rel.subtype) tv_rel.var_idx)
      | _ -> 
        let useful_sub_set = TaintExp.TaintVarSet.inter (TaintExp.get_var_set tv_rel.subtype) acc in
        if TaintExp.TaintVarSet.cardinal useful_sub_set = 0 then
          (TaintExp.TaintVarSet.add tv_rel.var_idx acc), (tv_rel.var_idx, TaintVar tv_rel.var_idx)
        else
          acc, (tv_rel.var_idx, TaintExp.eval (TaintExp useful_sub_set))
    in
    pp_subtype 0 tv_rel_list;
    List.fold_left_map helper TaintExp.TaintVarSet.empty tv_rel_list

  let solve_subtype_list (subtype_list: sub_t list) : TaintExp.local_var_map_t =
    pp_sub_t_list 0 subtype_list;
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
    let tv_rel_list, subtype_opt_list = List.fold_left_map update [] subtype_list in
    let _, sol_list = find_var_sol tv_rel_list in
    let subtype_list = List.filter_map (fun x -> x) subtype_opt_list in
    pp_sub_t_list 0 subtype_list;
    let subtype_list = List.map (fun (x, y) -> TaintExp.repl_context_var sol_list x, TaintExp.repl_context_var sol_list y) subtype_list in
    let new_taint_var, subtype_list = filter filter_taint_helper TaintExp.TaintVarSet.empty subtype_list in
    let sol_list =
      (List.map (fun x -> (x, TaintExp.TaintConst false)) (TaintExp.TaintVarSet.to_list untaint_var)) @
      (List.map (fun x -> (x, TaintExp.TaintConst true)) (TaintExp.TaintVarSet.to_list taint_var)) @
      List.map (fun (x, x_sol) -> x, TaintExp.repl_taint new_taint_var x_sol) sol_list
    in
    TaintExp.pp_local_var 0 sol_list;
    if List.length subtype_list = 0 then sol_list
    else begin
      pp_sub_t_list 0 subtype_list;
      taint_subtype_error "Cannot resolve all subtype relations!!!"
    end
  
end
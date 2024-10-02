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
    sol: TaintExp.t option;
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
    ) subtype_list;
    Printf.printf "\n"

  let sol_to_string (sol: TaintExp.t option) : string =
    match sol with
    | None -> "None"
    | Some e -> TaintExp.to_string e
  
  let pp_subtype (lvl: int) (tv_rel_list: t) =
    PP.print_lvl lvl "<SubtypeRel list>\n";
    List.iter (
      fun (x: type_rel) -> 
        PP.print_lvl (lvl + 1) 
          "%d: sol = %s sub = %s; sup = %s\n" 
          x.var_idx (sol_to_string x.sol) (TaintExp.to_string x.subtype) (TaintExp.to_string (TaintExp x.suptype))
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
    let reg_helper_entry 
        (subtype_list: sub_t list) 
        (sub_entry: TaintEntryType.t) (sup_entry: TaintEntryType.t) : sub_t list =
      let _, sub_taint = sub_entry in
      let _, sup_taint = sup_entry in
      (sub_taint, sup_taint) :: subtype_list
    in
    let mem_helper_entry 
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
      let subtype_list = List.fold_left2 reg_helper_entry subtype_list sub_block.reg_type sup_block.reg_type in
      ArchType.MemType.fold_left2 mem_helper_entry subtype_list sub_block.mem_type sup_block.mem_type
    in
    List.fold_left (helper sup_block) subtype_list sub_block_list    


  let get_taint_constraint 
      (block_subtype_list: ArchType.block_subtype_t list) : 
      sub_t list =
    List.fold_left get_one_block_subtype [] block_subtype_list

  let add_untaint (* for sub -> false *)
      (untaint_var: TaintExp.TaintVarSet.t)
      (sub: TaintExp.t) : TaintExp.TaintVarSet.t =
    match sub with
    | TaintConst false -> untaint_var
    | TaintConst true -> taint_subtype_error "add_untaint find unsat constraint ture -> false"
    | TaintVar v -> TaintExp.TaintVarSet.add v untaint_var
    | TaintExp e -> TaintExp.TaintVarSet.union e untaint_var
  
  let remove_untaint (* replace untaint var with false in sub of sub -> sup, if sub = false, return None *)
      (untaint_var: TaintExp.TaintVarSet.t)
      (subtype: sub_t) : sub_t option =
    let sub, sup = subtype in
    let sub = TaintExp.repl_untaint untaint_var sub in
    match sub with
    | TaintConst false -> None
    | _ -> Some (sub, sup)
  
  let filter_untaint_helper
      (untaint_var: TaintExp.TaintVarSet.t)
      (subtype: sub_t) :
      TaintExp.TaintVarSet.t * (sub_t option) =
    let sub, sup = subtype in
    match sup with
    | TaintConst false -> 
      (* Printf.printf "Add untaint %s\n" (TaintExp.to_string sub); *)
      add_untaint untaint_var sub, None
    | TaintConst true -> untaint_var, None (* This subtype relation implies nothing, so remove *)
    | _ ->
      if TaintExp.TaintVarSet.subset (TaintExp.get_var_set sup) (untaint_var) then begin
        (* Printf.printf "Add untaint %s -> %s\n" (TaintExp.to_string sub) (TaintExp.to_string sup); *)
        add_untaint untaint_var sub, None
      end else
        untaint_var, remove_untaint untaint_var subtype
      
  
  let add_taint (* for true -> sup *)
      (taint_var: TaintExp.TaintVarSet.t)
      (sup: TaintExp.t) : TaintExp.TaintVarSet.t * (sub_t option) =
    match sup with
    | TaintConst false -> taint_subtype_error "add_taint find unsat constraint true -> false"
    | TaintConst true -> taint_var, None
    | TaintVar v -> TaintExp.TaintVarSet.add v taint_var, None
    | _ -> taint_var, Some (TaintConst true, sup)
  
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
  
  let rec filter_naive
      (filter_helper: TaintExp.TaintVarSet.t -> sub_t -> TaintExp.TaintVarSet.t * (sub_t option))
      (var_set: TaintExp.TaintVarSet.t)
      (subtype_list: sub_t list) :
      TaintExp.TaintVarSet.t * (sub_t list) =
    let old_num = TaintExp.TaintVarSet.cardinal var_set in
    let old_len = List.length subtype_list in
    let var_set, subtype_opt_list = List.fold_left_map filter_helper var_set subtype_list in
    let subtype_list = List.filter_map (fun x -> x) subtype_opt_list in
    if TaintExp.TaintVarSet.cardinal var_set > old_num && List.length subtype_list < old_len then
      filter_naive filter_helper var_set subtype_list
    else
      var_set, subtype_list

  let find_or_add_entry_non_input (tv_rel: t) (var_idx: TaintExp.taint_var_id) : t * type_rel =
    let find_entry = List.find_opt (fun x -> x.var_idx = var_idx) tv_rel in
    match find_entry with
    | Some entry -> tv_rel, entry
    | None ->
      let entry = {
        var_idx = var_idx;
        sol = None;
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
    let tv_rel_list, sub_tv = find_or_add_entry_non_input tv_rel_list sub_id in
    let tv_rel_list =
      List.fold_left 
        (add_one_sub_super sup_id) tv_rel_list 
        (TaintExp.TaintVarSet.to_list (TaintExp.TaintVarSet.add sub_id (TaintExp.get_var_set sub_tv.subtype))) 
    in
    add_one_sub tv_rel_list sub_tv.subtype sup_id

  let add_sub (sub: TaintExp.t) (tv_rel_list: t) (sup_id: TaintExp.taint_var_id) : t =
    match sub with
    | TaintConst _ ->
      (* taint_subtype_error "add_sub should not have TaintConst subtype!!!" *)
      add_one_sub tv_rel_list sub sup_id
    | _ ->
      List.fold_left (add_subid sup_id) tv_rel_list (TaintExp.TaintVarSet.to_list (TaintExp.get_var_set sub))

  let update (tv_rel_list: t) (subtype: sub_t) : t * (sub_t option) =
    let sub, sup = subtype in
    match sup with
    | TaintVar sup_id ->
      let tv_rel_list, sup_tv_rel = find_or_add_entry_non_input tv_rel_list sup_id in
      let sup_list = TaintExp.TaintVarSet.add sup_id sup_tv_rel.suptype in
      List.fold_left (add_sub sub) tv_rel_list (TaintExp.TaintVarSet.to_list sup_list), None
    | TaintConst false -> taint_subtype_error (Printf.sprintf "update find %s -> TaintConst false" (TaintExp.to_string sub))
    | _ -> tv_rel_list, Some subtype

  let find_var_sol (input_var: TaintExp.TaintVarSet.t) (tv_rel_list: t) : t =
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

    let tv_rel_list = List.sort (fun x y -> compare_idx x.var_idx y.var_idx) tv_rel_list in
    Printf.printf "find_var_sol\n";
    pp_subtype 0 tv_rel_list;

    let helper_inner
        (acc: TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t) (tv_rel: type_rel) : 
        (TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t) * type_rel =
      if tv_rel.sol = None then
        let untaint_var, taint_var, useful_var, resolved_var = acc in
        match TaintExp.repl_taint taint_var (TaintExp.repl_untaint untaint_var tv_rel.subtype) with
        | TaintConst false -> 
          (TaintExp.TaintVarSet.add tv_rel.var_idx untaint_var,
          taint_var, useful_var,
          TaintExp.TaintVarSet.add tv_rel.var_idx resolved_var), 
          { tv_rel with sol = Some (TaintConst false) }
        | TaintConst true ->
          (untaint_var,
          TaintExp.TaintVarSet.add tv_rel.var_idx taint_var, 
          useful_var, 
          TaintExp.TaintVarSet.add tv_rel.var_idx resolved_var), 
          { tv_rel with sol = Some (TaintConst true) }
        | _ -> 
          let sub_var_set = TaintExp.get_var_set tv_rel.subtype in
          let real_sub_var_set = TaintExp.TaintVarSet.diff sub_var_set tv_rel.suptype in
          let unresolved_sub = 
            TaintExp.TaintVarSet.diff 
              (TaintExp.TaintVarSet.remove tv_rel.var_idx real_sub_var_set)
              resolved_var
          in
          if TaintExp.TaintVarSet.is_empty unresolved_sub then
            let useful_sub_set = TaintExp.TaintVarSet.inter (TaintExp.get_var_set tv_rel.subtype) useful_var in
            if TaintExp.TaintVarSet.is_empty useful_sub_set then
              if TaintExp.TaintVarSet.mem tv_rel.var_idx input_var then
                (untaint_var, taint_var, 
                TaintExp.TaintVarSet.add tv_rel.var_idx useful_var, 
                TaintExp.TaintVarSet.add tv_rel.var_idx resolved_var), 
                { tv_rel with sol = Some (TaintVar tv_rel.var_idx) }
              else
                (TaintExp.TaintVarSet.add tv_rel.var_idx untaint_var,
                taint_var, useful_var,
                TaintExp.TaintVarSet.add tv_rel.var_idx resolved_var), 
                { tv_rel with sol = Some (TaintConst false) }
            else
              (untaint_var, taint_var, useful_var, 
              TaintExp.TaintVarSet.add tv_rel.var_idx resolved_var),
              { tv_rel with sol = Some (TaintExp.eval (TaintExp useful_sub_set)) }
              (* (tv_rel.var_idx, TaintExp.eval (TaintExp useful_sub_set)) *)
          else
            acc, tv_rel
      else acc, tv_rel
    in
    let rec helper 
        (acc: TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t * TaintExp.TaintVarSet.t) 
        (tv_rel_list: t) : t = 
      let _, _, _, resolved_var = acc in
      let old_resolved = TaintExp.TaintVarSet.cardinal resolved_var in
      let (untaint_var, taint_var, useful_var, resolve_var), tv_rel_list = List.fold_left_map helper_inner acc tv_rel_list in
      let new_resolved = TaintExp.TaintVarSet.cardinal resolve_var in
      if List.find_opt (fun x -> x.sol = None) tv_rel_list = None then tv_rel_list
      else if new_resolved > old_resolved then helper (untaint_var, taint_var, useful_var, resolve_var) tv_rel_list
      else begin
        Printf.printf "%d %d %d\n" old_resolved new_resolved (List.length tv_rel_list);
        pp_subtype 0 tv_rel_list;
        taint_subtype_error "cannot find solution"
      end

    in
    helper (TaintExp.TaintVarSet.empty, TaintExp.TaintVarSet.empty, TaintExp.TaintVarSet.empty, TaintExp.TaintVarSet.empty) tv_rel_list

  let check_sub_t (entry: sub_t) : bool =
    match entry with
    | TaintConst false, _ -> true
    | TaintConst true, TaintConst true -> true
    | TaintVar _, TaintConst true -> true
    | TaintVar v1, TaintVar v2 -> v1 = v2
    | TaintVar v1, TaintExp s2 -> TaintExp.TaintVarSet.mem v1 s2
    | TaintExp s1, TaintExp s2 -> TaintExp.TaintVarSet.subset s1 s2
    | _ -> true

  let check_and_filter (subtype_list: sub_t list) : sub_t list =
    List.filter (
      fun (entry: sub_t) -> not (check_sub_t entry)
    ) subtype_list

  let rec solve_subtype_list_helper
      (input_var: TaintExp.TaintVarSet.t)
      (tv_rel_list: t) (subtype_list: sub_t list) : TaintExp.local_var_map_t =
    let orig_subtype_len = List.length subtype_list in
    (* Update tv_rel_list (which is expected to contain all constraints) *)
    let tv_rel_list, subtype_opt_list = List.fold_left_map update tv_rel_list subtype_list in
    let subtype_list = List.filter_map (fun x -> x) subtype_opt_list in
    (* Find sol based on current (might be incomplete) tv_rel_list *)
    let tv_rel_list = find_var_sol input_var tv_rel_list in
    let sol_list = List.map (fun x -> x.var_idx, Option.get x.sol) tv_rel_list in
    (* Simplify subtype_list with current sol (the sol might be incomplete) *)
    let subtype_list = 
      List.map (
        fun (x, y) -> TaintExp.repl_context_var_no_error sol_list x, TaintExp.repl_context_var_no_error sol_list y
      ) subtype_list 
    in
    (* Remove always sat subtype *)
    let subtype_list = check_and_filter subtype_list in
    let remain_subtype_len = List.length subtype_list in
    if remain_subtype_len = 0 then sol_list
    else if remain_subtype_len < orig_subtype_len then
      solve_subtype_list_helper input_var tv_rel_list subtype_list
    else begin
      pp_subtype 0 tv_rel_list;
      TaintExp.pp_local_var 0 sol_list;
      pp_sub_t_list 0 subtype_list;
      let remain = 
        List.filter (
          fun (x, y) -> not (check_sub_t (TaintExp.repl_context_var_no_error sol_list x, TaintExp.repl_context_var_no_error sol_list y))
        ) subtype_list 
      in
      pp_sub_t_list 0 remain;
      taint_subtype_error "Cannot handle subtype list"
    end


  let solve_subtype_list (input_var: TaintExp.TaintVarSet.t) (orig_subtype_list: sub_t list) : TaintExp.local_var_map_t =
    Printf.printf "solve_subtype_list, orig_subtype_list\n";
    pp_sub_t_list 0 orig_subtype_list;
    (* Handle naive cases*)
    let untaint_var, subtype_list = filter_naive filter_untaint_helper TaintExp.TaintVarSet.empty orig_subtype_list in
    (* let taint_var, subtype_list = filter_naive filter_taint_helper TaintExp.TaintVarSet.empty subtype_list in *)
    Printf.printf "Untaint var set: %s\n" (TaintExp.to_string (TaintExp.TaintExp untaint_var));
    Printf.printf "solve_subtype_list, after remove untaint subtype_list\n";
    pp_sub_t_list 0 subtype_list;

    let sol_list = solve_subtype_list_helper input_var [] subtype_list in
    
    (* Naive cases again? *)
    (* let new_taint_var, subtype_list = filter_naive filter_taint_helper TaintExp.TaintVarSet.empty subtype_list in *)

    (* Full solution, and update with new taint vars *)
    let sol_list =
      (List.map (fun x -> (x, TaintExp.TaintConst false)) (TaintExp.TaintVarSet.to_list untaint_var)) @ sol_list
      (* (List.map (fun x -> (x, TaintExp.TaintConst true)) (TaintExp.TaintVarSet.to_list taint_var)) @ *)
      (* List.map (fun (x, x_sol) -> x, TaintExp.repl_taint new_taint_var x_sol) sol_list *)
    in

    TaintExp.pp_local_var 0 sol_list;
    (* sol_list *)

    (* Optional: Final check solution!!! *)
    (* Remove always sat subtype *)
    let remain = 
      List.filter (
        fun (x, y) -> not (check_sub_t (TaintExp.repl_context_var sol_list x, TaintExp.repl_context_var sol_list y))
      ) subtype_list 
    in
    if List.length remain = 0 then sol_list
    else begin
      pp_sub_t_list 0 remain;
      taint_subtype_error "Cannot resolve all subtype relations!!!"
    end
  
end
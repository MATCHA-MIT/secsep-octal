(* Structure to hold subtype relations *)
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type
open Code_type
open Pretty_print

module SubType = struct
  exception SubTypeError of string

  let sub_type_error msg = raise (SubTypeError ("[Sub Type Error] " ^ msg))

  type type_var_rel = {
    type_var_idx: TypeExp.type_var_id;
    type_sol: TypeFullExp.type_sol;
    subtype_list: TypeFullExp.t list;
    supertype_list: (TypeFullExp.CondVarSet.t * TypeExp.type_var_id) list; (* Entry (set, idx) refers to (TypeVar type_var_idx, set) -> (TypeVar idx, {}) *)
  }

  type t = type_var_rel list

  let init (total_var: int) =
    List.init total_var (fun x -> {type_var_idx = x; type_sol = SolNone; subtype_list = []; supertype_list = []})

  let get_pure_sol (sol: t) : (TypeExp.type_var_id * TypeFullExp.type_sol) list =
    List.map (fun x -> (x.type_var_idx, x.type_sol)) sol

  let get_type_var_rel (tv_rel: t) (idx: int) : type_var_rel =
    List.find (fun x -> x.type_var_idx = idx) tv_rel

  let type_list_insert (type_list: TypeFullExp.t list) (ty: TypeFullExp.t) : TypeFullExp.t list =
    let t_exp, t_cond = ty in
    let helper (x: TypeFullExp.t) : (TypeFullExp.t, TypeFullExp.t) Either.t =
      let x_exp, x_cond = x in
      if (TypeExp.cmp x_exp t_exp = 0) then Right (x_exp, TypeFullExp.CondVarSet.inter x_cond t_cond)
        (* Here we are being conservative on calculating OR of two conds with Ints.Inter *)
      else Left x
    in
    let l_list, r_list = List.partition_map helper type_list in
    match r_list with
    | [] -> ty :: l_list
    | hd :: tl ->
      begin match tl with
      | [] -> hd :: l_list
      | _ -> sub_type_error ("type_list_insert: find more than one match")
      end

  let add_one_sub_type (tv_rel: type_var_rel) (ty: TypeFullExp.t) : type_var_rel =
    {tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty}
    (* {tv_rel with subtype_list = ty :: tv_rel.subtype_list} *)

  let type_var_list_insert 
      (type_var_list: (TypeFullExp.CondVarSet.t * TypeExp.type_var_id) list) 
      (a_cond: TypeFullExp.CondVarSet.t) 
      (b_idx) : (TypeFullExp.CondVarSet.t * TypeExp.type_var_id) list =
    let helper (x: TypeFullExp.CondVarSet.t * TypeExp.type_var_id) : 
        (TypeFullExp.CondVarSet.t * TypeExp.type_var_id, TypeFullExp.CondVarSet.t * TypeExp.type_var_id) Either.t =
      let x_cond, x_idx = x in 
      if x_idx = b_idx then Right (TypeFullExp.CondVarSet.inter x_cond a_cond, x_idx)
        (* Here we are being conservative on calculating OR of two conds with Ints.Inter *)
      else Left x
    in
    let l_list, r_list = List.partition_map helper type_var_list in
    match r_list with
    | [] -> (a_cond, b_idx) :: l_list
    | hd :: tl ->
      begin match tl with
      | [] -> hd :: l_list
      | _ -> sub_type_error ("type_var_list_insert: find more than one match")
      end

  let add_one_super_type (tv_rel: type_var_rel) (a_cond: TypeFullExp.CondVarSet.t) (b_idx) : type_var_rel =
    {tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list a_cond b_idx}
    (* {tv_rel with supertype_list = (a_cond, b_idx) :: tv_rel.supertype_list} *)

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a: TypeFullExp.t) (b_idx: int) : t =
    match a with
    | (TypeVar a_idx, a_cond) ->
      if a_idx = b_idx then tv_rel
      else
        List.map (fun x -> 
                    if x.type_var_idx = a_idx then 
                      (* {x with supertype_list = (a_cond, b_idx) :: x.supertype_list} *)
                      add_one_super_type x a_cond b_idx
                    else if x.type_var_idx = b_idx then
                      (* {x with subtype_list = a :: x.subtype_list} *)
                      add_one_sub_type x a
                    else x) tv_rel
    | _ ->
      List.map (fun x -> if x.type_var_idx = b_idx then add_one_sub_type x a else x) tv_rel

  let add_sub_sub_super (tv_rel: t) (a: TypeFullExp.t) (b_idx: int) : t =
    match a with
    | (TypeVar a_idx, a_cond) ->
      let a_rel = get_type_var_rel tv_rel a_idx in
      let new_tv_rel = List.fold_left 
                        (fun acc_tv_rel sub_a -> 
                          add_one_sub_super acc_tv_rel (CodeType.add_type_cond_set sub_a a_cond) b_idx) 
                        tv_rel a_rel.subtype_list in
      add_one_sub_super new_tv_rel a b_idx
    | _ -> add_one_sub_super tv_rel a b_idx
  
  let add_sub_sub_super_super (tv_rel: t) (a: TypeFullExp.t) (b_idx: int) : t =
    let b_rel = get_type_var_rel tv_rel b_idx in
    let new_tv_rel = List.fold_left
                      (fun acc_tv_rel (b_cond, super_b_idx) ->
                        add_sub_sub_super acc_tv_rel (CodeType.add_type_cond_set a b_cond) super_b_idx)
                      tv_rel b_rel.supertype_list in
    add_sub_sub_super new_tv_rel a b_idx
      
  let add_sub_type_full_exp (tv_rel: t) (a: TypeFullExp.t) (b: TypeFullExp.t) : t =
    match b with
    | (TypeVar b_idx, b_cond) ->
      if TypeFullExp.CondVarSet.is_empty b_cond then add_sub_sub_super_super tv_rel a b_idx
      else sub_type_error ("add_sub_type_full_exp: super type must has empty condition")
    | _ -> 
      let a_exp, _ = a in
      let b_exp, _ = b in
      if TypeExp.cmp a_exp b_exp = 0 then tv_rel (* Handle the special case for rsp *)
      else sub_type_error ("add_sub_type_full_exp: incorrect sub/super types " ^ 
        (TypeExp.string_of_type_exp a_exp) ^ " " ^ (TypeExp.string_of_type_exp b_exp))

  let add_sub_state_type (tv_rel: t) (start_type: CodeType.state_type) (end_type: CodeType.state_type) : t =
    let start_code_type = start_type.reg_type in
    let start_mem_type = start_type.mem_type in
    let end_code_type = end_type.reg_type in
    let end_mem_type = end_type.mem_type in
    let tv_rel_after_reg =
      List.fold_left2 add_sub_type_full_exp tv_rel start_code_type end_code_type in
    List.fold_left2 
      (fun acc_tv_rel (start_off, start_type) (end_off, end_type) ->
        if start_off = end_off then add_sub_type_full_exp acc_tv_rel start_type end_type
        else sub_type_error ("add_sub_state_type: mem type offset does not match"))
      tv_rel_after_reg start_mem_type end_mem_type
  
  let remove_one_var_dummy_sub (tv_rel: type_var_rel) : type_var_rel =
    let no_var_subtype = 
      List.filter (fun (exp: TypeFullExp.t) -> 
        match exp with
        | (TypeVar _, _)
        | (TypeBot, _ ) -> false
        | _ -> true) tv_rel.subtype_list
      in
      { tv_rel with subtype_list = no_var_subtype }

  let remove_all_var_dummy_sub (tv_rel: t) : t =
    List.map remove_one_var_dummy_sub tv_rel

  let rec simplify_one_sub  
      (new_type_var_idx: TypeExp.type_var_id) 
      (super_type_list: (TypeFullExp.CondVarSet.t * TypeExp.type_var_id) list)
      (fe: TypeFullExp.t) : TypeFullExp.t =
    let e, old_cond = fe in
    match e with
    | TypeVar v ->
      begin match List.find_opt (fun (_, super_idx) -> super_idx = v) super_type_list with
      | Some (cond, _) -> (TypeVar new_type_var_idx, TypeFullExp.CondVarSet.union old_cond cond)
      | None -> fe
      end
    | TypeBExp (op, l, r) ->
      let l', cond_l = simplify_one_sub new_type_var_idx super_type_list (l, old_cond) in
      let r', cond_r = simplify_one_sub new_type_var_idx super_type_list (r, old_cond) in
      (TypeBExp (op, l', r'), TypeFullExp.CondVarSet.union cond_l cond_r)
    | TypeUExp (op, e) ->
      let e', cond_e = simplify_one_sub new_type_var_idx super_type_list (e, old_cond) in
      (TypeUExp (op, e'), cond_e)
    | _ -> fe

  let simplify_one_var (tv_rel: type_var_rel) : type_var_rel =
    (* let no_var_subtype = 
      List.filter (fun (exp: TypeFullExp.t) -> 
        match exp with
        | (TypeVar _, _)
        | (TypeBot, _ ) -> false
        | _ -> true) tv_rel.subtype_list in *)
    { tv_rel with subtype_list = List.map (simplify_one_sub tv_rel.type_var_idx tv_rel.supertype_list) tv_rel.subtype_list }
    (* {
      type_var_idx = tv_rel.type_var_idx;
      subtype_list = List.map (simplify_one_sub tv_rel.type_var_idx tv_rel.supertype_list) no_var_subtype;
      supertype_list = tv_rel.supertype_list
    } *)

  let simplify_all_var (tv_rel: t) : t =
    List.map simplify_one_var tv_rel

  let find_loop_base (fe_list: TypeFullExp.t list) : SingleExp.t option =
    let helper (fe: TypeFullExp.t) : SingleExp.t option =
      let e, _ = fe in
      match e with
      | TypeSingle s -> Some s
      | _ -> None
    in
    List.find_map helper fe_list

  let find_loop_bound (fe_list: TypeFullExp.t list) (idx: TypeExp.type_var_id) : (TypeFullExp.t * int64 * bool) option =
    let helper (fe: TypeFullExp.t) : (TypeFullExp.t * int64 * bool) option =
      let e, _ = fe in
      match e with
      | TypeBExp (bop, TypeVar v, TypeSingle s)
      | TypeBExp (bop, TypeSingle s, TypeVar v) ->
        if v = idx && bop = TypeAdd then
          match s with
          | SingleConst c ->
            if c = 0L then sub_type_error ("find_loop_bound: step should be non-zero")
            else if c > 0L then Some (fe, c, true)
            else Some (fe, Int64.neg c, false)
          | _ -> None (* TODO!!! *)
        else None
      | _ -> None
    in
    List.find_map helper fe_list

  let find_cond_naive 
      (cond_list: CondType.t list) 
      (cond_set: TypeFullExp.CondVarSet.t) 
      (* (tv_idx: TypeExp.type_var_id) *)
      (bound_exp: TypeExp.t)
      (* (super_type_list: (TypeFullExp.CondVarSet.t * TypeExp.type_var_id) list) *)
      (base: SingleExp.t) (step: int64) (inc: bool) : TypeFullExp.type_sol =
    let helper (acc: TypeFullExp.type_sol) (cond_idx: int) : TypeFullExp.type_sol = (* (SingleExp.t * bool) option = *)
      match acc with
      | SolSimple _ | SolCond _ -> acc
      | SolNone ->
        let cond = CondType.get_cond_type cond_list cond_idx in
        let pure_cond_idx = cond_idx / 2 in
        begin match cond with
        | CondNe (fe, (TypeSingle bound, _))
        | CondNe ((TypeSingle bound, _), fe) ->
          (* let new_e, _ = simplify_one_sub tv_idx super_type_list fe in *)
          let new_e, _ = fe in
          if TypeExp.cmp bound_exp new_e = 0 then 
            if inc then
              let bound_1 = SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, bound, SingleExp.SingleConst step)) in
              let bound_2 = SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, bound_1, SingleExp.SingleConst step)) in
              SolCond (
                TypeRange (base, true, bound_1, true, step), 
                pure_cond_idx, 
                TypeRange (base, true, bound_2, true, step),
                TypeSingle bound_1
                )
            else
              let bound_1 = SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, bound, SingleExp.SingleConst step)) in
              let bound_2 = SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleSub, bound_1, SingleExp.SingleConst step)) in
              SolCond (
                TypeRange (bound_1, true, base, true, step), 
                pure_cond_idx, 
                TypeRange (bound_2, true, base, true, step),
                TypeSingle bound_1
                )
            (* Some (bound, false)  *)
          else SolNone
        | _ -> SolNone
        end
    in
    (* TODO: merge the following match to helper *)
    List.fold_left helper SolNone (TypeFullExp.CondVarSet.to_list cond_set)
    (* TODO: Maybe use close for both sides!!! *)

  (* TODO: these two functions should both use find_map or fold_left *)
  let find_cond_other
      (cond_list: CondType.t list)
      (cond_set: TypeFullExp.CondVarSet.t) 
      (* (tv_idx: TypeExp.type_var_id) *)
      (* (bound: TypeExp.t) *)
      (base: SingleExp.t) (step: int64) (inc: bool)
      (tv_rel_list: t) : TypeFullExp.type_sol =
    let helper (acc: TypeFullExp.type_sol) (cond_idx: int) : TypeFullExp.type_sol =
      match acc with
      | SolSimple _ | SolCond _ -> acc
      | SolNone ->
        let cond = CondType.get_cond_type cond_list cond_idx in
        begin match cond with
        | CondNe ((e, _), (TypeSingle _, _))
        | CondNe ((TypeSingle _, _), (e, _)) ->
          begin match e with
          | TypeBExp (TypeAdd, TypeVar v, TypeSingle (SingleConst c))
          | TypeBExp (TypeAdd, TypeSingle (SingleConst c), TypeVar v) ->
            let v_tv_rel = List.find (fun (x: type_var_rel) -> x.type_var_idx = v) tv_rel_list in
            let v_cond_set_option = List.find_map 
              (fun (fx: TypeFullExp.t) ->
                let x, cond = fx in
                if TypeExp.cmp x e = 0 then Some cond else None)
              v_tv_rel.subtype_list
            in
            begin match v_cond_set_option with
            | Some v_cond_set ->
              if TypeFullExp.CondVarSet.equal v_cond_set cond_set then
                (* Use v to represent solution of tv_idx *)
                let v_base_option = List.find_map
                  (fun (fx: TypeFullExp.t) ->
                    let x, _ = fx in
                    match x with 
                    | TypeSingle s -> Some s
                    | _ -> None)
                  v_tv_rel.subtype_list
                in
                begin match v_base_option with
                | None -> SolNone
                | Some v_base -> 
                  let original_step = if inc then step else Int64.neg step in
                  (* TODO: Check whether c divides original_step or not *)
                  SolSimple (TypeExp.eval (TypeBExp (TypeAdd, TypeBExp (TypeMul, TypeBExp (TypeSub, TypeVar v, TypeSingle v_base), TypeSingle (SingleConst (Int64.div original_step c))), TypeSingle base)))
                end
              else SolNone
            | _ -> SolNone
            end
          | _ -> SolNone
          end
        | _ -> SolNone
        end
    in
    List.fold_left helper SolNone (TypeFullExp.CondVarSet.to_list cond_set)

  let try_solve_one_var (tv_rel: type_var_rel) (cond_list: CondType.t list) (tv_rel_list: t) : type_var_rel =
    let try_solve_top (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol =
      let helper (found: bool) (fe: TypeFullExp.t) : bool =
        if found then true
        else begin
          let exp, _ = fe in
          match exp with
          | TypeExp.TypeTop -> true
          | _ -> false
        end
      in
      let found = List.fold_left helper false subtype_list in
      if found then SolSimple TypeExp.TypeTop
      else SolNone
    in
    let try_solve_single_sub_val (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol =
      match subtype_list with
      | hd :: [] ->
        let exp, _  = hd in
        if TypeExp.is_val exp then SolSimple exp else SolNone
      | _ -> SolNone
    in
    let try_solve_loop_cond (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol =
      let find_base = find_loop_base subtype_list in
      let find_bound = find_loop_bound subtype_list tv_rel.type_var_idx in
      match find_base, find_bound with
      | Some base, Some ((bound_var, bound_cond), step, inc) -> 
        begin match find_cond_naive cond_list bound_cond bound_var base step inc with
        | SolNone -> find_cond_other cond_list bound_cond base step inc tv_rel_list
        | naive_sol -> naive_sol
        (* TODO: Add rule for another case, and check whether this rule only works in the second round *)
        end
      | _ -> SolNone
    in
    let solve_rules : ((TypeFullExp.t list) -> TypeFullExp.type_sol) list = [
      try_solve_top;
      try_solve_single_sub_val;
      try_solve_loop_cond
    ]
    in
    let subtype_list = tv_rel.subtype_list in
    let helper (acc: TypeFullExp.type_sol) (rule: (TypeFullExp.t list) -> TypeFullExp.type_sol) : TypeFullExp.type_sol =
      match acc with
      | SolNone -> rule subtype_list
      | _ -> acc
    in
    { tv_rel with type_sol = List.fold_left helper tv_rel.type_sol solve_rules }
    (* match try_solve_single_sub_val subtype_list with
    | Some e -> { tv_rel with type_sol = Some e }
    | None -> { tv_rel with type_sol = None }  *)
    (* TODO Add more rules *)

  let try_solve_vars (tv_rel_list: t) (cond_list: CondType.t list)  : ((TypeExp.type_var_id * TypeFullExp.type_sol) list) * t =
    let helper (acc: (TypeExp.type_var_id * TypeFullExp.type_sol) list) (tv_rel: type_var_rel) : ((TypeExp.type_var_id * TypeFullExp.type_sol) list) * type_var_rel =
      (* let new_tv_rel = try_solve_one_var (simplify_one_var tv_rel) cond_list tv_rel_list in *)
      let new_tv_rel = try_solve_one_var tv_rel cond_list tv_rel_list in
      match new_tv_rel.type_sol with
      | SolNone -> (acc, tv_rel)
      | e -> ((new_tv_rel.type_var_idx, e) :: acc, new_tv_rel)
    in
    List.fold_left_map helper [] tv_rel_list

  let update_type_var_rel (tv_rel: type_var_rel) (sol: TypeExp.type_var_id * TypeFullExp.type_sol) : type_var_rel =
    (* TODO: When replace variables with their solutions, we should consider about the effects of conditions!!! *)
    match tv_rel.type_sol with
    | SolNone -> { tv_rel with subtype_list = List.map (TypeFullExp.repl_type_sol sol) tv_rel.subtype_list }
    | _ -> tv_rel

  let merge_sub_type (fe_list: TypeFullExp.t list) : TypeFullExp.t list =
    let helper (acc: TypeFullExp.t) (fe: TypeFullExp.t) : ((TypeFullExp.t) * TypeFullExp.t) =
      let acc_e, _ = acc in
      let e, _ = fe in
      match acc_e, e with
      | TypeBot, TypeSingle _
      | TypeBot, TypeRange _ -> (fe, (TypeBot, TypeFullExp.CondVarSet.empty))
      | TypeSingle _, TypeRange _
      | TypeRange _, TypeSingle _ ->
        begin match TypeFullExp.merge acc fe with
        | Some new_fe -> ((TypeBot, TypeFullExp.CondVarSet.empty), new_fe)
        | None -> (acc, fe)
        end
      | _ -> (acc, fe)
    in
    let new_acc, new_list = List.fold_left_map helper (TypeBot, TypeFullExp.CondVarSet.empty) fe_list in
    let helper (fe: TypeFullExp.t) : bool =
      match fe with
      | (TypeBot, _) -> false
      | _ -> true
    in
    List.filter helper (new_acc :: new_list)

  let simplify_tv_rel_sub (tv_rel: type_var_rel) : type_var_rel =
    match tv_rel.type_sol with
    | SolNone -> {tv_rel with subtype_list = merge_sub_type tv_rel.subtype_list}
    | _ -> tv_rel

  let rec solve_vars (tv_rel_list: t) (cond_list: CondType.t list) (num_iter: int) : t =
    if num_iter == 0 then tv_rel_list
    else begin
      let new_sol_list, new_tv_rel_list = try_solve_vars tv_rel_list cond_list in
      match new_sol_list with
      | [] -> new_tv_rel_list
      | _ ->
        let update_tv_rel_list = List.map (fun (tv: type_var_rel) -> List.fold_left update_type_var_rel tv new_sol_list) new_tv_rel_list in
        let update_cond_list = List.map (fun (cond: CondType.t) -> List.fold_left CondType.repl_type_sol cond new_sol_list) cond_list in
        let merged_tv_rel_list = List.map simplify_tv_rel_sub update_tv_rel_list in
        solve_vars merged_tv_rel_list update_cond_list (num_iter - 1)
    end

  let string_of_sol (sol: TypeExp.t option) =
    match sol with
    | None -> "None"
    | Some e -> "Some " ^ (TypeExp.string_of_type_exp e)

  let pp_tv_rels (lvl: int) (tv_rels: t) =
    List.iter (fun x ->
      PP.print_lvl lvl "<TypeVar %d>\n" x.type_var_idx;
      PP.print_lvl (lvl + 1) "Solution: %s\n" (TypeFullExp.string_of_type_sol x.type_sol);
      PP.print_lvl (lvl + 1) "SubType: [\n";
      List.iter (fun tf -> 
        TypeFullExp.pp_type_full_exp (lvl + 2) tf;
        Printf.printf ";\n";
      ) x.subtype_list;
      PP.print_lvl (lvl + 1) "]\n";
      PP.print_lvl (lvl + 1) "SuperType: [\n";
      List.iter (fun (cond, id) ->
        PP.print_lvl (lvl + 2) "(TypeVar: %d,\n" id;
        PP.print_lvl (lvl + 2) " Cond: %s);\n" (TypeFullExp.string_of_cond_status cond);
      ) x.supertype_list;
      PP.print_lvl (lvl + 1) "]\n"
    ) tv_rels

end


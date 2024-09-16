(* Structure to hold subtype relations *)
open Single_exp
open Type_exp
open Type_full_exp
open Cond_type
open Mem_offset
open Smt_emitter
open State_type
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

  let remove_unused (tv_rel_list: t) (drop_var_set: TypeExp.TypeVarSet.t) : t =
    List.filter (fun x -> (TypeExp.TypeVarSet.find_opt x.type_var_idx drop_var_set) = None) tv_rel_list

  let add_new (tv_rel_list: t) (new_var_set: TypeExp.TypeVarSet.t) : t =
    let new_tv_rel_list = 
      List.map 
        (fun x -> {type_var_idx = x; type_sol = SolNone; subtype_list = []; supertype_list = []}) 
        (TypeExp.TypeVarSet.elements new_var_set) 
    in
    tv_rel_list @ new_tv_rel_list

  let clear (tv_rel_list: t) : t =
    List.map 
    (fun rel -> { type_var_idx = rel.type_var_idx; type_sol = SolNone; subtype_list = []; supertype_list = []})
    tv_rel_list

  let get_pure_sol (sol: t) : (TypeExp.type_var_id * TypeFullExp.type_sol) list =
    List.map (fun x -> (x.type_var_idx, x.type_sol)) sol

  let get_type_var_rel (tv_rel: t) (idx: TypeExp.type_var_id) : type_var_rel =
    let find_result = List.find_opt (fun x -> x.type_var_idx = idx) tv_rel in
    match find_result with
    | Some x -> x
    | None -> { type_var_idx = idx; type_sol = SolNone; subtype_list = []; supertype_list = [] }

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

  let add_type_var_rel_opt (tv_rel: t) (idx: TypeExp.type_var_id) (new_type_opt: TypeFullExp.t option) : t =
    match new_type_opt with
    | Some fe -> 
      (* { type_var_idx = idx; type_sol = SolNone; subtype_list = [fe]; supertype_list = [] } :: tv_rel *)
      let helper (acc: bool) (tv: type_var_rel) : bool * type_var_rel =
        begin match acc, tv.type_var_idx = idx with
        | true, true -> sub_type_error "add_type_var_rel_opt found and idx match should not be both true"
        | false, true -> (true, add_one_sub_type tv fe)
        | _ -> (acc, tv)
        end
      in
      let found, new_tv_rel = List.fold_left_map helper false tv_rel in
      if found then new_tv_rel
      else { type_var_idx = idx; type_sol = SolNone; subtype_list = [fe]; supertype_list = [] } :: new_tv_rel
    | None -> tv_rel

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
                          add_one_sub_super acc_tv_rel (TypeFullExp.add_type_cond_set sub_a a_cond) b_idx) 
                        tv_rel a_rel.subtype_list in
      add_one_sub_super new_tv_rel a b_idx
    | _ -> add_one_sub_super tv_rel a b_idx
  
  let add_sub_sub_super_super (tv_rel: t) (a: TypeFullExp.t) (b_idx: int) : t =
    let b_rel = get_type_var_rel tv_rel b_idx in
    let new_tv_rel = List.fold_left
                      (fun acc_tv_rel (b_cond, super_b_idx) ->
                        add_sub_sub_super acc_tv_rel (TypeFullExp.add_type_cond_set a b_cond) super_b_idx)
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
        (TypeExp.to_string a_exp) ^ " " ^ (TypeExp.to_string b_exp))
  
  let add_sub_type_exp (a_cond: TypeFullExp.CondVarSet.t) (tv_rel: t) (a: TypeExp.t) (b: TypeExp.t) : t =
    match b with
    | TypeVar b_idx ->
      add_sub_sub_super_super tv_rel (a, a_cond) b_idx
    | _ -> 
      sub_type_error ("add_sub_type_exp: incorrect sub/super types " ^ 
        (TypeExp.to_string a) ^ " " ^ (TypeExp.to_string b))

  let add_sub_state_type (tv_rel: t) (start_type: StateType.t) (end_type: StateType.t) : t =
    let start_code_type = start_type.reg_type in
    let start_mem_type = start_type.mem_type.mem_type in
    let end_code_type = end_type.reg_type in
    let end_mem_type = end_type.mem_type.mem_type in
    let tv_rel_after_reg =
      List.fold_left2 (add_sub_type_exp start_type.cond_hist) tv_rel start_code_type end_code_type in
    List.fold_left2 (
      fun acc_tv_rel (start_ptr, start_mem_type) (end_ptr, end_mem_type) ->
        if start_ptr = end_ptr then
          List.fold_left2 
            (fun acc_tv_rel (start_off, start_mem_type) (end_off, end_mem_type) ->
              if MemOffset.cmp start_off end_off = 0 then (* SingleExp.cmp start_off1 end_off1 = 0 && SingleExp.cmp start_off2 end_off2 = 0 *) 
                (add_sub_type_exp start_type.cond_hist) acc_tv_rel start_mem_type end_mem_type
              else sub_type_error ("add_sub_state_type: mem type offset does not match"))
            acc_tv_rel
            start_mem_type end_mem_type
        else sub_type_error ("add_sub_state_type: mem type ptr does not match")
    ) tv_rel_after_reg start_mem_type end_mem_type
  
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
        (* One side should keep type var to represent the relation, the other side should reduced to value to be served as a boundary!!! *)
        (* Maybe we should update the boundary part as an optimization. *)
        | CondNe ((new_e, _), (_, TypeSingle bound), _)
        | CondNe ((_, TypeSingle bound), (new_e, _), _) ->
          (* let new_e, _ = simplify_one_sub tv_idx super_type_list fe in *)
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
        | CondLq ((new_e, _), (_, TypeSingle bound), _) ->
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
              SolNone
          else SolNone
        | CondLq ((_, TypeSingle bound), (new_e, _), _) ->
          if TypeExp.cmp bound_exp new_e = 0 then 
            if inc then
              SolNone
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
        | CondNe ((e, _), (_, TypeSingle _), _)
        | CondNe ((_, TypeSingle _), (e, _), _) ->
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

  let try_solve_one_var (smt_ctx: SmtEmitter.t) (tv_rel: type_var_rel) (cond_list: CondType.t list) (tv_rel_list: t) : type_var_rel * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t =
    let try_solve_top (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol * MemOffset.ConstraintSet.t =
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
      if found then (SolSimple TypeExp.TypeTop, MemOffset.ConstraintSet.empty)
      else (SolNone, MemOffset.ConstraintSet.empty)
    in
    let try_solve_single_sub_val (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol * MemOffset.ConstraintSet.t =
      match subtype_list with
      | hd :: [] ->
        let exp, _  = hd in
        if TypeExp.is_val exp then (SolSimple exp, MemOffset.ConstraintSet.empty) else (SolNone, MemOffset.ConstraintSet.empty)
      | _ -> (SolNone, MemOffset.ConstraintSet.empty)
    in
    let try_solve_list_single_sub_val (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol * MemOffset.ConstraintSet.t =
      let helper (fe: TypeFullExp.t) : (SingleExp.t, unit) Either.t =
        let e, _ = fe in
        match e with
        | TypeSingle s -> Left s
        | _ -> Right ()
      in
      let left_list, right_list = List.partition_map helper subtype_list in
      if List.length right_list <> 0 then (SolNone, MemOffset.ConstraintSet.empty)
      else begin
        let find_bound 
            (acc: SingleExp.t * SingleExp.t * MemOffset.ConstraintSet.t) (entry: SingleExp.t) :
            SingleExp.t * SingleExp.t * MemOffset.ConstraintSet.t =
          let min, max, constraint_set = acc in
          match MemOffset.conditional_ge smt_ctx min entry with
          | MemOffset.SatYes -> (entry, max, constraint_set)
          | MemOffset.SatCond constraints -> (entry, max, MemOffset.ConstraintSet.union constraint_set constraints) (* warning: undetermine, TODO: return SolNone *)
          | MemOffset.SatNo -> begin
            match MemOffset.conditional_ge smt_ctx entry max with
            | MemOffset.SatYes -> (min, entry, constraint_set)
            | MemOffset.SatCond constraints -> (min, entry, MemOffset.ConstraintSet.union constraint_set constraints) (* warning: undetermine *)
            | MemOffset.SatNo -> (min, max, constraint_set) (* the constraint set is not populated *)
          end
        in
        match left_list with
        | hd1 :: hd2 :: tl -> 
          let left, right, constriant_set =
          List.fold_left find_bound (hd1, hd1, MemOffset.ConstraintSet.empty) (hd2 :: tl) in
          (SolSimple (TypeExp.TypeRange (left, true, right, true, 1L)), constriant_set)
        | _ -> (SolNone, MemOffset.ConstraintSet.empty)
      end
    in
    let try_solve_loop_cond (subtype_list: TypeFullExp.t list) : TypeFullExp.type_sol * MemOffset.ConstraintSet.t =
      let find_base = find_loop_base subtype_list in
      let find_bound = find_loop_bound subtype_list tv_rel.type_var_idx in
      match find_base, find_bound with
      | Some base, Some ((bound_var, bound_cond), step, inc) -> 
        begin match find_cond_naive cond_list bound_cond bound_var base step inc with
        | SolNone -> (find_cond_other cond_list bound_cond base step inc tv_rel_list, MemOffset.ConstraintSet.empty)
        | naive_sol -> (naive_sol, MemOffset.ConstraintSet.empty)
        (* TODO: Add rule for another case, and check whether this rule only works in the second round *)
        end
      | _ -> (SolNone, MemOffset.ConstraintSet.empty)
    in
    let solve_rules : ((TypeFullExp.t list) -> TypeFullExp.type_sol * MemOffset.ConstraintSet.t) list = [
      try_solve_top;
      try_solve_single_sub_val;
      try_solve_list_single_sub_val;
      try_solve_loop_cond
    ]
    in
    let subtype_list = tv_rel.subtype_list in
    let helper 
        (acc: TypeFullExp.type_sol * MemOffset.ConstraintSet.t) 
        (rule: (TypeFullExp.t list) -> TypeFullExp.type_sol * MemOffset.ConstraintSet.t) : 
        TypeFullExp.type_sol *  MemOffset.ConstraintSet.t =
      let acc_sol, _ = acc in
      match acc_sol with
      | SolNone -> rule subtype_list
      | _ -> acc
    in
    let sol, sol_cons = List.fold_left helper (tv_rel.type_sol, MemOffset.ConstraintSet.empty) solve_rules in
    let sol_useful =
      if sol = SolNone then
        TypeExp.var_union (List.map (fun (e, _) -> TypeExp.get_vars e) subtype_list)
      else TypeExp.TypeVarSet.empty
    in
    ({ tv_rel with type_sol = sol }, sol_cons, sol_useful)
    (* match try_solve_single_sub_val subtype_list with
    | Some e -> { tv_rel with type_sol = Some e }
    | None -> { tv_rel with type_sol = None }  *)
    (* TODO Add more rules *)

  let try_solve_vars
      (smt_ctx: SmtEmitter.t)
      (tv_rel_list: t) 
      (cond_list: CondType.t list) 
      (useful_set: TypeExp.TypeVarSet.t) : 
      (((TypeExp.type_var_id * TypeFullExp.type_sol) list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * t =
    let helper (acc: (TypeExp.type_var_id * TypeFullExp.type_sol) list * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) (tv_rel: type_var_rel) : 
      ((TypeExp.type_var_id * TypeFullExp.type_sol) list * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t) * type_var_rel =
      (* let new_tv_rel = try_solve_one_var (simplify_one_var tv_rel) cond_list tv_rel_list in *)
      (* TODO: 1. test whether need to solve 2. update useful_set *)
      let acc_sol, acc_cons, acc_useful = acc in
      (* Printf.printf "!!!Useful vars\n";
      TypeExp.pp_type_var_set 0 acc_useful;
      Printf.printf "\n"; *)
      if TypeExp.TypeVarSet.find_opt tv_rel.type_var_idx acc_useful <> None then
        let new_tv_rel, sol_cons, sol_useful = try_solve_one_var smt_ctx tv_rel cond_list tv_rel_list in
        match new_tv_rel.type_sol with
        | SolNone -> ((acc_sol, acc_cons, TypeExp.TypeVarSet.union acc_useful sol_useful), tv_rel)
        | e -> (((new_tv_rel.type_var_idx, e) :: acc_sol, MemOffset.ConstraintSet.union acc_cons sol_cons, acc_useful), new_tv_rel)
      else
        (acc, tv_rel)
    in
    List.fold_left_map helper ([], MemOffset.ConstraintSet.empty, useful_set) tv_rel_list

  let update_type_var_rel (tv_rel: type_var_rel) (sol: TypeExp.type_var_id * TypeFullExp.type_sol) : type_var_rel =
    (* TODO: When replace variables with their solutions, we should consider about the effects of conditions!!! *)
    match tv_rel.type_sol with
    | SolNone -> { tv_rel with subtype_list = List.map (TypeFullExp.repl_type_sol sol) tv_rel.subtype_list }
    | _ -> tv_rel

  (* NOTE: This function is a bit weird, and we may need to improve this later *)
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
        | Some new_fe -> ((TypeBot, TypeFullExp.CondVarSet.empty), new_fe) (* Here is pretty weird *)
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

  let solve_vars (smt_ctx: SmtEmitter.t) (tv_rel_list: t) (cond_list: CondType.t list) (useful_var: TypeExp.TypeVarSet.t) (num_iter: int) : t * (CondType.t list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t =
    let rec helper 
        (tv_rel_list: t) 
        (cond_list: CondType.t list) 
        (cons: MemOffset.ConstraintSet.t) 
        (useful_var: TypeExp.TypeVarSet.t) 
        (num_iter: int) : 
        t * (CondType.t list) * MemOffset.ConstraintSet.t * TypeExp.TypeVarSet.t =
    if num_iter == 0 then (tv_rel_list, cond_list, cons, useful_var)
    else begin
      let (new_sol_list, new_cons_list, new_useful), new_tv_rel_list = try_solve_vars smt_ctx tv_rel_list cond_list useful_var in
      let final_cons_list = MemOffset.ConstraintSet.union cons new_cons_list in
      match new_sol_list with
      | [] -> (new_tv_rel_list, cond_list, final_cons_list, new_useful)
      | _ ->
        let update_tv_rel_list = List.map (fun (tv: type_var_rel) -> List.fold_left update_type_var_rel tv new_sol_list) new_tv_rel_list in
        (* TODO: It seems that for some case we need to update condition, while for other cases we should not??? *)
        let cond_list = List.map (fun (cond: CondType.t) -> List.fold_left CondType.repl_type_sol cond new_sol_list) cond_list in
        let merged_tv_rel_list = List.map simplify_tv_rel_sub update_tv_rel_list in
        helper merged_tv_rel_list cond_list final_cons_list new_useful (num_iter - 1)
    end
    in
    helper tv_rel_list cond_list MemOffset.ConstraintSet.empty useful_var num_iter

  let string_of_sol (sol: TypeExp.t option) =
    match sol with
    | None -> "None"
    | Some e -> "Some " ^ (TypeExp.to_string e)

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


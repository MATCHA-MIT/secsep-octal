(* Structure to hold subtype relations *)

open Code_type
open Pretty_print

module SubType = struct
  exception SubTypeError of string

  let sub_type_error msg = raise (SubTypeError ("[Sub Type Error] " ^ msg))

  type type_var_rel = {
    type_var_idx: CodeType.type_var_id;
    type_sol: CodeType.type_exp option;
    subtype_list: CodeType.type_full_exp list;
    supertype_list: (CodeType.Ints.t * CodeType.type_var_id) list; (* Entry (set, idx) refers to (TypeVar type_var_idx, set) -> (TypeVar idx, {}) *)
  }

  type t = type_var_rel list

  let init (total_var: int) =
    List.init total_var (fun x -> {type_var_idx = x; type_sol = None; subtype_list = []; supertype_list = []})

  let get_type_var_rel (tv_rel: t) (idx: int) : type_var_rel =
    List.find (fun x -> x.type_var_idx = idx) tv_rel

  let type_list_insert (type_list: CodeType.type_full_exp list) (ty: CodeType.type_full_exp) : CodeType.type_full_exp list =
    let t_exp, t_cond = ty in
    let helper (x: CodeType.type_full_exp) : (CodeType.type_full_exp, CodeType.type_full_exp) Either.t =
      let x_exp, x_cond = x in
      if (CodeType.cmp_type_exp x_exp t_exp) then Right (x_exp, CodeType.Ints.inter x_cond t_cond)
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

  let add_one_sub_type (tv_rel: type_var_rel) (ty: CodeType.type_full_exp) : type_var_rel =
    {tv_rel with subtype_list = type_list_insert tv_rel.subtype_list ty}
    (* {tv_rel with subtype_list = ty :: tv_rel.subtype_list} *)

  let type_var_list_insert 
      (type_var_list: (CodeType.Ints.t * CodeType.type_var_id) list) 
      (a_cond: CodeType.Ints.t) 
      (b_idx) : (CodeType.Ints.t * CodeType.type_var_id) list =
    let helper (x: CodeType.Ints.t * CodeType.type_var_id) : 
        (CodeType.Ints.t * CodeType.type_var_id, CodeType.Ints.t * CodeType.type_var_id) Either.t =
      let x_cond, x_idx = x in 
      if x_idx = b_idx then Right (CodeType.Ints.inter x_cond a_cond, x_idx)
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

  let add_one_super_type (tv_rel: type_var_rel) (a_cond: CodeType.Ints.t) (b_idx) : type_var_rel =
    {tv_rel with supertype_list = type_var_list_insert tv_rel.supertype_list a_cond b_idx}
    (* {tv_rel with supertype_list = (a_cond, b_idx) :: tv_rel.supertype_list} *)

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a: CodeType.type_full_exp) (b_idx: int) : t =
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

  let add_sub_sub_super (tv_rel: t) (a: CodeType.type_full_exp) (b_idx: int) : t =
    match a with
    | (TypeVar a_idx, a_cond) ->
      let a_rel = get_type_var_rel tv_rel a_idx in
      let new_tv_rel = List.fold_left 
                        (fun acc_tv_rel sub_a -> 
                          add_one_sub_super acc_tv_rel (CodeType.add_type_cond_set sub_a a_cond) b_idx) 
                        tv_rel a_rel.subtype_list in
      add_one_sub_super new_tv_rel a b_idx
    | _ -> add_one_sub_super tv_rel a b_idx
  
  let add_sub_sub_super_super (tv_rel: t) (a: CodeType.type_full_exp) (b_idx: int) : t =
    let b_rel = get_type_var_rel tv_rel b_idx in
    let new_tv_rel = List.fold_left
                      (fun acc_tv_rel (b_cond, super_b_idx) ->
                        add_sub_sub_super acc_tv_rel (CodeType.add_type_cond_set a b_cond) super_b_idx)
                      tv_rel b_rel.supertype_list in
    add_sub_sub_super new_tv_rel a b_idx
      
  let add_sub_type_full_exp (tv_rel: t) (a: CodeType.type_full_exp) (b: CodeType.type_full_exp) : t =
    match b with
    | (TypeVar b_idx, b_cond) ->
      if CodeType.Ints.is_empty b_cond then add_sub_sub_super_super tv_rel a b_idx
      else sub_type_error ("add_sub_type_full_exp: super type must has empty condition")
    | _ -> 
      let a_exp, _ = a in
      let b_exp, _ = b in
      if CodeType.cmp_type_exp a_exp b_exp then tv_rel (* Handle the special case for rsp *)
      else sub_type_error ("add_sub_type_full_exp: super type must be type var")

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
  
  let rec simplify_one_sub  
      (new_type_var_idx: CodeType.type_var_id) 
      (super_type_list: (CodeType.Ints.t * CodeType.type_var_id) list)
      (fe: CodeType.type_full_exp) : CodeType.type_full_exp =
    let e, old_cond = fe in
    match e with
    | TypeVar v ->
      begin match List.find_opt (fun (_, super_idx) -> super_idx = v) super_type_list with
      | Some (cond, _) -> (TypeVar new_type_var_idx, CodeType.Ints.union old_cond cond)
      | None -> fe
      end
    | TypeBExp (op, l, r) ->
      let l', cond_l = simplify_one_sub new_type_var_idx super_type_list (l, old_cond) in
      let r', cond_r = simplify_one_sub new_type_var_idx super_type_list (r, old_cond) in
      (TypeBExp (op, l', r'), CodeType.Ints.union cond_l cond_r)
    | TypeUExp (op, e) ->
      let e', cond_e = simplify_one_sub new_type_var_idx super_type_list (e, old_cond) in
      (TypeUExp (op, e'), cond_e)
    | _ -> fe

  let simplify_one_var (tv_rel: type_var_rel) : type_var_rel =
    let no_var_subtype = 
      List.filter (fun (exp: CodeType.type_full_exp) -> 
        match exp with
        | (TypeVar _, _)
        | (TypeBot, _ ) -> false
        | _ -> true) tv_rel.subtype_list in
    { tv_rel with subtype_list = List.map (simplify_one_sub tv_rel.type_var_idx tv_rel.supertype_list) no_var_subtype }
    (* {
      type_var_idx = tv_rel.type_var_idx;
      subtype_list = List.map (simplify_one_sub tv_rel.type_var_idx tv_rel.supertype_list) no_var_subtype;
      supertype_list = tv_rel.supertype_list
    } *)

  let simplify_all_var (tv_rel: t) : t =
    List.map simplify_one_var tv_rel

  let try_solve_single_sub_val (subtype_list: CodeType.type_full_exp list) : CodeType.type_exp option =
    match subtype_list with
    | hd :: [] ->
      let exp, _  = hd in
      if CodeType.is_type_exp_val exp then Some exp else None
    | _ -> None

  let try_solve_one_var (tv_rel: type_var_rel) : type_var_rel =
    let subtype_list = tv_rel.subtype_list in
    match try_solve_single_sub_val subtype_list with
    | Some e -> { tv_rel with type_sol = Some e }
    | None -> { tv_rel with type_sol = None } (* TODO Add more rules *)

  let try_solve_vars (tv_rel_list: t) : ((CodeType.type_var_id * CodeType.type_exp) list) * t =
    let helper (acc: (CodeType.type_var_id * CodeType.type_exp) list) (tv_rel: type_var_rel) : ((CodeType.type_var_id * CodeType.type_exp) list) * type_var_rel =
      let new_tv_rel = try_solve_one_var tv_rel in
      match new_tv_rel.type_sol with
      | Some e -> ((new_tv_rel.type_var_idx, e) :: acc, new_tv_rel)
      | None -> (acc, new_tv_rel)
    in
    List.fold_left_map helper [] tv_rel_list

  let update_type_var_rel (tv_rel: type_var_rel) (sol: CodeType.type_var_id * CodeType.type_exp) : type_var_rel =
    match tv_rel.type_sol with
    | Some _ -> tv_rel
    | None -> { tv_rel with subtype_list = List.map (CodeType.repl_type_full_exp sol) tv_rel.subtype_list }

  let rec solve_vars (tv_rel_list: t) (num_iter: int) : t =
    if num_iter == 0 then tv_rel_list
    else begin
      let new_sol_list, new_tv_rel_list = try_solve_vars tv_rel_list in
      match new_sol_list with
      | [] -> new_tv_rel_list
      | _ ->
        let update_tv_rel_list = List.map (fun (tv: type_var_rel) -> List.fold_left update_type_var_rel tv new_sol_list) new_tv_rel_list in
        solve_vars update_tv_rel_list (num_iter - 1)
    end

  let string_of_sol (sol: CodeType.type_exp option) =
    match sol with
    | None -> "None"
    | Some e -> "Some " ^ (CodeType.string_of_type_exp e)

  let pp_tv_rels (lvl: int) (tv_rels: t) =
    List.iter (fun x ->
      PP.print_lvl lvl "<TypeVar %d>\n" x.type_var_idx;
      PP.print_lvl (lvl + 1) "Solution: %s\n" (string_of_sol x.type_sol);
      PP.print_lvl (lvl + 1) "SubType: [\n";
      List.iter (fun tf -> 
        CodeType.pp_type_full_exp (lvl + 2) tf;
        Printf.printf ";\n";
      ) x.subtype_list;
      PP.print_lvl (lvl + 1) "]\n";
      PP.print_lvl (lvl + 1) "SuperType: [\n";
      List.iter (fun (cond, id) ->
        PP.print_lvl (lvl + 2) "(TypeVar: %d,\n" id;
        PP.print_lvl (lvl + 2) " Cond: %s);\n" (CodeType.string_of_cond_status cond);
      ) x.supertype_list;
      PP.print_lvl (lvl + 1) "]\n"
    ) tv_rels

end


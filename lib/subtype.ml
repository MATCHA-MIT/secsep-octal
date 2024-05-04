(* Structure to hold subtype relations *)

open Code_type
open Pretty_print

module SubType = struct
  exception SubTypeError of string

  let sub_type_error msg = raise (SubTypeError ("[Sub Type Error] " ^ msg))

  type type_var_rel = {
    type_var_idx: CodeType.type_var_id;
    subtype_list: CodeType.type_full_exp list;
    supertype_list: (CodeType.Ints.t * CodeType.type_var_id) list; (* Entry (set, idx) refers to (TypeVar type_var_idx, set) -> (TypeVar idx, {}) *)
  }

  type t = type_var_rel list

  let init (total_var: int) =
    List.init total_var (fun x -> {type_var_idx = x; subtype_list = []; supertype_list = []})

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
    | _ -> sub_type_error ("add_sub_type_full_exp: super type must be type var")

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

  let pp_tv_rels (lvl: int) (tv_rels: t) =
    List.iter (fun x ->
      PP.print_lvl lvl "<TypeVar %d>\n" x.type_var_idx;
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


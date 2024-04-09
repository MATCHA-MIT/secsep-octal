(* Structure to hold subtype relations *)

open Code_type

module SubType = struct
  exception SubTypeError of string

  let sub_type_error msg = raise (SubTypeError ("[Sub Type Error] " ^ msg))

  type type_var_rel = {
    type_var_idx: int;
    subtype_list: CodeType.type_full_exp list;
    supertype_list: (CodeType.Ints.t * int) list; (* Entry (set, idx) refers to (TypeVar type_var_idx, set) -> (TypeVar idx, {}) *)
  }

  type t = type_var_rel list

  let init (total_var: int) =
    List.init total_var (fun x -> {type_var_idx = x; subtype_list = []; supertype_list = []})

  let get_type_var_rel (tv_rel: t) (idx: int) : type_var_rel =
    List.find (fun x -> x.type_var_idx = idx) tv_rel

  let add_one_sub_type (tv_rel: type_var_rel) (t: CodeType.type_full_exp) : type_var_rel =
    {tv_rel with subtype_list = t :: tv_rel.subtype_list}

  let add_one_super_type (tv_rel: type_var_rel) (a_cond: CodeType.Ints.t) (b_idx) : type_var_rel =
    {tv_rel with supertype_list = (a_cond, b_idx) :: tv_rel.supertype_list}

  (* Connect a->b *)
  let add_one_sub_super (tv_rel: t) (a: CodeType.type_full_exp) (b_idx: int) : t =
    match a with
    | (TypeVar a_idx, a_cond) ->
      List.map (fun x -> 
                  if x.type_var_idx = a_idx then 
                    {x with supertype_list = (a_cond, b_idx) :: x.supertype_list}
                  else if x.type_var_idx = b_idx then
                    {x with subtype_list = a :: x.subtype_list}
                  else x) tv_rel
    | _ ->
      List.map (fun x -> if x.type_var_idx = b_idx then {x with subtype_list = a :: x.subtype_list} else x) tv_rel

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

end


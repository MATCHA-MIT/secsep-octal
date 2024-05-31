open Type_exp
open Type_full_exp
open Pretty_print

module CondType = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type t =
    | CondNe of (TypeExp.t * TypeExp.t * TypeFullExp.CondVarSet.t)
    | CondEq of (TypeExp.t * TypeExp.t * TypeFullExp.CondVarSet.t)
    | CondLq of (TypeExp.t * TypeExp.t * TypeFullExp.CondVarSet.t)
    | CondLe of (TypeExp.t * TypeExp.t * TypeFullExp.CondVarSet.t)

  let not_cond_type (cond: t) : t = 
    match cond with
    | CondNe (l, r, c) -> CondEq (l, r, c)
    | CondEq (l, r, c) -> CondNe (l, r, c)
    | CondLq (l, r, c) -> CondLq (r, l, c)
    | CondLe (l, r, c) -> CondLe (r, l, c)

  let get_cond_idx (idx: int) (taken: bool) : int =
    if taken then idx * 2 + 1
    else idx * 2

  let parse_cond_idx (cond_idx: int) : int * bool =
    (cond_idx / 2, Int.rem cond_idx 2 = 1)
  
  let get_cond_type (cond_list: t list) (cond_idx: int) : t =
    let idx, taken = parse_cond_idx cond_idx in
    (* let idx = cond_idx / 2 in *)
    let cond = List.nth cond_list ((List.length cond_list) - idx) in
    (* let taken = Int.rem cond_idx 2 in *)
    if taken then cond
    else not_cond_type cond
  
  let add_cond_type (cond_list: t list) (cond: t) (taken: bool) : (t list) * int =
    (* let cond_suffix = if taken then 1 else 0 in
    (cond::cond_list, ((List.length cond_list) + 1) * 2 + cond_suffix) *)
    (cond::cond_list, get_cond_idx ((List.length cond_list) + 1) taken)

  let repl_type_sol (e: t) (sol: TypeExp.type_var_id * TypeFullExp.type_sol) : t =
    let helper (e1: TypeExp.t) (e2: TypeExp.t) (c: TypeFullExp.CondVarSet.t) : (TypeExp.t * TypeExp.t) =
      let ee1, _ = TypeFullExp.repl_type_sol sol (e1, c) in
      let ee2, _ = TypeFullExp.repl_type_sol sol (e2, c) in
      (ee1, ee2)
    in
    match e with
    | CondNe (e1, e2, c) -> let ee1, ee2 = helper e1 e2 c in CondNe (ee1, ee2, c)
    | CondEq (e1, e2, c) -> let ee1, ee2 = helper e1 e2 c in CondEq (ee1, ee2, c)
    | CondLq (e1, e2, c) -> let ee1, ee2 = helper e1 e2 c in CondLq (ee1, ee2, c)
    | CondLe (e1, e2, c) -> let ee1, ee2 = helper e1 e2 c in CondLe (ee1, ee2, c)

  let pp_cond (lvl: int) (cond: t) =
    let op, str1, str2 = match cond with
    | CondNe (l, r, _) -> ("Ne", TypeExp.string_of_type_exp l, TypeExp.string_of_type_exp r)
    | CondEq (l, r, _) -> ("Eq", TypeExp.string_of_type_exp l, TypeExp.string_of_type_exp r)
    | CondLq (l, r, _) -> ("Lq", TypeExp.string_of_type_exp l, TypeExp.string_of_type_exp r)
    | CondLe (l, r, _) -> ("Le", TypeExp.string_of_type_exp l, TypeExp.string_of_type_exp r)
    in
    PP.print_lvl lvl "Cond %s between\n" op;
    PP.print_lvl (lvl + 1) "%s\n" str1;
    PP.print_lvl (lvl + 1) "%s\n" str2

  let pp_cond_list (lvl: int) (cond_list: t list) =
    List.iteri (fun i x -> 
      PP.print_lvl lvl "<Cond %d>\n" (i + 1);
      pp_cond (lvl + 1) x
    ) (List.rev cond_list)

end

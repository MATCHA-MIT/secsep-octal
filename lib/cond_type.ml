open Type_exp
open Type_full_exp
open Pretty_print

module CondType = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type t =
    | CondNe of (TypeFullExp.t * TypeFullExp.t)
    | CondEq of (TypeFullExp.t * TypeFullExp.t)
    | CondLq of (TypeFullExp.t * TypeFullExp.t)
    | CondLe of (TypeFullExp.t * TypeFullExp.t)

  let not_cond_type (cond: t) : t = 
    match cond with
    | CondNe (l, r) -> CondEq (l, r)
    | CondEq (l, r) -> CondNe (l, r)
    | CondLq (l, r) -> CondLq (r, l)
    | CondLe (l, r) -> CondLe (r, l)
  
  let get_cond_type (cond_list: t list) (cond_idx: int) : t =
    let idx, taken = TypeFullExp.parse_cond_idx cond_idx in
    (* let idx = cond_idx / 2 in *)
    let cond = List.nth cond_list ((List.length cond_list) - idx) in
    (* let taken = Int.rem cond_idx 2 in *)
    if taken then cond
    else not_cond_type cond
  
  let add_cond_type (cond_list: t list) (cond: t) (taken: bool) : (t list) * int =
    (* let cond_suffix = if taken then 1 else 0 in
    (cond::cond_list, ((List.length cond_list) + 1) * 2 + cond_suffix) *)
    (cond::cond_list, TypeFullExp.get_cond_idx ((List.length cond_list) + 1) taken)

  let pp_cond (lvl: int) (cond: t) =
    let op, str1, str2 = match cond with
    | CondNe (l, r) -> ("Ne", TypeExp.string_of_type_exp (fst l), TypeExp.string_of_type_exp (fst r))
    | CondEq (l, r) -> ("Eq", TypeExp.string_of_type_exp (fst l), TypeExp.string_of_type_exp (fst r))
    | CondLq (l, r) -> ("Lq", TypeExp.string_of_type_exp (fst l), TypeExp.string_of_type_exp (fst r))
    | CondLe (l, r) -> ("Le", TypeExp.string_of_type_exp (fst l), TypeExp.string_of_type_exp (fst r))
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

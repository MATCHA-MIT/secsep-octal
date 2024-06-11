open Type_exp
open Pretty_print

module TypeFullExp = struct
  exception TypeFullExpError of string
  let type_full_exp_error msg = raise (TypeFullExpError ("[Type Full Exp Error] " ^ msg))

  module CondVarSet = Set.Make(Int)

  type t = TypeExp.t * CondVarSet.t

  type type_sol =
  | SolNone
  | SolSimple of TypeExp.t
  | SolCond of TypeExp.t * int * TypeExp.t * TypeExp.t
    (* sol, cond, sol when taken, sol when not taken *)

  let get_cond_idx (idx: int) (taken: bool) : int =
    if taken then idx * 2 + 1
    else idx * 2

  let parse_cond_idx (cond_idx: int) : int * bool =
    (cond_idx / 2, Int.rem cond_idx 2 = 1)

  let merge (fe1: t) (fe2: t) : t option =
    let e1, cond1 = fe1 in
    let e2, cond2 = fe2 in
    match TypeExp.merge e1 e2 with
    | Some e -> Some (e, CondVarSet.inter cond1 cond2)
    | None -> None

  let repl_type_full_exp (sol: TypeExp.type_var_id * TypeExp.t) (fe: t) : t =
    let e, cond = fe in
    let new_e = TypeExp.repl_type_exp sol e in (new_e, cond)

  let repl_type_sol (sol: TypeExp.type_var_id * type_sol) (fe: t) : t =
    let idx, s = sol in
    let e, cond = fe in
    match s with
    | SolNone -> fe
    | SolSimple sol_e -> (TypeExp.repl_type_exp (idx, sol_e) e, cond)
    | SolCond (sol_e, pure_cond_idx, sol_taken, sol_not_taken) ->
      begin match CondVarSet.find_opt (get_cond_idx pure_cond_idx true) cond with 
      | Some _ -> (TypeExp.repl_type_exp (idx, sol_taken) e, cond) (* Taken *)
      | None ->
        begin match CondVarSet.find_opt (get_cond_idx pure_cond_idx false) cond with
        | Some _ -> (TypeExp.repl_type_exp (idx, sol_not_taken) e, cond) (* Not Taken *)
        | None -> (TypeExp.repl_type_exp (idx, sol_e) e, cond) (* No branch *)
        end
      end

  let repl_all_sol (sol: (TypeExp.type_var_id * type_sol) list) (fe: t) : t =
    let e, cond = fe in
    (* Trick: Use sub-function helper can reduce times of copying large objects!!! *)
    let rec helper (e: TypeExp.t) : TypeExp.t =
      match e with
      | TypeVar v ->
        let sol_opt = List.find_opt (fun (idx, _) -> idx = v) sol in
        begin match sol_opt with
        | Some sol -> let new_ve, _ = repl_type_sol sol (TypeVar v, cond) in new_ve
        | None -> e
        end
      | TypeBExp (bop, e1, e2) -> TypeBExp (bop, helper e1, helper e2)
      | TypeUExp (uop, e) -> TypeUExp (uop, helper e)
      | TypePtr (e, size) -> TypePtr (helper e, size)
      | _ -> e
    in
    (TypeExp.eval (helper (helper e)), cond)

  let add_type_cond (tf: t) (cond_idx: int) : t =
    let t, cond = tf in (t, CondVarSet.add cond_idx cond)
  
  let add_type_cond_set (tf: t) (cond_set: CondVarSet.t) : t =
    let t, cond = tf in (t, CondVarSet.union cond_set cond)

  let string_of_type_sol (t: type_sol) =
    match t with
    | SolSimple e -> "SolSimple " ^ (TypeExp.to_string e)
    | SolCond (e, c, e1, e2) -> "SolCond " ^ (TypeExp.to_string e) ^ " cond " ^ (string_of_int c) ^ " taken " ^ (TypeExp.to_string e1) ^ " not taken " ^ (TypeExp.to_string e2)
    | SolNone -> "SolNone"

  let string_of_one_cond_status (cond: int) =
    let cond_idx = cond / 2 in
    let taken = if Int.rem cond 2 = 1 then "Taken" else "NotTaken" in
    string_of_int cond_idx ^ " " ^ taken

  let string_of_cond_status (conds: CondVarSet.t) =
    "[" ^ (CondVarSet.fold (fun x acc -> (if acc = "" then "" else acc ^ ", ") ^ (string_of_one_cond_status x)) conds "") ^ "]"

  let pp_type_full_exp (lvl: int) (tf: t) =
    let t, cond = tf in
    PP.print_lvl lvl "(Type = %s, Cond = %s)"
      (TypeExp.to_string t)
      (string_of_cond_status cond)

end

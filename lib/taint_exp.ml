module TaintExp = struct
  exception TaintExpError of string
  let taint_exp_error msg = raise (TaintExpError ("[Taint Type Error] " ^ msg))

  type taint_var_id = int
  module TaintVarSet = Set.Make(Int)

  type t = 
    | TaintConst of bool
    | TaintVar of taint_var_id
    | TaintExp of TaintVarSet.t

  type local_var_map_t = (taint_var_id * t) list

  let cmp (e1: t) (e2: t) : int =
    match e1, e2 with
    | TaintConst b1, TaintConst b2 -> compare b1 b2
    | TaintConst _, _ -> -1
    | TaintVar _, TaintConst _ -> 1
    | TaintVar v1, TaintVar v2 -> compare v1 v2
    | TaintVar _, _ -> -1
    | TaintExp s1, TaintExp s2 -> compare s1 s2
    | TaintExp _, _ -> 1

  let eval (e: t) : t =
    match e with
    | TaintExp s ->
      let s_list = TaintVarSet.elements s in
      begin match s_list with
      | [] -> TaintConst false
      | hd :: [] -> TaintVar hd
      | _ -> e
      end
    | _ -> e

  let merge (e1: t) (e2: t) : t =
    match e1, e2 with
    | TaintConst true, _
    | _, TaintConst true -> TaintConst true
    | TaintConst false, _ -> e2
    | _, TaintConst false -> e1
    | TaintVar v1, TaintVar v2 ->
      if v1 = v2 then TaintVar v1
      else TaintExp (TaintVarSet.of_list [ v1; v2 ])
    | TaintVar v, TaintExp s
    | TaintExp s, TaintVar v -> TaintExp (TaintVarSet.add v s)
    | TaintExp s1, TaintExp s2 -> TaintExp (TaintVarSet.union s1 s2)

  let merge_opt (e1: t option) (e2: t option) : t =
    match e1, e2 with
    | Some e1, Some e2 -> merge e1 e2
    | Some e, None | None, Some e -> e
    | None, None -> TaintConst false

  let merge_all (e_list: t list) : t =
    List.fold_left merge (TaintConst false) e_list

  let repl (sol: taint_var_id * t) (e: t) : t =
    let sol_id, sol_v = sol in
    match e with
    | TaintConst _ -> e
    | TaintVar id -> if sol_id = id then sol_v else e
    | TaintExp s ->
      if TaintVarSet.find_opt sol_id s = None then e
      else
        let remove_s = TaintVarSet.remove sol_id s in
        eval (merge sol_v (TaintExp remove_s))

  let partial_read_val (e: t) : t = e
  let partial_write_val (orig_e: t) (write_e: t) : t = merge orig_e write_e
  let next_var (e: t) : t =
    match e with
    | TaintVar x -> TaintVar (x + 1)
    | _ -> taint_exp_error "next_var should only be called on taint var"

  let to_string (e: t) : string =
    match e with
    | TaintConst true -> "Taint T"
    | TaintConst false -> "Taint UT"
    | TaintVar x -> "TaintVar " ^ (string_of_int x)
    | TaintExp s ->
      let var_str_list = List.map string_of_int (TaintVarSet.elements s) in
      "TaintVarSet (" ^ (String.concat " " var_str_list) ^ ")"
end

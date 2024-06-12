module TaintExp = struct
  exception TaintExpError of string
  let taint_exp_error msg = raise (TaintExpError ("[Taint Type Error] " ^ msg))

  type taint_var_id = int
  module TaintVarSet = Set.Make(Int)

  type t = 
    | TaintConst of bool
    | TaintVar of taint_var_id
    | TaintExp of TaintVarSet.t

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
      else
        TaintExp (TaintVarSet.union (TaintVarSet.singleton v1) (TaintVarSet.singleton v2))
    | TaintVar v, TaintExp s
    | TaintExp s, TaintVar v -> TaintExp (TaintVarSet.add v s)
    | TaintExp s1, TaintExp s2 -> TaintExp (TaintVarSet.union s1 s2)

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

module MemDepExp = struct
  exception MemDepExpError of string
  let mem_dep_exp_error msg = raise (MemDepExpError ("[Mem Dep Exp Error] " ^ msg))

  type pc = int * int (* #block * #inst in the block *)
  type mem_dep_var_id = int
  
  let cmp_pc (pc1: pc) (pc2: pc) : int =
    let l1, r1 = pc1 in
    let l2, r2 = pc2 in
    if l1 < l2 then -1
    else if l1 > l2 then 1
    else
      if r1 < r2 then -1
      else if r1 > r2 then 1
      else 0

  module PCSet = Set.Make(
    struct
      let compare = cmp_pc
      type t = pc
    end
  )

  module MemDepVarSet = Set.Make(Int)

  type t =
    | MemDepSet of PCSet.t
    | MemDepVar of mem_dep_var_id
    | MemDepExp of MemDepVarSet.t * PCSet.t

  let eval (e: t) : t =
    match e with
    | MemDepSet _ | MemDepVar _ -> e
    | MemDepExp (sx, s) ->
      if MemDepVarSet.is_empty sx then MemDepSet s
      else if PCSet.is_empty s then begin
        match MemDepVarSet.elements sx with
        | [] -> mem_dep_exp_error "[eval] MemDepVarSet should not be empty here"
        | hd :: [] -> MemDepVar hd
        | _ -> e
      end else e

  let merge (e1: t) (e2: t) : t =
    match e1, e2 with
    | MemDepSet s1, MemDepSet s2 -> MemDepSet (PCSet.union s1 s2)
    | MemDepSet s, MemDepVar x 
    | MemDepVar x, MemDepSet s -> MemDepExp ((MemDepVarSet.singleton x), s)
    | MemDepSet s1, MemDepExp (sx, s2)
    | MemDepExp (sx, s1), MemDepSet s2 -> MemDepExp (sx, PCSet.union s1 s2)
    | MemDepVar x1, MemDepVar x2 -> 
      MemDepExp ((MemDepVarSet.add x2 (MemDepVarSet.singleton x1)), PCSet.empty)
    | MemDepVar x, MemDepExp (sx, s)
    | MemDepExp (sx, s), MemDepVar x -> MemDepExp (MemDepVarSet.add x sx, s)
    | MemDepExp (sx1, s1), MemDepExp (sx2, s2) ->
      MemDepExp (MemDepVarSet.union sx1 sx2, PCSet.union s1 s2)

  let repl (sol: mem_dep_var_id * t) (e: t) : t =
    let sol_id, sol_v = sol in
    match e with
    | MemDepSet _ -> e
    | MemDepVar x ->
      if x = sol_id then sol_v else e
    | MemDepExp (sx, s) ->
      if MemDepVarSet.find_opt sol_id sx = None then e
      else
        let remove_sx = MemDepVarSet.remove sol_id sx in
        eval (merge sol_v (MemDepExp (remove_sx, s)))

  let partial_read_val (e: t) : t = e
  let partial_write_val (orig_e: t) (write_e: t) : t = merge orig_e write_e
  let next_var (e: t) : t =
    match e with
    | MemDepVar x -> MemDepVar (x + 1)
    | _ -> mem_dep_exp_error "next_var should only be called on mem dep var"

  let to_string (e: t) : string =
    let helper (x: pc) : string =
      let label, idx = x in
      "(" ^ (string_of_int label) ^ "-" ^ (string_of_int idx) ^ ")"
    in
    match e with
    | MemDepSet s ->
      "MemDepSet " ^ (String.concat " " (List.map helper (PCSet.elements s)))
    | MemDepVar x -> "MemDepVar " ^ (string_of_int x)
    | MemDepExp (sx, s) ->
      "MemDepExp (" ^ (String.concat " " (List.map string_of_int (MemDepVarSet.elements sx)))
      ^ ") (" ^ (String.concat " " (List.map helper (PCSet.elements s))) ^ ")"
end

module TaintDepExp = struct
  exception TaintDepExpError of string
  let taint_dep_exp_error msg = raise (TaintDepExpError ("[Taint Dep Exp Error] " ^ msg))

  type t = TaintExp.t * MemDepExp.t

  let partial_read_val (e: t) : t =
    let e1, e2 = e in
    (TaintExp.partial_read_val e1, MemDepExp.partial_read_val e2)

  let partial_write_val (orig_e: t) (new_e: t) : t =
    let orig_e1, orig_e2 = orig_e in
    let new_e1, new_e2 = new_e in
    (TaintExp.partial_write_val orig_e1 new_e1,
    MemDepExp.partial_write_val orig_e2 new_e2)

  let next_var (e: t) : t =
    let e1, e2 = e in
    (TaintExp.next_var e1, MemDepExp.next_var e2)

  let to_string (e: t) : string =
    let taint, mem_dep = e in
    (TaintExp.to_string taint) ^ " " ^ (MemDepExp.to_string mem_dep)
end

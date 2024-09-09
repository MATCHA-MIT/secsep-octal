open Pretty_print

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

  let update_local_var (map: local_var_map_t) (e: t) (pc: int) : (local_var_map_t * t) =
    let new_idx = -pc in
    (new_idx, e) :: map, TaintVar new_idx

  let add_local_var (map: local_var_map_t) (e1: t) (e2: t) : local_var_map_t =
    match e1 with
    | TaintVar v ->
      begin match List.find_opt (fun (idx, _) -> idx = v) map with
      | Some (_, e) ->
        if cmp e e2 = 0 then map
        else 
          taint_exp_error 
            (Printf.sprintf "add_local var conflict on var %d exp %s and %s" v (to_string e) (to_string e2))
      | None -> (v, e2) :: map
      end
    | TaintConst _ ->
      if cmp e1 e2 = 0 then map
      else taint_exp_error (Printf.sprintf "add_local_var cannot add %s->%s" (to_string e1) (to_string e2))
    | _ -> taint_exp_error (Printf.sprintf "add_local_var cannot add %s->%s" (to_string e1) (to_string e2))

  let pp_local_var (lvl: int) (map: local_var_map_t) : unit =
    PP.print_lvl lvl "<Taint var map>\n";
    List.iter (
      fun (x, e) -> PP.print_lvl (lvl + 1) "%d -> %s\n" x (to_string e)
    ) map

  let find_local_var_map (map: local_var_map_t) (idx: int) : t option =
    List.find_map (fun (i, e) -> if i = idx then Some e else None) map

  let repl_local_var (map: local_var_map_t) (e: t) : t =
    (* Only applied to repl local var with block vars *)
    (* Recursive repl; if not found, then just leave the var there *)
    let rec repl_helper (e: t) : t =
      match e with
      | TaintConst _ -> e
      | TaintVar v ->
        if v > 0 then (* input/block var *)
          e (* Here is dirty since it will also lookup global var *)
        else begin
          match find_local_var_map map v with
          | Some e -> repl_helper e
          | None -> e
        end
      | TaintExp var_set ->
        List.fold_left (
          fun (acc: t) (entry: taint_var_id) ->
            merge acc (repl_helper (TaintVar entry))
        ) (TaintConst false) (TaintVarSet.to_list var_set)
    in
    repl_helper e

  let repl_context_var (map: local_var_map_t) (e: t) : t =
      (* Only applied to repl var from one context with var from the other context *)
      (* Non-recursive repl; if not found, raise exception!!! *)
    let rec repl_helper (e: t) : t =
      match e with
      | TaintConst _ -> e
      | TaintVar v ->
        begin match find_local_var_map map v with
        | Some e -> e
        | None -> taint_exp_error (Printf.sprintf "repl_context_var cannot find var %d in context" v)
        end
      | TaintExp var_set ->
        List.fold_left (
          fun (acc: t) (entry: taint_var_id) ->
            merge acc (repl_helper (TaintVar entry))
        ) (TaintConst false) (TaintVarSet.to_list var_set)
    in
    repl_helper e
    
  let is_val (var_set: TaintVarSet.t) (e: t) : bool =
    match e with
    | TaintConst _ -> true
    | TaintVar v -> TaintVarSet.mem v var_set
    | TaintExp s -> TaintVarSet.subset s var_set
  
  let is_val2 (map: local_var_map_t) (e: t) : bool =
    let var_set =
      List.fold_left (
        fun (acc: TaintVarSet.t) (x, _) ->
          TaintVarSet.add x acc
      ) TaintVarSet.empty map
    in
    is_val var_set e

end

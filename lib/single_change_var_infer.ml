open Isa_basic
open Single_exp
open Range_exp
open Reg_range
open Single_subtype
open Single_type_infer
open Taint_type_infer
open Set_sexp


module SingleChangeVarInfer = struct
  exception SingleChangeVarInferError of string
  let single_change_var_infer_error msg = raise (SingleChangeVarInferError ("[Single Change Var Infer Error] " ^ msg))

  module VarMap = IntVarMap

  (* type var_change_type_t =
  | NonChange
  | ChangeBase of int
  | ChangeNoBase
  [@@deriving sexp]

  module ChangeMap = IntMapSexp (
    struct
      type t = var_change_type_t
      [@@deriving sexp]
    end
  ) *)

  type t = {
    (* input_var_set: IntSet.t; *)
    non_change_set: IntSet.t;
    change_ptr_set: IntSet.t;
    change_ptr_base_map: VarMap.t; 
    (* record var and its base ptr where we want to assert var-ptr is non change *)
  }
  [@@deriving sexp]

  type type_rel = SingleSubtype.type_rel
  [@@deriving sexp]

  let get_default () = {
    non_change_set = IntSet.empty;
    change_ptr_set = IntSet.empty;
    change_ptr_base_map = VarMap.empty;
  }

  let is_non_change_exp_naive (var_info: t) (e: SingleExp.t) : bool =
    if e = SingleTop then true else
    let v_set = SingleExp.get_vars e in
    IntSet.subset v_set var_info.non_change_set

  let find_base_ptr (var_info: t) (e: SingleExp.t) : int option option =
    let p_set = SingleExp.filter_single_var e in
    match IntSet.inter var_info.change_ptr_set p_set |> IntSet.to_list with
    | [] -> None (* Cannot find a base of change var *)
    | hd :: [] -> Some (VarMap.find_opt hd var_info.change_ptr_base_map)
    (* Left None -> is change var, but cannot determine the root ptr 
       Right (Some ptr) -> is change var and the "base" is ptr *)
    | _ -> None
      (* single_change_var_infer_error "has multiple bases, cannot infer" *)

  let find_e_list_some_change (var_info: t) (e_list: SingleExp.t list) : bool =
    List.fold_left (
      fun acc e ->
        match find_base_ptr var_info e with
        | Some _ -> true
        | _ -> acc
    ) false e_list

  let find_e_list_base_ptr (var_info: t) (e_list: SingleExp.t list) : int option option =
    match e_list with
    | [] -> None
    | _ ->
      let p_set = List.fold_left (fun acc e -> IntSet.inter acc (SingleExp.filter_single_var e)) var_info.change_ptr_set e_list in
      match IntSet.to_list p_set with
      | [] -> None
      | hd :: [] -> Some (VarMap.find_opt hd var_info.change_ptr_base_map)
      | _ -> None

  let find_range_base_ptr (var_info: t) (r: RangeExp.t) : bool * (int option option) =
    match r with
    | Single e ->
      if is_non_change_exp_naive var_info e then true, None
      else false, find_base_ptr var_info e
    | Range (e1, e2, _) ->
      if is_non_change_exp_naive var_info e1 || is_non_change_exp_naive var_info e2 then true, None
      else begin
        match find_base_ptr var_info e1 with
        | Some p_opt -> false, Some p_opt
        | None -> false, find_base_ptr var_info e2
      end
    | SingleSet e_list ->
      let is_non_change = List.find_opt (
        fun e -> not (is_non_change_exp_naive var_info e)
      ) e_list = None
      in
      if is_non_change then true, None else
      let has_some_ptr = find_e_list_some_change var_info e_list in
      let ptr_opt_opt = find_e_list_base_ptr var_info e_list in
      let ptr =
        match ptr_opt_opt with
        | Some _ -> ptr_opt_opt
        | _ -> if has_some_ptr then Some None else None
      in
      false, ptr
    | Top -> true, None

  let find_base_ptr (var_info: t) (tv_rel: type_rel) : t * (type_rel option) =
    let var_idx, _ = tv_rel.var_idx in
    let is_non_change, ptr_opt =
      match tv_rel.sol with
      | SolNone -> false, None
      | SolSimple r
      | SolCond (_, r, _, _) -> find_range_base_ptr var_info r
    in
    if is_non_change then
      { var_info with non_change_set = IntSet.add var_idx var_info.non_change_set }, None
    else begin
      match ptr_opt with
      | None -> var_info, Some tv_rel
      | Some None -> (* change, but no base *)
        { var_info with
          change_ptr_set = IntSet.add var_idx var_info.change_ptr_set },
        None
      | Some Some ptr ->
        { var_info with
          change_ptr_set = IntSet.add var_idx var_info.change_ptr_set;
          change_ptr_base_map = VarMap.add var_idx ptr var_info.change_ptr_base_map },
        None
    end

  let filter_ptr_base_map (var_info: t) (tv_rel_list: type_rel list) : t =
    (* Heuristic: I want to reduce extra offset non change assertions I made, so that I can avoid false assertion as much as possible.
      If a var's sol is single, we do not need to assert var-ptr is non change since it that is true, it is already implied by its sol. *)
    (* However, better heuristic might be need to avoid over-assert var-ptr is non change (adding incorrect pairs to change_ptr_base_map) 
      for cases not covered by our benchmarks. *)
    let non_single_sol_var =
      List.filter_map (
        fun (tv_rel: type_rel) ->
          match tv_rel.sol with
          | SolSimple (Single _) -> None
          | _ -> Some (fst tv_rel.var_idx) 
      ) tv_rel_list |> IntSet.of_list
    in
    let change_ptr_base_map =
      VarMap.filter (
        fun v _ -> IntSet.mem v non_single_sol_var
      ) var_info.change_ptr_base_map
    in
    { var_info with change_ptr_base_map = change_ptr_base_map }

  let find_base_ptr (func_name: string) (var_info: t) (tv_rel_list: type_rel list) : t =
    let rec helper (var_info: t) (tv_rel_list: type_rel list) : t * (type_rel list) =
      let var_info, tv_rel_opt_list = List.fold_left_map find_base_ptr var_info tv_rel_list in
      let new_tv_rel_list = List.filter_map (fun x -> x) tv_rel_opt_list in
      if List.is_empty new_tv_rel_list || List.length new_tv_rel_list = List.length tv_rel_list then
        var_info, new_tv_rel_list
      else helper var_info new_tv_rel_list
    in
    let var_info, remain_tv_rel_list = helper var_info tv_rel_list in
    let var_info = filter_ptr_base_map var_info tv_rel_list in
    Printf.printf "Infer func %s\n" func_name;
    Printf.printf "Change Var Info\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t var_info));
    Printf.printf "remain tv_rel_list\n%s\n" (Sexplib.Sexp.to_string_hum (SingleSubtype.sexp_of_t remain_tv_rel_list));
    if not (List.is_empty remain_tv_rel_list) then Printf.printf "fail to infer %s\n" func_name;
    var_info

  let init_var_info (state: SingleTypeInfer.t) : t =
    let ptr_set = 
      SingleTypeInfer.ArchType.MemType.get_ptr_set (List.hd state.func_type).mem_type 
      |> IntSet.filter (fun x -> x >= 0)
    in
    let callee_saved_var_set = 
      List.filteri 
        (fun i _ -> i <> IsaBasic.rsp_idx && IsaBasic.is_reg_idx_callee_saved i) 
        (List.hd state.func_type).reg_type
      |> List.filter_map (
        fun (_, e) -> 
          match e with
          | SingleExp.SingleVar v -> Some v
          | _ -> None
      ) |> IntSet.of_list
    in
    let invalid_var_set =
      List.filter_map (
        fun (valid, e) ->
          match valid, e with
          | RegRange.RangeConst (0L, 0L), SingleExp.SingleVar v -> Some v
          | _ -> None
      ) (List.hd state.func_type).reg_type
      |> IntSet.of_list
    in
    let ptr_set = IntSet.union ptr_set callee_saved_var_set |> IntSet.union invalid_var_set in
    {
      non_change_set = IntSet.diff state.input_var_set ptr_set;
      change_ptr_set = ptr_set;
      change_ptr_base_map = List.map (fun x -> x, x) (IntSet.to_list ptr_set) |> VarMap.of_list;
    }

  let infer (single_infer_state_list: SingleTypeInfer.t list) : t list =
    List.map (
      fun (s: SingleTypeInfer.t) -> 
        find_base_ptr s.func_name (init_var_info s) s.single_subtype
    ) single_infer_state_list

  let update_arch_type 
      (var_info: t) (input_var_set: IntSet.t) 
      (a_type_list: TaintTypeInfer.ArchType.t list) : TaintTypeInfer.ArchType.t list =
    List.map (
      fun (a_type: TaintTypeInfer.ArchType.t) ->
        let change_var_set =
          IntSet.diff 
            (IntSet.inter var_info.change_ptr_set (IntSet.union input_var_set a_type.useful_var)
            |> IntSet.union (fst a_type.change_var))
            var_info.non_change_set
        in
        let change_var_ptr_map =
          VarMap.filter (fun v ptr -> IntSet.mem v change_var_set && v <> ptr) var_info.change_ptr_base_map
        in
        { a_type with 
          change_var = change_var_set, change_var_ptr_map }
    ) a_type_list

end

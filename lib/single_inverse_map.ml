open Isa_basic
open Single_exp
open Set_sexp

module SingleInverseMap = struct
  exception SingleInverseMapError of string

  let single_inverse_map_error msg = raise (SingleInverseMapError ("[Single Inverse Map Error] " ^ msg))

  type var_id = IsaBasic.imm_var_id

  type exp_t = SingleExp.t
  [@@deriving sexp]

  type t = SingleExp.local_var_map_t
  [@@deriving sexp]

  let var_inverse (target_var_set: IntSet.t) (v_exp: var_id * exp_t) : (var_id * exp_t) option =
    let v1, e2 = v_exp in
    match e2 with
    | SingleVar v2 -> 
      if IntSet.mem v2 target_var_set then
        Some (v2, SingleVar v1)
      else None
    | _ -> None

  let var_const_inverse (target_var_set: IntSet.t) (v_exp: var_id * exp_t) : (var_id * exp_t) option =
    let v1, e2 = v_exp in
    match e2 with
    | SingleBExp (SingleAdd, SingleVar v2, SingleConst c) ->
      if IntSet.mem v2 target_var_set then
        Some (v2, SingleBExp (SingleSub, SingleVar v1, SingleConst c) |> SingleExp.eval)
      else None
    | _ -> None

  let exp_inverse (context_map: t) (target_var_set: IntSet.t) (v_exp: var_id * exp_t) : (var_id * exp_t) option =
    (* Note for all var in e2, it should satisfies one of the following conditions:
       1. it is a global or input var
       2. it is in context_map
       3. it is in target_var_set *)
    (* TODO: Here we handle the case where v1 -> v2 + e2_shift,
       we should also support the pattern for v1 -> e2_shift - v2 by doing filter_single_var on neg e2 *)
    let v1, e2 = v_exp in
    let base_var_set = SingleExp.filter_single_var e2 in
    let base_candidate_list = IntSet.inter target_var_set base_var_set |> IntSet.to_list in
    match base_candidate_list with
    | [ v2 ] ->
      let e2_shift = SingleBExp (SingleSub, e2, SingleVar v2) |> SingleExp.eval in
      let e2_shift_target_var = SingleExp.get_vars e2_shift |> IntSet.inter target_var_set in
      if IntSet.is_empty e2_shift_target_var then
        let e2_shift_map_exp = SingleExp.repl_var context_map e2_shift in
        Some (
          v2,
          SingleBExp (SingleSub, SingleVar v1, e2_shift_map_exp) |> SingleExp.eval
        )
      else None
    | _ -> None

  let get_inverse_context_map (input_var_set: IntSet.t) (context_map: t) : t =
    (* context_map: ctx1 -> ctx2 *)
    (* 1. Get all local vars in ctx2 *)
    let ctx2_target_var_set =
      IntSet.diff
      (List.fold_left (
        fun acc (_, exp) -> 
          SingleExp.get_vars exp |> IntSet.union acc
      ) IntSet.empty context_map) input_var_set
    in
    Printf.printf "context_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t context_map));
    Printf.printf "ctx2_target_var_set\n%s\n" (Sexplib.Sexp.to_string_hum (IntSet.sexp_of_t ctx2_target_var_set));
    (* 2. Apply var_inverse and var_const_inverse to get naive map *)
    let simple_inverse_helper
        (inverse_func: IntSet.t -> var_id * exp_t -> (var_id * exp_t) option)
        (acc: IntSet.t) (v_exp: var_id * exp_t) : 
        IntSet.t * (var_id * exp_t, var_id * exp_t) Either.t =
      match inverse_func acc v_exp with
      | Some (new_v, new_exp) -> 
        IntSet.remove new_v acc, Left (new_v, new_exp)
      | None -> acc, Right v_exp
    in
    let ctx2_target_var_set, result_map = 
      List.fold_left_map (simple_inverse_helper var_inverse) 
        ctx2_target_var_set context_map
    in
    let reverse_map1, context_map = List.partition_map (fun x -> x) result_map in
    let ctx2_target_var_set, result_map =
      List.fold_left_map (simple_inverse_helper var_const_inverse)
        ctx2_target_var_set context_map
    in
    let reverse_map2, context_map = List.partition_map (fun x -> x) result_map in
    let reverse_map = reverse_map1 @ reverse_map2 in
    (* 3. Apply exp_inverse to get more map *)
    let adv_inverse_helper
        (acc: t * IntSet.t) (v_exp: var_id * exp_t) : 
        (t * IntSet.t) * ((var_id * exp_t) option) =
      let acc_inverse_map, acc_target_var_set = acc in
      match exp_inverse acc_inverse_map acc_target_var_set v_exp with
      | Some (new_v, new_v_exp) ->
        (
          (new_v, new_v_exp) :: acc_inverse_map,
          IntSet.remove new_v acc_target_var_set
        ), None
      | None -> acc, Some v_exp
    in
    let rec get_adv_inverse
        (reverse_map: t) (ctx2_target_var_set: IntSet.t) (context_map: t) : t =
      let init_var_set_size = IntSet.cardinal ctx2_target_var_set in
      let init_context_map_size = List.length context_map in
      let (reverse_map, ctx2_target_var_set), result_map =
        List.fold_left_map adv_inverse_helper (reverse_map, ctx2_target_var_set) context_map
      in
      let new_var_set_size = IntSet.cardinal ctx2_target_var_set in
      if new_var_set_size >= init_var_set_size then reverse_map
      else if new_var_set_size = 0 then reverse_map
      else
        let context_map = List.filter_map (fun x -> x) result_map in
        let new_context_map_size = List.length context_map in
        if new_context_map_size >= init_context_map_size then reverse_map
        else if new_context_map_size = 0 then reverse_map
        else
          get_adv_inverse reverse_map ctx2_target_var_set context_map
    in
    get_adv_inverse reverse_map ctx2_target_var_set context_map





end

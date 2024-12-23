open Taint_exp
open Sexplib.Std

module TaintContext = struct
  exception TaintContextError of string

  let taint_context_error msg = raise (TaintContextError ("[Taint Context Error] " ^ msg))

  type idx_t = TaintExp.taint_var_id
  [@@deriving sexp]

  module VarSet = TaintExp.TaintVarSet

  type t = {
    var_idx: idx_t;
    sub_var_set: VarSet.t;
    sup_exp_body_list: VarSet.t list; (* This is equivalent to SingleExp.t list, but I ignore the case of consant and unknown taint. *)
  }
  [@@deriving sexp]
  (* We use this data structure to represent some extra taint relation 
     that is not convenient to be directly represented by taint solution. *)

  type sub_t = TaintExp.t * TaintExp.t
  [@@deriving sexp]

  let get_sub_sup_set (taint_ctx: t list) (var_idx: idx_t) : VarSet.t * (VarSet.t list) =
    match List.find_opt (fun (x: t) -> x.var_idx = var_idx) taint_ctx with
    | Some entry -> entry.sub_var_set, entry.sup_exp_body_list
    | _ -> VarSet.empty, []

  let check_sub_helper (taint_ctx: t list) (sub_idx: idx_t) (sup_exp_body: VarSet.t) : bool =
    if VarSet.mem sub_idx sup_exp_body then true
    else
      let _, sup_exp_body_list = get_sub_sup_set taint_ctx sub_idx in
      let find_sub_exp_body =
        List.find_opt (
          fun (exp_body: VarSet.t) ->
            VarSet.subset exp_body sup_exp_body
        ) sup_exp_body_list
      in
      find_sub_exp_body <> None

  let check_sub_t (taint_ctx: t list) (sub_sup: sub_t) : bool =
    match sub_sup with
    | TaintUnknown, _ | _, TaintUnknown -> taint_context_error "should not check unknown"
    | TaintConst false, _ -> true
    | _, TaintConst false -> false
    | _, TaintConst true -> true
    | TaintConst true, _ -> false
    | TaintVar v1, TaintVar v2 ->
      if v1 = v2 then true
      else
        let v2_sub_set, _ = get_sub_sup_set taint_ctx v2 in
        VarSet.mem v1 v2_sub_set
    | TaintVar v1, TaintExp s2 ->
      check_sub_helper taint_ctx v1 s2
    | TaintExp s1, TaintVar v2 ->
      let v2_sub_set, _ = get_sub_sup_set taint_ctx v2 in
      VarSet.subset s1 v2_sub_set
    | TaintExp s1, TaintExp s2 ->
      List.fold_left (
        fun (acc: bool) (sub_idx: idx_t) ->
          acc && (check_sub_helper taint_ctx sub_idx s2)
      ) true (VarSet.to_list s1)
    
  let add_one_var_sub (new_sub_body: VarSet.t) (ctx: t) : t =
    { ctx with sub_var_set = VarSet.union ctx.sub_var_set new_sub_body }

  let add_one_var_sup (new_sup_body: VarSet.t) (ctx: t) : t =
    let merged, sup_exp_body_list =
      List.fold_left_map (
        fun (acc: bool) (sup_exp_body: VarSet.t) ->
          if acc then true, sup_exp_body
          else
            if VarSet.subset new_sup_body sup_exp_body then true, new_sup_body
            else if VarSet.subset sup_exp_body new_sup_body then true, sup_exp_body
            else false, sup_exp_body
      ) false ctx.sup_exp_body_list
    in
    if merged then { ctx with sup_exp_body_list = sup_exp_body_list }
    else { ctx with sup_exp_body_list = new_sup_body :: sup_exp_body_list }

  let update_context (update_helper: t -> t) (ctx: t list) (var_idx: idx_t) : t list =
    List.map (
      fun (entry_ctx: t) ->
        if entry_ctx.var_idx = var_idx then update_helper entry_ctx
        else entry_ctx
    ) ctx

  let update_context_with_sub_t (ctx: t list) (sub_sup: sub_t) : t list =
    match sub_sup with
    | TaintUnknown, _ | _, TaintUnknown -> taint_context_error "should not have unknown"
    | TaintConst _, _ | _, TaintConst _ -> ctx
    | TaintVar v1, TaintVar v2 ->
      update_context (add_one_var_sup (VarSet.singleton v2)) ctx v1
    | TaintVar v1, TaintExp s2 ->
      update_context (add_one_var_sup s2) ctx v1
    | TaintExp s1, TaintVar v2 -> 
      List.fold_left (update_context (add_one_var_sup (VarSet.singleton v2))) ctx (VarSet.to_list s1)
    | TaintExp s1, TaintExp s2 ->
      List.fold_left (update_context (add_one_var_sup s2)) ctx (VarSet.to_list s1)

  let one_ctx_to_sub_t (target: t) : sub_t list =
    let self_exp = TaintExp.TaintVar target.var_idx in
    (TaintExp.eval (TaintExp target.sub_var_set), self_exp) ::
    (List.map (fun (sup_body: VarSet.t) -> (self_exp, TaintExp.eval (TaintExp sup_body))) target.sup_exp_body_list)

  let ctx_to_sub_t (target_list: t list) : sub_t list =
    List.concat_map one_ctx_to_sub_t target_list

  let check_ctx (assert_ctx: t list) (check_target: t list) : bool =
    let subtype_list = ctx_to_sub_t check_target in
    List.fold_left (
      fun (acc: bool) (entry: sub_t) ->
        acc && (check_sub_t assert_ctx entry)
    ) true subtype_list

end

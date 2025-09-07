open Basic_type
open Type.Smt_emitter
open Z3
open Sexplib.Std

module DepChangeCtx = struct
  exception DepChangeCtxerror of string
  let dep_change_ctx_error msg = raise (DepChangeCtxerror ("[Dep Change Ctx Error] " ^ msg))

  type exp_t = Expr.expr

  type map_t = DepType.ctx_t * DepType.ctx_t
  [@@deriving sexp]

  type var_exp_map_t = (int * DepType.exp_t) list
  [@@deriving sexp]

  type t = {
    change_copy_map: map_t;
    non_change_offset: DepType.ctx_t;
  }
  [@@deriving sexp]

  let get_copy_var (ctx: context) (var_idx: int) (exp: exp_t) : exp_t =
    Expr.mk_fresh_const ctx (Int.to_string var_idx) (BitVector.mk_sort ctx (DepType.get_exp_bit_size exp))

  let get_copy_exp (copy_map: map_t) (e: exp_t) : exp_t option =
    let copy_orig, copy_target = copy_map in 
    let new_e = Expr.substitute e copy_orig copy_target in
    if Expr.equal new_e e then None
    else Some new_e

  let add_copy_assertion (smt_ctx: SmtEmitter.t) (copy_map: map_t) (e: exp_t) : unit =
    match get_copy_exp copy_map e with
    | Some new_e -> SmtEmitter.add_assertions smt_ctx [new_e]
    | None -> ()

  let copy_all_assertions (smt_ctx: SmtEmitter.t) (copy_map: map_t) : unit =
    let assertions = Solver.get_assertions (snd smt_ctx) in
    let copy_assertions = List.filter_map (get_copy_exp copy_map) assertions in
    SmtEmitter.add_assertions smt_ctx copy_assertions

  let add_extra_non_change_assertions (smt_ctx: SmtEmitter.t) (change_info: t) : unit =
    List.iter (
      fun non_change_offset ->
        match get_copy_exp change_info.change_copy_map non_change_offset with
        | None -> ()
        | Some copy_offset -> 
          SmtEmitter.add_assertions smt_ctx [ Z3.Boolean.mk_eq (fst smt_ctx) non_change_offset copy_offset ]
    ) change_info.non_change_offset

  let check_non_change_exp
      (smt_ctx: SmtEmitter.t)
      (copy_map: map_t) (e: exp_t) : bool =
    match get_copy_exp copy_map e with
    | None -> true (* no change *)
    | Some copy_e ->
      (* Printf.printf "check_non_change_exp\n%s\n%s\n" (Expr.to_string copy_e) (Expr.to_string e); *)
      DepType.check_eq smt_ctx copy_e e

  let check_non_change
      (smt_ctx: SmtEmitter.t)
      (copy_map: map_t)
      (e: DepType.t) : bool =
    match e with
    | Top _ -> true
    | Exp e -> check_non_change_exp smt_ctx copy_map e

  let check_subtype_non_change
      (smt_ctx: SmtEmitter.t) (* current ctx is sub's ctx *)
      (sub_change_info: t) (sup_change_info: t)
      (* (sub_copy_map: map_t) (sup_copy_map: map_t) *)
      (ctx_map: DepType.map_t) : bool =
    let sub_non_change_exp =
      List.filter_map (
        fun (var_idx, sub_exp) ->
          match sub_exp with
          | DepType.Top _ -> None (* sub is top -> is no change -> sat no matter whether sup is non change or not *)
          | Exp sub_exp ->
            let sup_exp = 
              BitVector.mk_const_s (fst smt_ctx) 
                (DepType.get_dep_var_string var_idx) 
                (DepType.get_exp_bit_size sub_exp)
            in
            match get_copy_exp sup_change_info.change_copy_map sup_exp with
            | None -> Some (var_idx, sub_exp) (* sup is non change, need to check sub is also non change *)
            | Some _ -> None (* sup is change, do not need to check sub is non change *)
      ) ctx_map
    in
    let sub_non_change_off_exp = 
      List.map (
        fun sup_non_change_off ->
          match DepType.substitute_exp_t (fst smt_ctx) ctx_map sup_non_change_off with
          | Top _ -> dep_change_ctx_error "check_subtype_non_change unexpected top"
          | Exp e -> (-1000, e) (* -1000 is an arbitrary number for debug purpose, which should be different from var idx *)
      ) sup_change_info.non_change_offset 
    in
    let sub_change_exp = List.filter (
      fun (_, sub_exp) -> not (check_non_change_exp smt_ctx sub_change_info.change_copy_map sub_exp)
    ) (sub_non_change_exp @ sub_non_change_off_exp)
    in
    if List.is_empty sub_change_exp then true
    else begin
      Printf.printf "failed to check subtype map non change\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_var_exp_map_t sub_change_exp));
      false
    end

end


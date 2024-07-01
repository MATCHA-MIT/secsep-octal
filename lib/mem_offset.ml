open Single_exp
open Pretty_print
open Smt_emitter

module MemOffset = struct
  exception MemOffsetError of string
  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = SingleExp.t * SingleExp.t
  type diff_t = SingleExp.t * SingleExp.t * bool

  let to_string (e: t) : string =
    let left, right = e in
    "[" ^ (SingleExp.to_string left) ^ ", " ^ (SingleExp.to_string right) ^ "]"

  module ConstraintSet = Set.Make(
    struct
      let compare = SingleExp.cmp
      type t = SingleExp.t
    end
  )

  type sat_result =
  | SatYes
  | SatCond of ConstraintSet.t
  | SatNo

  let smt_error (msg: string) (solver: Z3.Solver.solver) (ge_exps: (SingleExp.t * SingleExp.t * bool) list) =
    mem_offset_error (
      "check_compliance failed: " ^ msg ^ "\n" ^
      "solver:\n" ^
      (Z3.Solver.to_string solver) ^
      "new assumptions:\n  " ^ (String.concat "\n  " (
          List.map (
            fun (se_l, se_r, eq) -> SingleExp.to_string se_l ^ (if eq then " >= " else " > ") ^ SingleExp.to_string se_r
          ) ge_exps
      ))
    )

  let constraint_set_to_solver_exps (z3_ctx: Z3.context) (constraints: ConstraintSet.t) : Z3.Expr.expr list =
    ConstraintSet.fold (fun (se: SingleExp.t) (acc: Z3.Expr.expr list) ->
      let e = Z3.BitVector.mk_sge z3_ctx
        (SmtEmitter.expr_of_single_exp z3_ctx se)
        (Z3.BitVector.mk_numeral z3_ctx "0" SmtEmitter.bv_width) in
      Printf.printf "\n%s\n" (Z3.Expr.to_string e);
      e :: acc
    ) constraints []

  let check_compliance (smt_ctx: SmtEmitter.t) (diffs: diff_t list) : sat_result =
    let (ctx, solver) = smt_ctx in
    let ge_exps = List.map (fun diff ->
      let se_l, se_r, eq = diff in
      if eq then
        SingleExp.eval (SingleBExp (SingleSub, se_l, se_r))
      else
        SingleExp.eval (SingleBExp (SingleSub, se_l, SingleBExp (SingleExp.SingleAdd, se_r, SingleExp.SingleConst 1L)))
    ) diffs in
    let assertions = List.map (fun diff ->
      let se_l, se_r, eq = diff in
      let e_l = SmtEmitter.expr_of_single_exp ctx se_l in
      let e_r = SmtEmitter.expr_of_single_exp ctx se_r in
      if eq then
        Z3.BitVector.mk_uge ctx e_l e_r (* unsigned comparison *)
      else
        Z3.BitVector.mk_ugt ctx e_l e_r (* unsigned comparison *)
    ) diffs in
    let negation = Z3.Boolean.mk_or ctx (
      List.map (fun diff ->
      let se_l, se_r, eq = diff in
      let e_l = SmtEmitter.expr_of_single_exp ctx se_l in
      let e_r = SmtEmitter.expr_of_single_exp ctx se_r in
      if eq then
        Z3.BitVector.mk_ult ctx e_l e_r (* unsigned comparison *)
      else
        Z3.BitVector.mk_ule ctx e_l e_r (* unsigned comparison *)
      ) diffs
    ) in
    Printf.printf "check_compliance\n";
    Z3.Solver.push solver;
    Z3.Solver.add solver assertions;
    Printf.printf "solver = \n%s\n" (Z3.Solver.to_string solver);
    Z3.Solver.pop solver 1;
    Printf.printf "negation = %s\n" (Z3.Expr.to_string negation);
    let result_sat = Z3.Solver.check solver assertions in
    let result_neg = Z3.Solver.check solver [negation] in
    match result_sat, result_neg with
    (* unknown *)
    | Z3.Solver.UNKNOWN, _
    | _, Z3.Solver.UNKNOWN -> smt_error "solver reported UNKNOWN" solver diffs
    (* definite *)
    | Z3.Solver.SATISFIABLE, Z3.Solver.UNSATISFIABLE -> SatYes
    (* satisfiable *)
    | Z3.Solver.SATISFIABLE, Z3.Solver.SATISFIABLE ->
        SatCond (List.fold_left (fun set diff -> ConstraintSet.add diff set) ConstraintSet.empty ge_exps)
    (* unsatisfiable *)
    | Z3.Solver.UNSATISFIABLE, _ -> SatNo

  let constraint_union (constraint_list: ConstraintSet.t list) : ConstraintSet.t =
    let helper (acc: ConstraintSet.t) (entry: ConstraintSet.t) : ConstraintSet.t =
      ConstraintSet.union acc entry
    in
    List.fold_left helper ConstraintSet.empty constraint_list

  let pp_constraint_set (lvl: int) (constraint_set: ConstraintSet.t) =
    PP.print_lvl lvl "Constraint set (>=0):\n";
    List.iteri (
      fun i x ->
        PP.print_lvl (lvl + 1) "<Constraint %d> " i;
        SingleExp.pp_single_exp (lvl + 2) x;
        Printf.printf "\n" 
    ) (ConstraintSet.elements constraint_set)

  (*
  let heuristic_cmp (e: SingleExp.t) : int * bool = (* (sign, sign is determined or not) *)
    match e with
    | SingleConst v ->
      if v > 0L then (1, true) else if v = 0L then (0, true) else (-1, true)
    | SingleBExp (SingleAdd, _, SingleConst offset)
    | SingleBExp (SingleAdd, _, SingleBExp (SingleMul, _, SingleConst offset))
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, _, SingleConst offset), _) ->
      if offset > 0L then 
        (1, false) 
      else if offset < 0L then 
        (-1, false)
      else 
        mem_offset_error "single exp +0 -> not fully evaluated"
    | SingleBExp (SingleMul, _, SingleConst coeff) ->
      if coeff > 0L then 
        (1, false) 
      else if coeff < 0L then 
        (-1, false) 
      else 
        mem_offset_error "single exp * -> not fully evaluated"
    | _ -> (1, false)
  *)

  (* TODO: Double check this!!! *)
  (**)
  let conditional_ge (smt_ctx: SmtEmitter.t) (e1: SingleExp.t) (e2: SingleExp.t) : sat_result =
    check_compliance smt_ctx [(e1, e2, true)]
    (* else
      (cmp_result >= 0, ConstraintSet.singleton diff) *)
    (* match diff with
    | SingleConst v ->
      if v >= 0L then (true, ConstraintSet.empty) else (false, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then (true, ConstraintSet.singleton diff) else (false, ConstraintSet.empty)
    | _ -> (false, ConstraintSet.empty) *)
  (* This might not be true anymore!!! (ge, cond) ge = true means e1 >= e2 if cond is true; ge = false means no conclusion is made from this comparison!!! *)

  (* Let SMT solver attempt to compare the two expressions. 
     If the result is deterministic, return the greater (or equal) expression.
     Otherwise, return the first expression, and the constraints where it is greater.
  *)
  (*
  let get_conditional_greater (smt_ctx: SmtEmitter.t) (e1: SingleExp.t) (e2: SingleExp.t) : SingleExp.t * ConstraintSet.t =
    let cmp_result, cmp_constraints = check_compliance smt_ctx [(e1, e2, true)] in
    let cmp_determined = ConstraintSet.cardinal cmp_constraints = 0 in
    if cmp_determined then
      ((if cmp_result then e1 else e2), cmp_constraints)
    else
      (e1, cmp_constraints)
  *)
    (* match diff with
    | SingleConst v ->
      if v >= 0L then Some (e1, ConstraintSet.empty) else Some (e2, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset >= 0L then 
        Some (e1, ConstraintSet.singleton diff) 
      else 
        Some (e2, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L)))))
    | _ -> None *)

  (*
  let get_conditional_less (smt_ctx: SmtEmitter.t) (e1: SingleExp.t) (e2: SingleExp.t) : SingleExp.t * ConstraintSet.t =
    let cmp_result, cmp_constraints = check_compliance smt_ctx [(e2, e1, true)] in
    let cmp_determined = ConstraintSet.cardinal cmp_constraints = 0 in
    if cmp_determined then
      ((if cmp_result then e1 else e2), cmp_constraints)
    else
      (e1, cmp_constraints)
  *)
    (* match diff with
    | SingleConst v ->
      if v <= 0L then Some (e1, ConstraintSet.empty) else Some (e2, ConstraintSet.empty)
    | SingleBExp (SingleAdd, SingleVar _, SingleConst offset)
    | SingleBExp (SingleAdd, SingleBExp (SingleMul, SingleVar _, SingleConst _), SingleConst offset) ->
      if offset <= 0L then 
        Some (e1, ConstraintSet.singleton (SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleMul, diff, SingleExp.SingleConst (-1L))))) 
      else 
        Some (e2, ConstraintSet.singleton diff)
    | _ -> None *)

  (* TODO: add SMT-fashion judgement *)
  let check_offset (smt_ctx: SmtEmitter.t) (offset: t) : ConstraintSet.t =
    let left, right = offset in
    Printf.printf "check_offset %s, %s\n%!" (SingleExp.to_string left) (SingleExp.to_string right);
    match check_compliance smt_ctx [(right, left, false)] with
    | SatYes -> Printf.printf "SatYes\n"; ConstraintSet.empty
    | SatNo -> (* Check Shixin's comment below *)
        Printf.printf "Offset [%s, %s]\n" (SingleExp.to_string left) (SingleExp.to_string right);
        mem_offset_error "check_offset failed"
    | SatCond constraints ->
        Printf.printf "SatCond\n" ;
        constraints
    (* let cond_ge, cond = conditional_ge right left in
    if cond_ge then cond 
    else begin
      Printf.printf "Offset [%s, %s]\n" (SingleExp.to_string left) (SingleExp.to_string right);
      mem_offset_error "check_offset failed"
      (* Shixin: This is not always true for valid programs!!! *)
      (* TODO: Fix this!!! *)
      (* Printf.printf "Warning: check_offset failed on offset [%s, %s]\n" (SingleExp.to_string left) (SingleExp.to_string right);
      ConstraintSet.singleton (SingleExp.eval (SingleBExp (SingleSub, right, left))) *)
    end *)
  (* TODO: Think about whether here should be greater than instead of greater than or equal to!!! *)

  (* TODO: add SMT-fashion judgement *)
  let cmp_or_merge (smt_ctx: SmtEmitter.t) (o1: t) (o2: t) : ((bool, t) Either.t) * ConstraintSet.t =
    (* let off_cond = ConstraintSet.union (check_offset o1) (check_offset o2) in *)
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let order12 = check_compliance smt_ctx [(l2, r1, true)] in
    let order21 = check_compliance smt_ctx [(l1, r2, true)] in
    match order12, order21 with
    | SatYes, SatNo -> (Left true, ConstraintSet.empty)
    | SatNo, SatYes -> (Left false, ConstraintSet.empty)
    | SatNo, SatNo -> begin (* intersection is guaranteed *)
        let l1min = check_compliance smt_ctx [(l2, l1, true)] in
        let r1max = check_compliance smt_ctx [(r1, r2, true)] in
        match l1min, r1max with
        | SatYes, SatYes -> (Right (l1, r1), ConstraintSet.empty)
        | SatYes, SatCond cr -> (Right (l1, r1), cr)
        | SatYes, SatNo -> (Right (l1, r2), ConstraintSet.empty)
        | SatCond cl, SatYes -> (Right (l1, r1), cl)
        | SatCond cl, SatNo -> (Right (l1, r2), cl)
        | SatNo, SatYes -> (Right (l2, r1), ConstraintSet.empty)
        | SatNo, SatCond cr -> (Right (l2, r1), cr)
        | SatNo, SatNo -> (Right (l2, r2), ConstraintSet.empty)
        | SatCond _, SatCond _ -> mem_offset_error "cmp_or_merge failed: undertermined"
      end
    | SatCond c12, SatNo -> (Left true, c12) (* warning: undertermined*)
    | SatNo, SatCond c21 -> (Left false, c21) (* warning: undertermined*)
    | SatCond _, SatCond _-> mem_offset_error "cmp_or_merge failed: undetermined"
    | SatYes, SatYes
    | SatYes, SatCond _
    | SatCond _, SatYes -> mem_offset_error "cmp_or_merge failed: expected to be impossible"

      (* match get_conditional_less l1 l2, get_conditional_greater r1 r2 with
      | Some (l, cond_l), Some (r, cond_r) -> 
        (Right (l, r), ConstraintSet.union cond_l cond_r)
      | _ ->
        Printf.printf "Cannot merge [%s, %s] [%s, %s]\n" 
            (SingleExp.to_string l1) 
            (SingleExp.to_string r1) 
            (SingleExp.to_string l2) 
            (SingleExp.to_string r2);
        mem_offset_error "cmp_or_merge cannot merge address offset range"  *)

  let equal (o1: t) (o2: t) : bool =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    SingleExp.cmp l1 l2 = 0 && SingleExp.cmp r1 r2 = 0

  (* TODO: add SMT-fashion judgement *)
  let subset (smt_ctx: SmtEmitter.t) (o1: t) (o2: t) : sat_result =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    check_compliance smt_ctx [(l2, l1, true); (r1, r2, true)]

  let cmp (o1: t) (o2: t) : int =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let cmp_l = SingleExp.cmp l1 l2 in
    if cmp_l = 0 then
      SingleExp.cmp r1 r2
    else cmp_l

end

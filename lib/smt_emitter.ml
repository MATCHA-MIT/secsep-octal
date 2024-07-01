open Z3
open Single_exp

module SmtEmitter = struct
  exception SmtEmitterError of string

  let smt_emitter_error msg = raise (SmtEmitterError ("[SMT Emitter Error] " ^ msg))

  type ctx_t = context
  type exp_t = Expr.expr

  (* SMT context*)
  type t = ctx_t * Solver.solver

  let bv_width = 64
  let init_smt_ctx () : t =
    let ctx = mk_context [] in
    let solver = Solver.mk_solver ctx None in
    (ctx, solver)

  let rec expr_of_single_exp (ctx: ctx_t) (se: SingleExp.t) : exp_t =
    match se with
    | SingleConst c -> BitVector.mk_numeral ctx (Int64.to_string c) bv_width
    | SingleVar v -> BitVector.mk_const_s ctx ("s" ^ (Int.to_string v)) bv_width
    | SingleBExp (op, se1, se2) ->
      let e1 = expr_of_single_exp ctx se1 in
      let e2 = expr_of_single_exp ctx se2 in
      begin
        match op with
        | SingleExp.SingleAdd -> BitVector.mk_add ctx e1 e2
        | SingleExp.SingleSub -> BitVector.mk_sub ctx e1 e2 (* should work the same on signed and unsigned entities *)
        | SingleExp.SingleMul -> BitVector.mk_mul ctx e1 e2
        | SingleExp.SingleSal -> BitVector.mk_shl ctx e1 e2
        | SingleExp.SingleSar -> BitVector.mk_ashr ctx e1 e2
        | SingleExp.SingleXor -> BitVector.mk_xor ctx e1 e2
        | SingleExp.SingleAnd -> BitVector.mk_and ctx e1 e2
        | SingleExp.SingleOr -> BitVector.mk_or ctx e1 e2
      end
    | SingleUExp (op, se) ->
      let e = expr_of_single_exp ctx se in
      begin
        match op with
        | SingleExp.SingleNot -> BitVector.mk_not ctx e
      end

end

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

  let mk_numeral (smt_ctx: t) (value: int64) : Expr.expr =
    let z3_ctx, _ = smt_ctx in
    BitVector.mk_numeral z3_ctx (Int64.to_string value) bv_width

  let rec expr_of_single_exp (smt_ctx: t) (se: SingleExp.t) (add_constr: bool) : exp_t =
    let z3_ctx, z3_solver = smt_ctx in
    match se with
    | SingleConst c -> mk_numeral smt_ctx c
    | SingleVar v -> BitVector.mk_const_s z3_ctx ("s" ^ (Int.to_string v)) bv_width
    | SingleBExp (op, se1, se2) ->
      let e1 = expr_of_single_exp smt_ctx se1 add_constr in
      let e2 = expr_of_single_exp smt_ctx se2 add_constr in
      begin
        match op with
        | SingleExp.SingleAdd ->  
            if add_constr then begin
              Solver.add z3_solver [
                BitVector.mk_add_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_add_no_underflow z3_ctx e1 e2;
              ];
            end;
            BitVector.mk_add z3_ctx e1 e2
        | SingleExp.SingleSub ->
            if add_constr then begin
              Solver.add z3_solver [
                BitVector.mk_sub_no_overflow z3_ctx e1 e2;
                BitVector.mk_sub_no_underflow z3_ctx e1 e2 true;
              ];
            end;
            BitVector.mk_sub z3_ctx e1 e2
        | SingleExp.SingleMul ->
            if add_constr then begin
              Solver.add z3_solver [
                BitVector.mk_mul_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_mul_no_underflow z3_ctx e1 e2;
              ];
            end;
            BitVector.mk_mul z3_ctx e1 e2
        | SingleExp.SingleSal -> BitVector.mk_shl z3_ctx e1 e2
        | SingleExp.SingleSar -> BitVector.mk_ashr z3_ctx e1 e2
        | SingleExp.SingleXor -> BitVector.mk_xor z3_ctx e1 e2
        | SingleExp.SingleAnd -> BitVector.mk_and z3_ctx e1 e2
        | SingleExp.SingleOr -> BitVector.mk_or z3_ctx e1 e2
      end
    | SingleUExp (op, se) ->
      let e = expr_of_single_exp smt_ctx se add_constr in
      begin
        match op with
        | SingleExp.SingleNot -> BitVector.mk_not z3_ctx e
      end

end

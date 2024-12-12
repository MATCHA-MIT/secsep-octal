open Z3
open Single_exp_basic
open Pretty_print
open Sexplib

module SmtEmitter = struct
  exception SmtEmitterError of string

  let smt_emitter_error msg = raise (SmtEmitterError ("[SMT Emitter Error] " ^ msg))

  type ctx_t = context
  type exp_t = Expr.expr

  (* SMT context*)
  type t = ctx_t * Solver.solver

  let bv_width = 64
  
  let to_string (smt_ctx: t) : string =
    Printf.sprintf "Current smt_ctx:\n%s\n" (Z3.Solver.to_string (snd smt_ctx))

  let pp_smt_ctx (lvl: int) (smt_ctx: t) : unit =
    PP.print_lvl lvl "%s" (to_string smt_ctx)
  
  let init_smt_ctx () : t =
    let ctx = mk_context [] in
    let solver = Solver.mk_solver ctx None in
    (ctx, solver)

  (* TODO: Maybe we need this later!!! *)
  let t_of_sexp (_: Sexp.t) : t =
  init_smt_ctx ()

  let sexp_of_t (_ : t) : Sexp.t =
    List (Atom "smt_emitter" :: [])

  let mk_numeral (smt_ctx: t) (value: int64) : Expr.expr =
    let z3_ctx, _ = smt_ctx in
    BitVector.mk_numeral z3_ctx (Int64.to_string value) bv_width

  let mk_true (smt_ctx: t) : Expr.expr =
    let ctx, _ = smt_ctx in
    Z3.Boolean.mk_true ctx

  let push (smt_ctx: t) : unit =
    Z3.Solver.push (snd smt_ctx)

  let pop (smt_ctx: t) (num: int) : unit =
    Z3.Solver.pop (snd smt_ctx) num

  let add_assertions (smt_ctx: t) (assertions: Expr.expr list) : unit =
    let _, z3_solver = smt_ctx in
    List.iter (fun a ->
      let a = Expr.simplify a None in
      if Option.is_none (List.find_opt (fun r -> Expr.equal r a) (Solver.get_assertions z3_solver)) then
      Solver.add z3_solver [a]
    ) assertions

  type sat_result_t =
  | SatYes
  | SatNo
  | SatUnknown

  let check_context (smt_ctx: t) : sat_result_t =
    let ctx, z3_solver = smt_ctx in
    match Z3.Solver.check z3_solver [ Z3.Boolean.mk_true ctx ] with
    | Z3.Solver.UNKNOWN -> SatUnknown
    | Z3.Solver.SATISFIABLE -> SatYes
    | Z3.Solver.UNSATISFIABLE -> SatNo

  let check_compliance (smt_ctx: t) (assertions: Expr.expr list) : sat_result_t =
    let ctx, z3_solver = smt_ctx in
    let assertions = 
      List.filter_map (
        fun e ->
          let e = Z3.Expr.simplify e None in
          if Z3.Boolean.is_true e then None else Some e
      ) assertions
    in
    if List.length assertions = 0 then SatYes 
    else
      let not_assertions =
        List.map (
          fun x -> Z3.Expr.simplify (Z3.Boolean.mk_not ctx x) None
        ) assertions
      in
      let negation = Z3.Boolean.mk_or ctx not_assertions in
      
      (*
      Printf.printf "\ncheck_compliance\n";
      Printf.printf "base solver (%d assertions) = \n%s\nbase result: %s\n"
        (Z3.Solver.get_num_assertions z3_solver) (Z3.Solver.to_string z3_solver) (Z3.Solver.string_of_status (Z3.Solver.check z3_solver []));
      (* Printf.printf "base solver (%d assertions) = \n%s\nbase result: %s\n"
      (Z3.Solver.get_num_assertions z3_solver) "..." (Z3.Solver.string_of_status (Z3.Solver.check z3_solver [])); *)
      (* get string of all assertion and concat them *)
      Printf.printf "assertion = \n%s\n\n" (
        List.fold_left (fun acc x -> (Z3.Expr.to_string x) ^ " " ^ acc) "" assertions
      );
      Printf.printf "negation  = \n%s\n\n" (Z3.Expr.to_string negation);
      *)
      match Z3.Solver.check z3_solver assertions with
      | Z3.Solver.UNKNOWN -> smt_emitter_error "solver reports unknown"
      | Z3.Solver.SATISFIABLE -> begin
          match Z3.Solver.check z3_solver [negation] with
          | Z3.Solver.UNKNOWN -> smt_emitter_error "solver reports unknown"
          | Z3.Solver.SATISFIABLE ->
            (* Printf.printf "both satyes\n"; *)
            SatUnknown
          | Z3.Solver.UNSATISFIABLE ->
            (* Printf.printf "satyes\n"; *)
            SatYes
        end
      | Z3.Solver.UNSATISFIABLE -> begin
          (* Printf.printf "satno\n"; *)
          SatNo
        end

  let rec expr_of_single_exp (smt_ctx: t) (se: SingleExpBasic.t) (add_constr: bool) : exp_t =
    (* let add_constr = true in *)
    let z3_ctx, _ = smt_ctx in
    match se with
    | SingleTop -> smt_emitter_error "expr_of_single_exp cannot convert SingleTop!!!"
    | SingleConst c -> mk_numeral smt_ctx c
    | SingleVar v -> BitVector.mk_const_s z3_ctx ("s" ^ (Int.to_string v)) bv_width
    | SingleBExp (op, se1, se2) ->
      let e1 = expr_of_single_exp smt_ctx se1 add_constr in
      let e2 = expr_of_single_exp smt_ctx se2 add_constr in
      begin
        match op with
        | SingleAdd ->  
            if add_constr then begin
              add_assertions smt_ctx [
                BitVector.mk_add_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_add_no_underflow z3_ctx e1 e2;
              ];
            end;
            BitVector.mk_add z3_ctx e1 e2
        | SingleSub ->
            if add_constr then begin
              add_assertions smt_ctx [
                BitVector.mk_sub_no_overflow z3_ctx e1 e2;
                BitVector.mk_sub_no_underflow z3_ctx e1 e2 true;
              ];
            end;
            BitVector.mk_sub z3_ctx e1 e2
        | SingleMul ->
            if add_constr then begin
              add_assertions smt_ctx [
                BitVector.mk_mul_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_mul_no_underflow z3_ctx e1 e2;
              ];
            end;
            BitVector.mk_mul z3_ctx e1 e2
        | SingleSal -> BitVector.mk_shl z3_ctx e1 e2
        | SingleSar -> BitVector.mk_ashr z3_ctx e1 e2
        | SingleXor -> BitVector.mk_xor z3_ctx e1 e2
        | SingleAnd -> BitVector.mk_and z3_ctx e1 e2
        | SingleOr -> BitVector.mk_or z3_ctx e1 e2
        | SingleMod ->BitVector.mk_smod z3_ctx e1 e2
      end
    | SingleUExp (op, se) ->
      let e = expr_of_single_exp smt_ctx se add_constr in
      begin
        match op with
        | SingleNot -> BitVector.mk_not z3_ctx e
      end

end

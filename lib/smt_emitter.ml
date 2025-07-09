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
  let () = assert (bv_width mod 8 = 0)
  
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
  [@@deriving sexp]

let get_model (smt_ctx: t) : Model.model option =
    let _, z3_solver = smt_ctx in
    Z3.Solver.get_model z3_solver

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
    if List.length assertions = 0 then SatYes else

    (*
    let _ = begin
      Printf.printf "\ncheck_compliance\n";
      Printf.printf "base solver (%d assertions) = \n%s\nbase result: %s\n"
        (Z3.Solver.get_num_assertions z3_solver) (Z3.Solver.to_string z3_solver) (Z3.Solver.string_of_status (Z3.Solver.check z3_solver []));
      (* Printf.printf "base solver (%d assertions) = \n%s\nbase result: %s\n"
      (Z3.Solver.get_num_assertions z3_solver) "..." (Z3.Solver.string_of_status (Z3.Solver.check z3_solver [])); *)
      (* get string of all assertion and concat them *)
      Printf.printf "assertion (%d) = \n%s\n\n" (List.length assertions) (
        List.fold_left (fun acc x -> (Z3.Expr.to_string x) ^ " " ^ acc) "" assertions
      );
    end in
    *)

    match Z3.Solver.check z3_solver assertions with
    | Z3.Solver.UNKNOWN -> smt_emitter_error "solver reports unknown"
    | Z3.Solver.UNSATISFIABLE -> begin
        (* Printf.printf "satno\n"; *)
        SatNo
      end
    | Z3.Solver.SATISFIABLE -> begin
        let neg_sat = List.exists (fun assertion ->
          (* Printf.printf "checking neg %s\n" (Z3.Expr.to_string assertion); *)
          let neg = Z3.Boolean.mk_not ctx assertion in
          match Z3.Solver.check z3_solver [neg] with
          | Z3.Solver.UNKNOWN -> smt_emitter_error "solver reports unknown"
          | Z3.Solver.SATISFIABLE ->
            (* Printf.printf "this expr is not always true:\n%s\n" (Z3.Expr.to_string assertion);
            Printf.printf "counter example:\n%s\n" (Z3.Model.to_string (get_model smt_ctx |> Option.get)); *)
            true
          | Z3.Solver.UNSATISFIABLE -> false
        ) assertions in
        if neg_sat then SatUnknown else SatYes
      end

  let expr_var_str_of_single_var (var_idx: int) : string =
    Printf.sprintf "s%d" var_idx

  let expr_var_str_of_taint_var (var_idx: int) : string =
    Printf.sprintf "t%d" var_idx
  
  let get_bv_size (bv: exp_t) : int =
    bv |> Expr.get_sort |> BitVector.get_size
  
  let signed_ext_bv (ctx: Z3.context) (target_sz: int) (bv: exp_t) : exp_t =
    let bv_sz = get_bv_size bv in
    (* if bv is wider than target_sz, we stay silent; if size mismatched for bv ops, complains will be generated *)
    if bv_sz >= target_sz then bv
    else BitVector.mk_sign_ext ctx (target_sz - bv_sz) bv
  
  let expr_of_ite_cond
      (z3_ctx: Z3.context)
      (cond: CondTypeBase.t)
      (l: exp_t)
      (r: exp_t)
      : exp_t =
    match cond with
    | Ne -> Boolean.mk_not z3_ctx (Boolean.mk_eq z3_ctx l r)
    | Eq -> Boolean.mk_eq z3_ctx l r
    | Lt -> BitVector.mk_slt z3_ctx l r
    | Le -> BitVector.mk_sle z3_ctx l r
    | Bt -> BitVector.mk_ult z3_ctx l r
    | Be -> BitVector.mk_ule z3_ctx l r
  
  let expr_of_single_exp ?(get_var_size: (int -> int option) option = None)
      (smt_ctx: t) (se: SingleExpBasic.t) (add_constr: bool) : exp_t =
    (* let add_constr = true in *)
    let z3_ctx, _ = smt_ctx in
    let get_var_size = Option.value get_var_size ~default:(fun _ -> Some (bv_width / 8)) in
    let rec helper (se: SingleExpBasic.t) : exp_t =
      let e = match se with
      | SingleTop -> smt_emitter_error "expr_of_single_exp cannot convert SingleTop!!!"
      | SingleConst c -> mk_numeral smt_ctx c
      | SingleVar v ->
        let var_size = (get_var_size v |> Option.value ~default:(bv_width / 8)) in
        BitVector.mk_const_s z3_ctx ("s" ^ (Int.to_string v)) (var_size * 8)
      | SingleBExp (op, se1, se2) ->
        let e1 = helper se1 in
        let e2 = helper se2 in
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
          | SingleShr -> BitVector.mk_lshr z3_ctx e1 e2
          | SingleXor -> BitVector.mk_xor z3_ctx e1 e2
          | SingleAnd -> BitVector.mk_and z3_ctx e1 e2
          | SingleOr -> BitVector.mk_or z3_ctx e1 e2
          | SingleMod ->BitVector.mk_srem z3_ctx e1 e2
        end
      | SingleUExp (op, se) ->
        let e = helper se in
        begin
          match op with
          | SingleNot -> BitVector.mk_not z3_ctx e
        end
      | SingleITE ((cond, cond_l_se, cond_r_se), then_se, else_se) ->
        let ite_cond = 
          if cond_l_se = SingleTop || cond_r_se = SingleTop then 
            Expr.mk_fresh_const z3_ctx "itetop" (Boolean.mk_sort z3_ctx)
          else
            expr_of_ite_cond z3_ctx cond (helper cond_l_se) (helper cond_r_se) 
        in
        let then_exp = helper then_se in
        let else_exp = helper else_se in
        Boolean.mk_ite z3_ctx ite_cond then_exp else_exp
      in
      signed_ext_bv z3_ctx bv_width e
    in
    helper se

  let get_exp_no_overflow_constraint ?(get_var_size: (int -> int option) option = None)
      (smt_ctx: t) (se: SingleExpBasic.t) : exp_t list =
    let z3_ctx, _ = smt_ctx in
    let get_var_size = Option.value get_var_size ~default:(fun _ -> Some (bv_width / 8)) in
    let rec helper (se: SingleExpBasic.t) (constraint_list: exp_t list) : exp_t * (exp_t list) =
      let e, cl = match se with
      | SingleTop -> smt_emitter_error "get_exp_no_overflow_constraint cannot convert SingleTop!!!"
      | SingleConst c -> mk_numeral smt_ctx c, constraint_list
      | SingleVar v ->
        let var_size = (get_var_size v |> Option.value ~default:(bv_width / 8)) in
        BitVector.mk_const_s z3_ctx ("s" ^ (Int.to_string v)) (var_size * 8), constraint_list
      | SingleBExp (op, se1, se2) ->
        let e1, constraint_list = helper se1 constraint_list in
        let e2, constraint_list = helper se2 constraint_list in
        begin
          match op with
          | SingleAdd ->  
              BitVector.mk_add z3_ctx e1 e2,
              [
                BitVector.mk_add_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_add_no_underflow z3_ctx e1 e2;
              ] @ constraint_list
          | SingleSub ->
              BitVector.mk_sub z3_ctx e1 e2,
              [
                BitVector.mk_sub_no_overflow z3_ctx e1 e2;
                BitVector.mk_sub_no_underflow z3_ctx e1 e2 true;
              ] @ constraint_list
          | SingleMul ->
              BitVector.mk_mul z3_ctx e1 e2,
              [
                BitVector.mk_mul_no_overflow z3_ctx e1 e2 true;
                BitVector.mk_mul_no_underflow z3_ctx e1 e2;
              ] @ constraint_list
          | SingleSal -> BitVector.mk_shl z3_ctx e1 e2, constraint_list
          | SingleSar -> BitVector.mk_ashr z3_ctx e1 e2, constraint_list
          | SingleShr -> BitVector.mk_lshr z3_ctx e1 e2, constraint_list
          | SingleXor -> BitVector.mk_xor z3_ctx e1 e2, constraint_list
          | SingleAnd -> BitVector.mk_and z3_ctx e1 e2, constraint_list
          | SingleOr -> BitVector.mk_or z3_ctx e1 e2, constraint_list
          | SingleMod ->BitVector.mk_srem z3_ctx e1 e2, constraint_list
        end
      | SingleUExp (op, se) ->
        let e, constraint_list = helper se constraint_list in
        begin
          match op with
          | SingleNot -> BitVector.mk_not z3_ctx e, constraint_list
        end
      | SingleITE ((cond, cond_l_se, cond_r_se), then_se, else_se) ->
        let cond_l, constraint_list = helper cond_l_se constraint_list in
        let cond_r, constraint_list = helper cond_r_se constraint_list in
        let ite_cond = expr_of_ite_cond z3_ctx cond cond_l cond_r in
        let then_exp, constraint_list = helper then_se constraint_list in
        let else_exp, constraint_list = helper else_se constraint_list in
        Boolean.mk_ite z3_ctx ite_cond then_exp else_exp, constraint_list
      in
      e |> signed_ext_bv z3_ctx bv_width, cl
    in
    let _, result = helper se [] in
    result

end

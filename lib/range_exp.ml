open Isa_basic
open Single_exp
open Single_context
open Smt_emitter
open Sexplib.Std

module RangeExp = struct
  exception RangeExpError of string

  let range_exp_error msg = raise (RangeExpError ("[Range Exp Error] " ^ msg))

  type t = (* TODO: Maybe need more... *)
    | Single of SingleExp.t
    (* begin, end, step; step>0 means begin-aligned, <0 means end-aligned, can be non-canonical*)
    | Range of SingleExp.t * SingleExp.t * int64
    | SingleSet of SingleExp.t list
    | Top
  [@@deriving sexp]

  let cmp (r1: t) (r2: t) : int =
    match r1, r2 with
    | Single e1, Single e2 -> SingleExp.cmp e1 e2
    | Single _, _ -> -1
    | Range _, Single _ -> 1
    | Range (l1, r1, step1), Range (l2, r2, step2) ->
      let c_l = SingleExp.cmp l1 l2 in
      if c_l = 0 then
        let c_r = SingleExp.cmp r1 r2 in
        if c_r = 0 then compare step1 step2
        else c_r
      else c_l
    | Range _, _ -> -1
    | SingleSet _, Single _ | SingleSet _, Range _ -> 1
    | SingleSet el1, SingleSet el2 ->
      List.compare SingleExp.cmp el1 el2
    | SingleSet _, Top -> -1
    | Top, Top -> 0
    | Top, _ -> 1

  let get_var (r: t) : SingleExp.SingleVarSet.t =
    match r with
    | Single s -> SingleExp.get_vars s
    | Range (l, r, _) ->
      let l_vars = SingleExp.get_vars l in
      let r_vars = SingleExp.get_vars r in
      SingleExp.SingleVarSet.union l_vars r_vars
    | SingleSet e_list ->
      List.fold_left (fun acc e -> SingleExp.SingleVarSet.union acc (SingleExp.get_vars e)) SingleExp.SingleVarSet.empty e_list
    | Top -> SingleExp.SingleVarSet.empty

  let canonicalize_range
      (l: SingleExp.t) (r: SingleExp.t) (step: int64)
      : SingleExp.t * SingleExp.t * int64 =
    if step = 0L then
      range_exp_error "RangeExp: step = 0";

    if step > 0L then (* left aligned *)
      (* r - (r - l) % s *)
      let r' = SingleBExp (SingleSub, r, SingleBExp (SingleMod, SingleBExp (SingleSub, r, l), SingleConst step))
        |> SingleExp.eval
      in
      (l, r', step)
    else (* step < 0L, right aligned *)
      let step = Int64.neg step in
      (* l + (r - l) % s *)
      let l' = SingleBExp (SingleAdd, l, SingleBExp (SingleMod, SingleBExp (SingleSub, r, l), SingleConst step))
        |> SingleExp.eval
      in
      (l', r, step)
  
  let canonicalize (r: t) : t =
    match r with
    | Range (l, r, step) ->
      let l, r, step = canonicalize_range l r step in
      Range (l, r, step)
    | _ -> r

  let to_string (e: t) : string =
    match e with
    | Single e -> Printf.sprintf "{%s}" (SingleExp.to_string e)
    | Range (a, b, step) -> Printf.sprintf "[%s, %s] step=%Ld" (SingleExp.to_string a) (SingleExp.to_string b) step
    | SingleSet e_list ->
      let str_list = List.map SingleExp.to_string e_list in
      Printf.sprintf "{%s}" (String.concat ", " str_list)
    | Top -> "Top"

  let to_ocaml_string (e: t) : string = 
    match e with
    | Single e -> Printf.sprintf "Single (%s)" (SingleExp.to_ocaml_string e)
    | Range (a, b, step) -> Printf.sprintf "Range (%s, %s, %LdL)" (SingleExp.to_ocaml_string a) (SingleExp.to_ocaml_string b) step
    | SingleSet e_list ->
      let str_list = List.map SingleExp.to_ocaml_string e_list in
      Printf.sprintf "SingleSet [%s]" (String.concat "; " str_list)
    | Top -> "Top"

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (r: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let v_exp = SingleExp.to_smt_expr smt_ctx (SingleVar v_idx) in
    match r with
    | Single e -> Z3.Boolean.mk_eq ctx v_exp (SingleExp.to_smt_expr smt_ctx e)
    | Range (a, b, step) ->
      let a_exp = SingleExp.to_smt_expr smt_ctx a in
      let b_exp = SingleExp.to_smt_expr smt_ctx b in
      let diff_align =
        if step > 0L then Z3.BitVector.mk_sub ctx v_exp a_exp
        else Z3.BitVector.mk_sub ctx v_exp b_exp
      in
      let step = SingleExp.to_smt_expr smt_ctx (SingleConst step) in
      let a_le_v = Z3.BitVector.mk_sle ctx a_exp v_exp in
      let v_le_b = Z3.BitVector.mk_sle ctx v_exp b_exp in
      let mod_step_eq_0 = 
        Z3.Boolean.mk_eq ctx 
          (Z3.BitVector.mk_smod ctx diff_align step) 
          (SingleExp.to_smt_expr smt_ctx (SingleConst 0L)) 
      in
      Z3.Boolean.mk_and ctx [a_le_v; v_le_b; mod_step_eq_0]
    | SingleSet e_list ->
      let eq_exp_list =
        List.map (
          fun x -> Z3.Boolean.mk_eq ctx v_exp (SingleExp.to_smt_expr smt_ctx x)
        ) e_list
      in
      Z3.Boolean.mk_or ctx eq_exp_list
    | Top -> Z3.Boolean.mk_true ctx

  let to_context (v_idx: int) (r: t) : SingleContext.t option =
    let v_exp = SingleExp.SingleVar v_idx in
    match r with
    | Single e -> Some (Cond (Eq, v_exp, e))
    | Range (a, b, step) ->
      let diff_align : SingleExp.t =
        if step > 0L then SingleBExp (SingleSub, v_exp, a)
        else range_exp_error (Printf.sprintf "expecting canonicalized range")
      in
      let step = SingleExp.SingleConst step in
      Some (And [
        Cond (Le, a, v_exp);
        Cond (Le, v_exp, b);
        Cond (Eq, SingleBExp (SingleMod, diff_align, step), SingleConst 0L);
      ])
    | SingleSet e_list ->
      let eq_exp_list =
        List.map (
          fun x -> SingleContext.Cond (Eq, v_exp, x)
        ) e_list
      in
      Some (Or eq_exp_list)
    | Top -> None

  let find_var_sol (sol: (IsaBasic.imm_var_id * t) list) (v_idx: IsaBasic.imm_var_id) : t =
    match List.find_opt (fun (v, _) -> v = v_idx) sol with
    | Some (_, s) -> s
    | None -> Top

  let to_mem_offset (r: t) (size: int64) : (SingleExp.t * SingleExp.t) option =
    match r with
    | Single e -> Some (e, SingleExp.eval (SingleBExp (SingleAdd, e, SingleConst size)))
    | Range (l, r, _) -> Some (l, SingleExp.eval (SingleBExp (SingleAdd, r, SingleConst size)))
    | _ -> None

  let to_mem_offset2 (r: t) : (SingleExp.t * SingleExp.t) option =
    match r with
    | Single e -> Some (e, e)
    | Range (l, r, _) -> Some (l, r)
    | _ -> None

  let eval_helper (single_eval_helper: SingleExp.t -> SingleExp.t) (r: t) : t =
    match r with
    | Single e -> Single (single_eval_helper e)
    | Range (l, r, step) -> Range (single_eval_helper l, single_eval_helper r, step) (* |> canonicalize *)
      (* NOTE: We only need canonicalize when generate a new range solution. *)
    | SingleSet e_list -> SingleSet (List.map single_eval_helper e_list)
    | Top -> Top

  (* let single_exp_repl_sol (sol: (IsaBasic.imm_var_id * t) list) (e: SingleExp.t) : t =
    let rec helper (e: SingleExp.t) : t =
      match e with
      | SingleTop -> Top
      | SingleConst c -> Single (SingleConst c)
      | SingleVar v -> find_var_sol sol v
      | SingleBExp (bop, e1, e2)  ->
        begin match bop, helper e1, helper e2 with
        | op, Single e1, Single e2 -> Single (SingleExp.eval (SingleBExp (op, e1, e2)))
        | SingleAdd, Single e, Range (e1, e2, s)
        | SingleAdd, Range (e1, e2, s), Single e ->
          Range (SingleExp.eval (SingleBExp (SingleAdd, e, e1)), SingleExp.eval (SingleBExp (SingleAdd, e, e2)), s)
        | SingleSub, Single e, Range (e1, e2, s) ->
          Range (SingleExp.eval (SingleBExp (SingleSub, e, e2)), SingleExp.eval (SingleBExp (SingleSub, e, e1)), s)
        | SingleSub, Range (e1, e2, s), Single e ->
          Range (SingleExp.eval (SingleBExp (SingleSub, e, e1)), SingleExp.eval (SingleBExp (SingleSub, e, e2)), s)
        | SingleMul, Single (SingleConst c), Range (e1, e2, s)
        | SingleMul, Range (e1, e2, s), Single (SingleConst c) ->
          if c = 0L then Single (SingleConst 0L)
          else if c > 0L then
            Range (SingleExp.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            SingleExp.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            Int64.mul c s)
          else
            Range (SingleExp.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            SingleExp.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            Int64.neg (Int64.mul c s))
        | _ -> Top (* TODO: maybe need to handle more cases here *)
        end
      | SingleUExp _ -> Top
    in
    helper e *)

end
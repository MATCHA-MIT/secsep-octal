open Isa
open Single_entry_type
open Smt_emitter

module RangeExp = struct
  exception RangeExpError of string

  let range_exp_error msg = raise (RangeExpError ("[Range Exp Error] " ^ msg))

  type t = (* TODO: Maybe need more... *)
    | Single of SingleEntryType.t
    | Range of SingleEntryType.t * SingleEntryType.t * int64 (* begin, end, step *)
    | SingleSet of SingleEntryType.t list
    | Top

  let to_string (e: t) : string =
    match e with
    | Single e -> Printf.sprintf "{%s}" (SingleEntryType.to_string e)
    | Range (a, b, step) -> Printf.sprintf "[%s, %s] step=%Ld" (SingleEntryType.to_string a) (SingleEntryType.to_string b) step
    | SingleSet e_list ->
      let str_list = List.map SingleEntryType.to_string e_list in
      Printf.sprintf "{%s}" (String.concat ", " str_list)
    | Top -> "Top"

  let to_ocaml_string (e: t) : string = 
    match e with
    | Single e -> Printf.sprintf "Single (%s)" (SingleEntryType.to_ocaml_string e)
    | Range (a, b, step) -> Printf.sprintf "Range (%s, %s, %Ld)" (SingleEntryType.to_ocaml_string a) (SingleEntryType.to_ocaml_string b) step
    | SingleSet e_list ->
      let str_list = List.map SingleEntryType.to_ocaml_string e_list in
      Printf.sprintf "SingleSet [%s]" (String.concat "; " str_list)
    | Top -> "Top"

  let to_smt_expr (smt_ctx: SmtEmitter.t) (v_idx: int) (r: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let v_exp = SingleEntryType.to_smt_expr smt_ctx (SingleVar v_idx) in
    match r with
    | Single e -> Z3.Boolean.mk_eq ctx v_exp (SingleEntryType.to_smt_expr smt_ctx e)
    | Range (a, b, step) ->
      let a_exp = SingleEntryType.to_smt_expr smt_ctx a in
      let b_exp = SingleEntryType.to_smt_expr smt_ctx b in
      let step = SingleEntryType.to_smt_expr smt_ctx (SingleConst step) in
      let a_le_v = Z3.BitVector.mk_sle ctx a_exp v_exp in
      let v_le_b = Z3.BitVector.mk_sle ctx v_exp b_exp in
      let mod_step_eq_0 = 
        Z3.Boolean.mk_eq ctx 
          (Z3.BitVector.mk_smod ctx (Z3.BitVector.mk_sub ctx v_exp a_exp) step) 
          (SingleEntryType.to_smt_expr smt_ctx (SingleConst 0L)) 
      in
      Z3.Boolean.mk_and ctx [a_le_v; v_le_b; mod_step_eq_0]
    | SingleSet e_list ->
      let eq_exp_list =
        List.map (
          fun x -> Z3.Boolean.mk_eq ctx v_exp (SingleEntryType.to_smt_expr smt_ctx x)
        ) e_list
      in
      Z3.Boolean.mk_or ctx eq_exp_list
    | Top -> Z3.Boolean.mk_true ctx

  let find_var_sol (sol: (Isa.imm_var_id * t) list) (v_idx: Isa.imm_var_id) : t =
    match List.find_opt (fun (v, _) -> v = v_idx) sol with
    | Some (_, s) -> s
    | None -> Top

  let to_mem_offset (r: t) (size: int64) : (SingleEntryType.t * SingleEntryType.t) option =
    match r with
    | Single e -> Some (e, SingleEntryType.eval (SingleBExp (SingleAdd, e, SingleConst size)))
    | Range (l, r, _) -> Some (l, SingleEntryType.eval (SingleBExp (SingleAdd, r, SingleConst size)))
    | _ -> None

  let to_mem_offset2 (r: t) : (SingleEntryType.t * SingleEntryType.t) option =
    match r with
    | Single e -> Some (e, e)
    | Range (l, r, _) -> Some (l, r)
    | _ -> None

  (* let single_exp_repl_sol (sol: (Isa.imm_var_id * t) list) (e: SingleEntryType.t) : t =
    let rec helper (e: SingleEntryType.t) : t =
      match e with
      | SingleTop -> Top
      | SingleConst c -> Single (SingleConst c)
      | SingleVar v -> find_var_sol sol v
      | SingleBExp (bop, e1, e2)  ->
        begin match bop, helper e1, helper e2 with
        | op, Single e1, Single e2 -> Single (SingleEntryType.eval (SingleBExp (op, e1, e2)))
        | SingleAdd, Single e, Range (e1, e2, s)
        | SingleAdd, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleAdd, e, e1)), SingleEntryType.eval (SingleBExp (SingleAdd, e, e2)), s)
        | SingleSub, Single e, Range (e1, e2, s) ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), s)
        | SingleSub, Range (e1, e2, s), Single e ->
          Range (SingleEntryType.eval (SingleBExp (SingleSub, e, e1)), SingleEntryType.eval (SingleBExp (SingleSub, e, e2)), s)
        | SingleMul, Single (SingleConst c), Range (e1, e2, s)
        | SingleMul, Range (e1, e2, s), Single (SingleConst c) ->
          if c = 0L then Single (SingleConst 0L)
          else if c > 0L then
            Range (SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            Int64.mul c s)
          else
            Range (SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e2)),
            SingleEntryType.eval (SingleBExp (SingleMul, SingleConst c, e1)),
            Int64.neg (Int64.mul c s))
        | _ -> Top (* TODO: maybe need to handle more cases here *)
        end
      | SingleUExp _ -> Top
    in
    helper e *)

end
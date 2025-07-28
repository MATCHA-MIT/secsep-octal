open Isa_basic
(* open Entry_type *)
open Single_exp_basic
open Single_exp
open Taint_exp
open Constraint
(* open Smt_emitter *)
open Sexplib.Std

module SingleEntryType = struct
include SingleExp

  type ext_t = 
  | SignExt
  | ZeroExt
  | OldExt of t (* Used for memory slot partial update *)
  [@@deriving sexp]

  type flag_src_t =
  | FlagCmp of t * t
  | FlagBInst of IsaBasic.bop * t * t
  | FlagUInst of IsaBasic.uop * t
  | FlagTInst of IsaBasic.top * t list
  [@@deriving sexp]

  type flag_t = {
    legacy: (t * t) option;   (* legacy left and right *)
    finstr: flag_src_t option; (* extra info recording instr modifying the flag *)
  }
  [@@deriving sexp]

  let get_empty_flag () : flag_t =
    { legacy = None; finstr = None }

  let make_flag ~(legacy: (t * t) option) ~(finstr: flag_src_t option) : flag_t =
    { legacy; finstr }

  let get_flag_taint (_: flag_t) : TaintExp.t option = None

  let flag_repl_local_var (map: local_var_map_t) (flag: flag_t) : flag_t =
    let repl = SingleExp.repl_local_var map in
    let legacy = Option.map (fun (l, r) -> (repl l, repl r)) flag.legacy in
    let finstr = Option.map (function
      | FlagCmp (l, r) -> FlagCmp (repl l, repl r)
      | FlagBInst (op, l, r) -> FlagBInst (op, repl l, repl r)
      | FlagUInst (op, e) -> FlagUInst (op, repl e)
      | FlagTInst (op, e_list) -> FlagTInst (op, List.map repl e_list)
    ) flag.finstr in
    { legacy; finstr }

  let flag_get_useful_vars (flag: flag_t) : SingleExp.SingleVarSet.t =
    let legacy_vars =
      match flag.legacy with
      | None -> SingleExp.SingleVarSet.empty
      | Some (l, r) -> SingleExp.SingleVarSet.union (SingleExp.get_vars l) (SingleExp.get_vars r)
    in
    let finstr_vars =
      match flag.finstr with
      | None -> SingleExp.SingleVarSet.empty
      | Some (FlagCmp (l, r)) -> SingleExp.SingleVarSet.union (SingleExp.get_vars l) (SingleExp.get_vars r)
      | Some (FlagBInst (_, l, r)) -> SingleExp.SingleVarSet.union (SingleExp.get_vars l) (SingleExp.get_vars r)
      | Some (FlagUInst (_, e)) -> SingleExp.get_vars e
      | Some (FlagTInst (_, e_list)) -> List.fold_left SingleExp.SingleVarSet.union SingleExp.SingleVarSet.empty (List.map SingleExp.get_vars e_list)
    in
    SingleExp.SingleVarSet.union legacy_vars finstr_vars

  let compile_cond_j (cc: IsaBasic.branch_cond) (flag: flag_t) : CondTypeBase.t * t * t =
    let error_msg =
      Printf.sprintf "compile_cond_j: unsupported combination, flag: %s, cc: %s\n"
        (flag |> sexp_of_flag_t |> Sexplib.Sexp.to_string_hum)
        (cc |> IsaBasic.sexp_of_branch_cond |> Sexplib.Sexp.to_string_hum)
    in
    match cc, flag.finstr with
    | _, Some FlagCmp (_, _)
    | _, Some FlagBInst (IsaBasic.Sub, _, _)
    | _, Some FlagUInst (IsaBasic.Dec, _) -> (
      let fl, fr = Option.get flag.legacy in
      match cc with
      | JNe -> (Ne, fl, fr)
      | JE ->  (Eq, fl, fr)
      | JL ->  (Lt, fl, fr)
      | JLe -> (Le, fl, fr)
      | JG ->  (Lt, fr, fl)
      | JGe -> (Le, fr, fl)
      | JB ->  (Bt, fl, fr)
      | JBe -> (Be, fl, fr)
      | JA ->  (Bt, fr, fl)
      | JAe -> (Be, fr, fl)
      | _ -> single_exp_error "expecting only (n)eq or signed conditions"
    )
    | _, Some (FlagBInst (IsaBasic.Add, l, r)) -> (
      match cc, l, r with
      | JB, SingleConst c, _ when c < 0L -> (Be, SingleConst (Int64.neg c), r)
      | JB, _, SingleConst c when c < 0L -> (Be, SingleConst (Int64.neg c), l)
      | _ -> single_exp_error error_msg
    )
    | JB, Some (FlagBInst (IsaBasic.Bt, l, r)) -> (
      let bit = eval (SingleBExp (SingleAnd, SingleConst 1L, SingleBExp (SingleSar, l, r))) in
      (Eq, bit, SingleConst 1L)
    )
    | JE, Some (FlagBInst (IsaBasic.And, l, r)) -> (Eq, SingleBExp (SingleAnd, l, r), SingleConst 0L)
    | JNe, Some (FlagBInst (IsaBasic.And, l, r)) -> (Ne, SingleBExp (SingleAnd, l, r), SingleConst 0L)
    | _ -> single_exp_error error_msg

  let compile_cond_cmov (cc: IsaBasic.bop) (flag: flag_t) : CondTypeBase.t * t * t =
    let dummy_j: IsaBasic.branch_cond = match cc with
    | CmovNe -> JNe
    | CmovE -> JE
    | CmovL -> JL
    | CmovLe -> JLe
    | CmovG -> JG
    | CmovGe -> JGe
    | CmovB -> JB
    | CmovBe -> JBe
    | CmovA -> JA
    | CmovAe -> JAe
    | CmovOther -> JOther
    | _ -> single_exp_error "compile_cond_cmov: expecting cmovxx"
    in
    compile_cond_j dummy_j flag

  let compile_cond_set (cc: IsaBasic.uop) (flag: flag_t) : CondTypeBase.t * t * t =
    let dummy_j: IsaBasic.branch_cond = match cc with
    | SetNe -> JNe
    | SetE -> JE
    | SetL -> JL
    | SetLe -> JLe
    | SetG -> JG
    | SetGe -> JGe
    | SetB -> JB
    | SetBe -> JBe
    | SetA -> JA
    | SetAe -> JAe
    | SetOther -> JOther
    | _ -> single_exp_error "compile_cond_set: expecting setxx"
    in
    compile_cond_j dummy_j flag

  let get_taint_var_map (_: local_var_map_t) : TaintExp.local_var_map_t option = None

  let partial_read_val (e: t) : t =
    match e with
    | _ -> SingleTop

  let partial_write_val (orig_e: t) (write_e: t) : t =
    match orig_e, write_e with
    | _ -> SingleTop

  let next_var (e: t) : t =
    match e with
    | SingleVar v -> SingleVar (v + 1)
    | _ -> single_exp_error "next_var is not called on a single var"

  let read_val (orig_sz: int64 option) (off: int64) (sz: int64) (e: t) : t =
    if Option.is_none orig_sz then begin if off = 0L then e else SingleTop end else
    let orig_sz = Option.get orig_sz in
    if off = 0L then begin
      let mask = match sz with
      | 1L when orig_sz > 1L -> 0xFFL
      | 2L when orig_sz > 2L -> 0xFFFFL
      (* when 4-byte register is used the variable represents the whole 8-byte registers (upper 32 bits are zero) *)
      (* this asserts there's no extracting lower 32-bits using 4-byte register when the register is used as 8-byte *)
      (* adding this will cause taint infer failed (spotted on salsa20) *)
      (* | 4L when orig_sz > 4L -> 0xFFFFFFFFL *)
      | _ -> 0L
      in
      if mask = 0L then
        e
      else
        SingleBExp (SingleAnd, e, SingleConst mask)
    end else
      SingleTop

  let write_gpr_partial (off: int64) (sz: int64) (orig_e: t) (write_e: t) : t =
    match off, sz with
    | 0L, 1L -> SingleBExp (SingleAdd, write_e, SingleBExp (SingleAnd, orig_e, SingleConst 0xFFFFFFFFFFFFFF00L))
    | 0L, 2L -> SingleBExp (SingleAdd, write_e, SingleBExp (SingleAnd, orig_e, SingleConst 0xFFFFFFFFFFFF0000L))
    | 1L, 1L -> SingleBExp (SingleAdd, (SingleBExp (SingleMul, write_e, SingleConst 256L)), SingleBExp (SingleAnd, orig_e, SingleConst 0xFFFFFFFFFFFF00FFL))
    | _ -> single_exp_error "write_gpr_partial: expecting off,sz=0,1/0,2/1,1"

  let mem_partial_read_val (e: t) : t =
    let _ = e in SingleTop

  let mem_partial_write_val (e_old: t) (e_new: t) : t =
    let _ = e_old, e_new in SingleTop

  let ext_val (ext_option: ext_t) (off: int64) (sz: int64) (e: t) : t =
    let _ = sz in
    match ext_option, off with
    | SignExt, 0L
    | ZeroExt, 0L -> e
    | _ -> SingleTop

  let get_eq_taint_constraint (old_e: t) (new_e: t) : Constraint.t list = 
    let _ = old_e, new_e in []

  let get_sub_taint_constraint (sub: t) (sup: t) : Constraint.t list =
    let _ = sub, sup in []

  let get_untaint_constraint (e: t) : Constraint.t list =
    let _ = e in []

  let get_overwritten_taint_constraint (e: t) : Constraint.t list =
    let _ = e in []

  let get_must_known_taint_constraint (e: t) : Constraint.t list =
    let _ = e in []

  let update_ld_taint_constraint (_: t) (_: TaintExp.t option) : Constraint.t list = []

  let update_st_taint_constraint (e: t) (st_taint: TaintExp.t option) : t * (Constraint.t list) =
    let _ = st_taint in e, []

  let get_const_type = get_imm_type

  let exe_cmov (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (flag: flag_t) : t * flag_t =
    (* Printf.printf "exe_cmov cond ? %s : %s\n" (SingleExp.to_string e2) (SingleExp.to_string e1); *)
    let res = match isa_bop with
    | CmovOther -> SingleTop
    | CmovNe | CmovE | CmovL | CmovLe | CmovG | CmovGe
    | CmovB | CmovBe | CmovA | CmovAe ->
      SingleITE (compile_cond_cmov isa_bop flag, e2, e1)
    | _ -> single_exp_error "exe_cmov: expecting cmovxx"
    in
    eval res, flag (* no impact on flag *)

  let exe_set (isa_uop: IsaBasic.uop) (_) (flag: flag_t) : t * flag_t =
    let e1 = SingleExp.SingleConst 1L in
    let e0 = SingleExp.SingleConst 0L in
    let res = match isa_uop with
    | SetOther -> SingleTop
    | SetNe | SetE | SetL | SetLe | SetG | SetGe
    | SetB | SetBe | SetA | SetAe ->
      SingleITE (compile_cond_set isa_uop flag, e1, e0)
    | _ -> single_exp_error "exe_set: expecting setxx"
    in
    eval res, flag (* no impact on flag *)

  let exe_bop_inst (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (flag: flag_t) (same_op: bool): t * flag_t =
    let flag_src = Some (FlagBInst (isa_bop, e1, e2)) in
    (* default new flag, if instr modifies any flag *)
    let new_flag = make_flag ~legacy:None ~finstr:flag_src in
    match isa_bop with
    | Add -> eval (SingleBExp (SingleAdd, e1, e2)), new_flag
    | Adc -> SingleTop, new_flag
    | Sub ->
      let res = eval (SingleBExp (SingleSub, e1, e2)) in
      res, make_flag ~legacy:(Some (res, SingleConst 0L)) ~finstr:flag_src
    | Sbb -> SingleTop, new_flag
    | Mul -> SingleTop, new_flag
    | Imul -> eval (SingleBExp (SingleMul, e1, e2)), new_flag
    | Sal | Shl -> eval (SingleBExp (SingleSal, e1, e2)), new_flag
    | Sar -> eval (SingleBExp (SingleSar, e1, e2)), new_flag
    | Shr -> eval (SingleBExp (SingleShr, e1, e2)), new_flag
    | Rol | Ror -> SingleTop, new_flag (* we assume that rol and ror always affect flag *)
    | Xor -> (if same_op then SingleConst 0L else eval (SingleBExp (SingleXor, e1, e2))), new_flag
    | And -> eval (SingleBExp (SingleAnd, e1, e2)), new_flag
    | Or -> eval (SingleBExp (SingleOr, e1, e2)), new_flag
    | CmovNe | CmovE | CmovL | CmovLe | CmovG | CmovGe
    | CmovB | CmovBe | CmovA | CmovAe | CmovOther ->
      exe_cmov isa_bop e1 e2 flag
    | Bt -> (* bit test, set CF to the bit *)
      e1, new_flag
    | Pxor | Xorp -> if same_op then SingleConst 0L, flag else SingleTop, flag
    | Punpck | Packxs
    | Pshuf
    | Padd | Psub | Pand | Pandn | Por
    | Psll | Psrl -> SingleTop, flag

  let exe_uop_inst (isa_uop: IsaBasic.uop) (e: t) (flag: flag_t) : t * flag_t =
    let flag_src = Some (FlagUInst (isa_uop, e)) in
    (* default new flag, if instr modifies any flag *)
    let new_flag = make_flag ~legacy:None ~finstr:flag_src in
    match isa_uop with
    | Mov | MovZ | MovS | Lea -> e, flag
    | Not -> eval (SingleUExp (SingleNot, e)), flag
    | Bswap -> SingleTop, flag
    | Neg ->
      eval (SingleBExp (SingleMul, e, SingleConst (-1L))), new_flag
    | Inc ->
      eval (SingleBExp (SingleAdd, e, SingleConst 1L)), new_flag
    | Dec ->
      let res = eval (SingleBExp (SingleAdd, e, SingleConst (-1L))) in
      res, make_flag ~legacy:(Some (res, SingleConst 0L)) ~finstr:flag_src
    | SetNe | SetE | SetL | SetLe | SetG | SetGe
    | SetB | SetBe | SetA | SetAe | SetOther ->
      exe_set isa_uop e flag

  let exe_top_inst (isa_top: IsaBasic.top) (e_list: t list) (flag: flag_t) : t * flag_t =
    let flag_src = Some (FlagTInst (isa_top, e_list)) in
    (* default new flag, if instr modifies any flag *)
    let new_flag = make_flag ~legacy:None ~finstr:flag_src in
    match isa_top with
    | Shld | Shrd -> SingleTop, new_flag
    | Shufp -> SingleTop, flag

  let get_taint_exp (_: t) : TaintExp.t option = None

  let get_single_taint_exp (_: t) : t * TaintExp.t =
    single_exp_error "Cannot get single taint exp on a single entry type"

  let set_taint_with_other (x: t) (_: t) : t = x

  let get_single_var_map (m: local_var_map_t) : SingleExp.local_var_map_t = m

  let get_top_type () : t = SingleTop
  let get_top_untaint_type () : t = SingleTop

  let get_top_taint_type () : t = SingleTop

  let get_unknown_taint_type () : t = SingleTop

  let add_context_map
      (is_mem: bool)
      (simp_func: SingleExp.t -> SingleExp.t)
      (m: local_var_map_t) (e1: t) (e2: t) :
      local_var_map_t =
    let _ = is_mem in
    add_local_var_simp simp_func m e1 e2

end

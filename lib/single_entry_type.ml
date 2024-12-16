open Isa_basic
(* open Entry_type *)
open Single_exp
open Taint_exp
open Constraint
(* open Smt_emitter *)

module SingleEntryType = struct
include SingleExp

  type ext_t = 
  | SignExt
  | ZeroExt
  | OldExt of t (* Used for memory slot partial update *)
  [@@deriving sexp]

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

  let read_val (off: int64) (sz: int64) (e: t) : t =
    let _ = sz in
    if off = 0L then e else SingleTop

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

  let update_ld_taint_constraint (_: t) (_: TaintExp.t option) : Constraint.t list = []

  let update_st_taint_constraint (e: t) (st_taint: TaintExp.t option) : t * (Constraint.t list) =
    let _ = st_taint in e, []

  let exe_bop_inst (is_check: bool) (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (same_op: bool) : t =
    match isa_bop with
    | Add -> eval (SingleBExp (SingleAdd, e1, e2))
    | Adc -> SingleTop
    | Sub -> eval (SingleBExp (SingleSub, e1, e2))
    | Mul -> SingleTop
    | Imul -> eval (SingleBExp (SingleMul, e1, e2))
    | Sal | Shl -> eval (SingleBExp (SingleSal, e1, e2))
    | Sar -> eval (SingleBExp (SingleSar, e1, e2))
    | Shr -> (* Check has a dirty, possibly incorrect calculation *)
      if is_check then SingleTop else eval (SingleBExp (SingleSar, e1, e2))
    | Rol | Ror -> SingleTop
    | Xor -> if same_op then SingleConst 0L else eval (SingleBExp (SingleXor, e1, e2))
    | And -> eval (SingleBExp (SingleAnd, e1, e2))
    | Or -> eval (SingleBExp (SingleOr, e1, e2))
    | Pxor | Xorps -> if same_op then SingleConst 0L else SingleTop
    | Punpck | Packus
    | Pand | Por
    | Psll | Psrl -> SingleTop

  let exe_uop_inst (isa_uop: IsaBasic.uop) (e: t) : t =
    match isa_uop with
    | Mov | MovZ | Lea -> e
    | Not -> eval (SingleUExp (SingleNot, e))
    | Bswap -> SingleTop
    | Neg -> eval (SingleBExp (SingleMul, e, SingleConst (-1L)))
    | Inc -> eval (SingleBExp (SingleAdd, e, SingleConst 1L))
    | Dec -> eval (SingleBExp (SingleAdd, e, SingleConst (-1L)))

  let exe_top_inst (isa_top: IsaBasic.top) (_: t list) : t =
    match isa_top with
    | _ -> SingleTop

  let get_single_taint_exp (_: t) : t * TaintExp.t =
    single_exp_error "Cannot get single taint exp on a single entry type"

  let set_taint_with_other (x: t) (_: t) : t = x

  let get_single_var_map (m: local_var_map_t) : SingleExp.local_var_map_t = m

  let get_const_type = get_imm_type

  let get_top_type () : t = SingleTop
  let get_top_untaint_type () : t = SingleTop

  let get_top_taint_type () : t = SingleTop

  let add_context_map
      (is_mem: bool)
      (simp_func: SingleExp.t -> SingleExp.t)
      (m: local_var_map_t) (e1: t) (e2: t) :
      local_var_map_t =
    let _ = is_mem in
    add_local_var_simp simp_func m e1 e2

end

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

  type flag_t = t * t
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

  let set_flag_helper (dest_type: t) =
    dest_type, (dest_type, get_const_type (IsaBasic.ImmNum (0L, None)))

  let exe_cmov (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (flags: flag_t) : t * flag_t =
    (* Printf.printf "exe_cmov cond ? %s : %s\n" (SingleExp.to_string e2) (SingleExp.to_string e1); *)
    let fl, fr = flags in
    let res = match isa_bop with
    | CmovNe -> SingleITE ((Ne, fl, fr), e2, e1)
    | CmovE -> SingleITE ((Eq, fl, fr), e2, e1)
    | CmovL -> SingleITE ((Lt, fl, fr), e2, e1)
    | CmovLe -> SingleITE ((Le, fl, fr), e2, e1)
    | CmovG -> SingleITE ((Lt, fr, fl), e2, e1)
    | CmovGe -> SingleITE ((Le, fr, fl), e2, e1)
    | CmovB -> SingleITE ((Bt, fl, fr), e2, e1)
    | CmovBe -> SingleITE ((Be, fl, fr), e2, e1)
    | CmovA -> SingleITE ((Bt, fr, fl), e2, e1)
    | CmovAe -> SingleITE ((Be, fr, fl), e2, e1)
    | CmovOther -> SingleTop
    | _ -> single_exp_error "exe_cmov: expecting cmovxx"
    in
    eval res |> set_flag_helper

  let exe_set (isa_bop: IsaBasic.uop) (_) (flags: flag_t) : t * flag_t =
    let fl, fr = flags in
    let e2 = SingleExp.SingleConst 1L in
    let e1 = SingleExp.SingleConst 0L in
    let res = match isa_bop with
    | SetNe -> SingleITE ((Ne, fl, fr), e2, e1)
    | SetE -> SingleITE ((Eq, fl, fr), e2, e1)
    | SetL -> SingleITE ((Lt, fl, fr), e2, e1)
    | SetLe -> SingleITE ((Le, fl, fr), e2, e1)
    | SetG -> SingleITE ((Lt, fr, fl), e2, e1)
    | SetGe -> SingleITE ((Le, fr, fl), e2, e1)
    | SetB -> SingleITE ((Bt, fl, fr), e2, e1)
    | SetBe -> SingleITE ((Be, fl, fr), e2, e1)
    | SetA -> SingleITE ((Bt, fr, fl), e2, e1)
    | SetAe -> SingleITE ((Be, fr, fl), e2, e1)
    | SetOther -> SingleTop
    | _ -> single_exp_error "exe_set: expecting setxx"
    in
    eval res |> set_flag_helper

  let exe_bop_inst (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (flags: flag_t) (same_op: bool): t * flag_t =
    match isa_bop with
    | Add -> eval (SingleBExp (SingleAdd, e1, e2)) |> set_flag_helper
    | Adc -> SingleTop |> set_flag_helper
    | Sub -> eval (SingleBExp (SingleSub, e1, e2)) |> set_flag_helper
    | Sbb -> SingleTop |> set_flag_helper
    | Mul -> SingleTop |> set_flag_helper
    | Imul -> eval (SingleBExp (SingleMul, e1, e2)) |> set_flag_helper
    | Sal | Shl -> eval (SingleBExp (SingleSal, e1, e2)) |> set_flag_helper
    | Sar -> eval (SingleBExp (SingleSar, e1, e2)) |> set_flag_helper
    | Shr -> eval (SingleBExp (SingleShr, e1, e2)) |> set_flag_helper
    | Rol | Ror -> SingleTop |> set_flag_helper
    | Xor -> (if same_op then SingleConst 0L else eval (SingleBExp (SingleXor, e1, e2))) |> set_flag_helper
    | And -> eval (SingleBExp (SingleAnd, e1, e2)) |> set_flag_helper
    | Or -> eval (SingleBExp (SingleOr, e1, e2)) |> set_flag_helper
    | CmovNe | CmovE | CmovL | CmovLe | CmovG | CmovGe
    | CmovB | CmovBe | CmovA | CmovAe | CmovOther ->
      exe_cmov isa_bop e1 e2 flags
    | Bt -> (* bit test, set CF to the bit *)
      let result = eval (SingleBExp (SingleAnd, SingleConst 1L, SingleBExp (SingleSar, e1, e2))) in
      e1, (result, get_const_type (IsaBasic.ImmNum (0L, None)))
    | Pxor | Xorp -> if same_op then SingleConst 0L, flags else SingleTop, flags
    | Punpck | Packxs
    | Pshuf
    | Padd | Psub | Pand | Pandn | Por
    | Psll | Psrl -> SingleTop, flags

  let exe_uop_inst (isa_uop: IsaBasic.uop) (e: t) (flags: flag_t) : t * flag_t =
    match isa_uop with
    | Mov | MovZ | MovS | Lea -> e, flags
    | Not -> eval (SingleUExp (SingleNot, e)), flags
    | Bswap -> SingleTop, flags
    | Neg ->
      eval (SingleBExp (SingleMul, e, SingleConst (-1L))) |> set_flag_helper
    | Inc ->
      eval (SingleBExp (SingleAdd, e, SingleConst 1L)) |> set_flag_helper
    | Dec ->
      eval (SingleBExp (SingleAdd, e, SingleConst (-1L))) |> set_flag_helper
    | SetNe | SetE | SetL | SetLe | SetG | SetGe
    | SetB | SetBe | SetA | SetAe | SetOther ->
      exe_set isa_uop e flags

  let exe_top_inst (isa_top: IsaBasic.top) (_: t list) (flags: flag_t) : t * flag_t =
    match isa_top with
    | Shld | Shrd -> SingleTop |> set_flag_helper
    | Shufp -> SingleTop, flags

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

open Isa
(* open Entry_type *)
open Single_exp
open Smt_emitter

module SingleEntryType = struct
include SingleExp

  type ext_t = 
  | SignExt
  | ZeroExt
  | OldExt of t (* Used for memory slot partial update *)

  type local_var_map_t = (Isa.imm_var_id * t) list

  let get_empty_var_map : local_var_map_t = []

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

  let ext_val (ext_option: ext_t) (off: int64) (sz: int64) (e: t) : t =
    let _ = sz in
    match ext_option, off with
    | SignExt, 0L
    | ZeroExt, 0L -> e
    | _ -> SingleTop

  let exe_bop_inst (isa_bop: Isa.bop) (e1: t) (e2: t) : t =
    match isa_bop with
    | Add -> eval (SingleBExp (SingleAdd, e1, e2))
    | Adc -> SingleTop
    | Sub -> eval (SingleBExp (SingleSub, e1, e2))
    | Mul -> SingleTop
    | Imul -> eval (SingleBExp (SingleMul, e1, e2))
    | Sal | Shl -> eval (SingleBExp (SingleSal, e1, e2))
    | Sar -> eval (SingleBExp (SingleSar, e1, e2))
    | Shr -> SingleTop
    | Rol | Ror -> SingleTop
    | Xor -> if cmp e1 e2 = 0 then SingleConst 0L else eval (SingleBExp (SingleXor, e1, e2))
    | And -> eval (SingleBExp (SingleAnd, e1, e2))
    | Or -> eval (SingleBExp (SingleOr, e1, e2))

  let exe_uop_inst (isa_uop: Isa.uop) (e: t) : t =
    match isa_uop with
    | Mov | MovS | MovZ | Lea -> e
    | Not -> eval (SingleUExp (SingleNot, e))
    | Bswap -> SingleTop

  let get_single_exp (e: t) : t = e

  let get_const_type = get_imm_type

  let get_top_type : t = SingleTop

  let update_local_var (map: local_var_map_t) (e: t) (pc: int) : (local_var_map_t * t) =
    let new_idx = -pc in
    (new_idx, e) :: map, SingleVar new_idx

  let find_local_var_map (map: local_var_map_t) (idx: int) : t option =
    List.find_map (fun (i, e) -> if i = idx then Some e else None) map

  let repl_local_var (map: local_var_map_t) (e: t) : t =
    let rec repl_helper (e: t) : t =
      match e with
      | SingleTop | SingleConst _ -> e
      | SingleVar v ->
        if v > 0 then e
        else begin 
          match find_local_var_map map v with
          | Some e -> repl_helper e
          | None -> e
        end
      | SingleBExp (bop, e1, e2) ->
        let e1 = repl_helper e1 in
        let e2 = repl_helper e2 in
        eval (SingleBExp (bop, e1, e2))
      | SingleUExp (uop, e) ->
        let e = repl_helper e in
        eval (SingleUExp (uop, e))
    in
    repl_helper e

  let to_smt_expr (smt_ctx: SmtEmitter.t) (e: t) : SmtEmitter.exp_t = 
    SmtEmitter.expr_of_single_exp smt_ctx e false

end

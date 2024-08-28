open Pretty_print
open Entry_type
open Smt_emitter
open Isa

module CondType (Entry: EntryType) = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type entry_t = Entry.t

  type cond = 
    | Eq | Ne
    | Le | Lt (* Signed comparison *)
    | Be | Bt (* Unsigned comparison *)

  type t = cond * entry_t * entry_t

  let not_cond_type (cond: t) : t =
    match cond with
    | (Eq, l, r) -> (Ne, l, r)
    | (Ne, l, r) -> (Eq, l, r)
    | (Le, l, r) -> (Lt, r, l)
    | (Lt, l, r) -> (Le, r, l)
    | (Be, l, r) -> (Bt, r, l)
    | (Bt, l, r) -> (Be, l, r)

  let to_string (cond: t) =
    let c, l, r = cond in
    let s = match c with
    | Eq -> "Eq" | Ne -> "Ne"
    | Le -> "Le" | Lt -> "Lt"
    | Be -> "Be" | Bt -> "Bt"
    in
    Printf.sprintf "%s(%s,%s)" s (Entry.to_string l) (Entry.to_string r)
    (* s ^ "(" ^ (Entry.to_string l) ^ "," ^ (Entry.to_string r) ^ ")" *)

  let get_taken_type (cond: Isa.branch_cond) (flag: entry_t * entry_t) : t option =
    let l, r = flag in
    match cond with
    | JNe -> Some (Ne, l, r)
    | JE -> Some (Eq, l, r)
    | JL -> Some (Lt, l, r)
    | JLe -> Some (Le, l, r)
    | JG -> Some (Lt, r, l)
    | JGe -> Some (Le, r, l)
    | JB -> Some (Bt, l, r)
    | JBe -> Some (Be, l, r)
    | JA -> Some (Bt, r, l)
    | JAe -> Some (Be, r, l)
    | JOther -> None

  let to_smt_expr (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let ctx, _ = smt_ctx in
    let cond, l, r = cond in
    let exp_l = Entry.to_smt_expr smt_ctx l in
    let exp_r = Entry.to_smt_expr smt_ctx r in
    match cond with
    | Ne -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx exp_l exp_r)
    | Eq -> Z3.Boolean.mk_eq ctx exp_l exp_r
    | Le -> Z3.BitVector.mk_sle ctx exp_l exp_r
    | Lt -> Z3.BitVector.mk_slt ctx exp_l exp_r
    | Be -> Z3.BitVector.mk_ule ctx exp_l exp_r
    | Bt -> Z3.BitVector.mk_ult ctx exp_l exp_r

  let pp_cond (lvl: int) (cond: t) =
    PP.print_lvl lvl "%s\n" (to_string cond)

  let pp_cond_list (lvl: int) (cond_list: t list) =
    PP.print_lvl lvl "<Cond list>\n";
    List.iter (fun x -> pp_cond (lvl + 1) x) (List.rev cond_list)

end

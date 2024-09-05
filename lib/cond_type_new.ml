open Pretty_print
open Entry_type
open Single_entry_type
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

  let has_top (cond: t) : bool =
    let _, l, r = cond in
    if Entry.cmp l Entry.get_top_type = 0 || Entry.cmp r Entry.get_top_type = 0 then
      true
    else false

  let pp_cond (lvl: int) (cond: t) =
    PP.print_lvl lvl "%s\n" (to_string cond)

  let pp_cond_list (lvl: int) (cond_list: t list) =
    PP.print_lvl lvl "<Cond list>\n";
    List.iter (fun x -> pp_cond (lvl + 1) x) (List.rev cond_list)

end

module SingleCondType = struct
include (CondType (SingleEntryType))
  let naive_check (cond: t) : (bool, t) Either.t =
    let cond, l, r = cond in
    let e = SingleEntryType.eval (SingleBExp (SingleSub, l, r)) in
    match e with
    | SingleConst c ->
      begin match cond with
      | Eq -> Left (c = 0L)
      | Ne -> Left (c != 0L)
      | Le -> Left (c <= 0L)
      | Lt -> Left (c < 0L)
      | _ -> Right (cond, e, SingleConst 0L)
      end
    | _ -> Right (cond, e, SingleConst 0L)

  let get_z3_mk (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let z3_ctx, _ = smt_ctx in
    let cond, l, r = cond in
    let l = SingleEntryType.to_smt_expr smt_ctx l in
    let r = SingleEntryType.to_smt_expr smt_ctx r in
    match cond with
    | Eq -> Z3.Boolean.mk_eq z3_ctx l r
    | Ne -> Z3.Boolean.mk_not z3_ctx (Z3.Boolean.mk_eq z3_ctx l r)
    | Le -> Z3.BitVector.mk_sle z3_ctx l r
    | Lt -> Z3.BitVector.mk_slt z3_ctx l r
    | Be -> Z3.BitVector.mk_ule z3_ctx l r
    | Bt -> Z3.BitVector.mk_ult z3_ctx l r

  let check (smt_ctx: SmtEmitter.t) (cond_list: t list) : SmtEmitter.sat_result_t =
    let known_list, unknown_list = List.partition_map naive_check cond_list in
    if List.find_opt (fun x -> not x) known_list != None then
      SatNo (* If any no is found, then it is definitely not satisfied *)
    else begin
      let exp_list = List.map (get_z3_mk smt_ctx) unknown_list in
      if List.length exp_list = 0 then
        SatYes
      else begin
        (* Printf.printf "check call smt %s\n" (String.concat "; " (List.map to_string unknown_list)); *)
        SmtEmitter.check_compliance smt_ctx exp_list
      end
    end

end

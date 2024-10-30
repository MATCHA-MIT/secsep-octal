open Pretty_print
open Entry_type_basic
open Single_exp
(* open Single_entry_type *)
open Smt_emitter
open Isa_basic

module CondType (Entry: EntryTypeBasic) = struct
  exception CondTypeError of string
  let cond_type_error msg = raise (CondTypeError ("[Cond Type Error] " ^ msg))

  type entry_t = Entry.t
  [@@deriving sexp]

  type cond = 
    | Eq | Ne
    | Le | Lt (* Signed comparison *)
    | Be | Bt (* Unsigned comparison *)
  [@@deriving sexp]

  type t = cond * entry_t * entry_t
  [@@deriving sexp]

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

  let to_ocaml_string (cond: t) =
    let c, l, r = cond in
    let s = match c with
    | Eq -> "Eq" | Ne -> "Ne"
    | Le -> "Le" | Lt -> "Lt"
    | Be -> "Be" | Bt -> "Bt"
    in
    Printf.sprintf "(%s, %s, %s)" s (Entry.to_ocaml_string l) (Entry.to_ocaml_string r)

  let get_taken_type (cond: IsaBasic.branch_cond) (flag: entry_t * entry_t) : t option =
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
    if SingleExp.cmp (Entry.get_single_exp l) SingleTop = 0 || 
      SingleExp.cmp (Entry.get_single_exp r) SingleTop = 0 then
      true
    else false

  let pp_cond (lvl: int) (cond: t) =
    PP.print_lvl lvl "%s\n" (to_string cond)

  let pp_cond_list (lvl: int) (cond_list: t list) =
    PP.print_lvl lvl "<Cond list>\n";
    List.iter (fun x -> pp_cond (lvl + 1) x) (List.rev cond_list)

  let pp_ocaml_cond_list (lvl: int) (buf: Buffer.t) (cond_list: t list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (fun x ->
      PP.bprint_lvl (lvl + 1) buf "%s;\n" (to_ocaml_string x)
    ) cond_list;
    PP.bprint_lvl lvl buf "]\n"

end

module SingleCondType = struct
include (CondType (SingleExp))
  let naive_check (cond: t) : (bool, t) Either.t =
    let cond, l, r = cond in
    let e = SingleExp.eval (SingleBExp (SingleSub, l, r)) in
    match e with
    | SingleConst c ->
      begin match cond with
      | Eq -> Left (c = 0L)
      | Ne -> Left (c <> 0L)
      | Le -> Left (c <= 0L)
      | Lt -> Left (c < 0L)
      (* TODO: Check after using these manual heuristic rules *)
      | _ -> Right (cond, l, r)
      end
    | _ -> Right (cond, l, r)

  let naive_check_impossible (cond: t) : bool =
    let cond, l, r = cond in
    let e = SingleExp.eval (SingleBExp (SingleSub, l, r)) in
    match e with
    | SingleConst c ->
      begin match cond with
      | Eq -> c <> 0L
      | Ne -> c = 0L
      | Le -> c > 0L
      | Lt -> c >= 0L
      | _ -> false
      end
    | _ -> false

  let get_z3_mk (smt_ctx: SmtEmitter.t) (cond: t) : SmtEmitter.exp_t =
    let z3_ctx, _ = smt_ctx in
    let cond, l, r = cond in
    let l = SingleExp.to_smt_expr smt_ctx l in
    let r = SingleExp.to_smt_expr smt_ctx r in
    match cond with
    | Eq -> Z3.Boolean.mk_eq z3_ctx l r
    | Ne -> Z3.Boolean.mk_not z3_ctx (Z3.Boolean.mk_eq z3_ctx l r)
    | Le -> Z3.BitVector.mk_sle z3_ctx l r
    | Lt -> Z3.BitVector.mk_slt z3_ctx l r
    | Be -> Z3.BitVector.mk_ule z3_ctx l r
    | Bt -> Z3.BitVector.mk_ult z3_ctx l r

  let add_assertions (smt_ctx: SmtEmitter.t) (cond_list: t list) : unit =
    SmtEmitter.add_assertions smt_ctx (List.map (get_z3_mk smt_ctx) cond_list)

  let check (is_quick: bool) (smt_ctx: SmtEmitter.t) (cond_list: t list) : SmtEmitter.sat_result_t =

    (* choose from one of two versions below *)

    let res: SmtEmitter.sat_result_t = begin
    (* if List.find_opt (fun x -> naive_check_impossible x) cond_list <> None then
      SatNo *)

    
    let known_list, unknown_list = List.partition_map naive_check cond_list in
    if List.find_opt (fun x -> not x) known_list <> None then
      SatNo (* If any no is found, then it is definitely not satisfied *)
   

    else begin

      (* choose from one of two versions below *)

      (* is_quick = true -> accept quick check, but may ignore overflow/underflow *)
      let exp_list = List.map (get_z3_mk smt_ctx) (if is_quick then unknown_list else cond_list) in

      (* let exp_list = List.map (get_z3_mk smt_ctx) unknown_list in *)

      if List.length exp_list = 0 then
        SatYes
      else begin
        (* Printf.printf "check call smt %s\n" (String.concat "; " (List.map to_string unknown_list)); *)
        SmtEmitter.check_compliance smt_ctx exp_list
      end
    end
    end in
    (* Printf.printf ">>>\n";
    Printf.printf "check\ninputs:\n";
    List.iter (fun x -> Printf.printf "  %s\n" (to_string x)) cond_list;
    Printf.printf "output: %s\n" (match res with SatYes -> "SatYes" | SatNo -> "SatNo" | SatUnknown -> "SatUnknown");
    Printf.printf "<<<\n"; *)
    res

  let check_trivial (cond: t) : bool option =
    (* Use quick check *)
    match check true (SmtEmitter.init_smt_ctx ()) [cond] with
    | SatYes -> Some true
    | SatNo -> Some false
    | _ -> None

  let check_or_assert
      (smt_ctx: SmtEmitter.t) (cond_list: t list) : (t list) option =
    (* If some cond in cond_list is SatNo, return None
      If all cond in cond_list is SatYes, return Some [], no constraint is added to smt_ctx 
      If some cond in cond_list is Unknown, assert it to be true in smt_ctx, return Some list contain it;
      We return the list of minimal extra assertions need to add to smt_ctx to make cond_list hold *)
    let helper (acc: (t list) option) (cond: t) : (t list) option =
      match acc with
      | None -> None
      | Some acc_cond_list ->
        begin match check true smt_ctx [cond] with
        | SatYes -> acc
        | SatNo -> None
        | _ -> 
          SmtEmitter.add_assertions smt_ctx [get_z3_mk smt_ctx cond];
          Some (cond :: acc_cond_list)
        end
    in
    List.fold_left helper (Some []) cond_list

end

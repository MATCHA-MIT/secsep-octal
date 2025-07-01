open Isa_basic
open Single_exp_basic
open Single_exp
open Cond_type_new
open Range_exp
open Pretty_print
open Smt_emitter
open Sexplib.Std
open Sexplib

module MemOffset = struct
  exception MemOffsetError of string

  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = SingleExp.t * SingleExp.t
  [@@deriving sexp]

  type t_pc = t * int
  [@@deriving sexp]

  type off_rel_t =
    | Eq 
    | Subset | Supset 
    | Le | Ge 
    | LOverlap | GOverlap
    | Other
  [@@deriving sexp]

  type off_cmp_mode =
    | CmpEqSubset
    | CmpEq
    | CmpSubset
    | CmpLeGe
    | CmpAll (* cmp [a, b] [b, c] -> Le *)
    | CmpOverlap (* cmp [a, b] [b, c] -> LOverlap *)
    | CmpLeSubsetLoverlap
    | CmpSupLoverlapRoverlapSub
  [@@deriving sexp]

  let off_rel_t_to_string (x: off_rel_t) : string =
    match x with
    | Eq -> "Eq"
    | Subset -> "Subset"
    | Supset -> "Supset"
    | Le -> "Le"
    | Ge -> "Ge"
    | LOverlap -> "LOverlap"
    | GOverlap -> "GOverlap"
    | Other -> "Other"

  let to_string (e: t) : string =
    let l, r = e in
    Printf.sprintf "[%s, %s]" (SingleExp.to_string l) (SingleExp.to_string r)

  let to_ocaml_string (e: t) : string =
    let l, r = e in
    Printf.sprintf "(%s, %s)" (SingleExp.to_ocaml_string l) (SingleExp.to_ocaml_string r)

  let has_top (e: t) : bool =
    fst e = SingleTop || snd e = SingleTop

  (* Syntatically cmp *)
  let cmp (o1: t) (o2: t) : int =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let cmp_l = SingleExp.cmp l1 l2 in
    if cmp_l = 0 then
      SingleExp.cmp r1 r2
    else cmp_l

  (* let to_smt_expr (smt_ctx: SmtEmitter.t) (o: t) : SmtEmitter.exp_t * SmtEmitter.exp_t =
    let l, r = o in
    (* TODO: This use add_no_overflow everytime offset is used for comparison. Consider to change to false for optimization!!! *)
    SmtEmitter.expr_of_single_exp smt_ctx l true,
    SmtEmitter.expr_of_single_exp smt_ctx r true *)

  (* Semantically cmp helper*)
  let offset_cmp_helper (is_quick: bool) (smt_ctx: SmtEmitter.t) (o1: t) (o2: t) (mode: off_cmp_mode) : off_rel_t =
    (* Printf.printf "offset_cmp (quick = %b) (mode = %d):\n%s\n%s\n" is_quick mode (to_string o1) (to_string o2); *)
    (* let stamp_beg = Unix.gettimeofday () in *)

    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let eq_req = [ (CondTypeBase.Eq, l1, l2); (CondTypeBase.Eq, r1, r2) ] in
    let le_req = [ (CondTypeBase.Le, r1, l2) ] in
    let ge_req = [ (CondTypeBase.Le, r2, l1) ] in
    let subset_req = [ (CondTypeBase.Le, l2, l1); (CondTypeBase.Le, r1, r2) ] in
    let supset_req = [ (CondTypeBase.Le, l1, l2); (CondTypeBase.Le, r2, r1) ] in
    let loverlap_req = [ (CondTypeBase.Le, l1, l2); (CondTypeBase.Le, l2, r1) ] in
    let goverlap_req = [ (CondTypeBase.Le, l1, r2); (CondTypeBase.Le, r2, r1) ] in
    let check = SingleCondType.check is_quick smt_ctx in

    (* let z3_ctx, _ = smt_ctx in
    let l1, r1 = to_smt_expr smt_ctx o1 in
    let l2, r2 = to_smt_expr smt_ctx o2 in
    let check = SmtEmitter.check_compliance smt_ctx in 
    let zero = Z3.BitVector.mk_numeral z3_ctx "0" SmtEmitter.bv_width in
    let sub_helper e1 e2 = Z3.BitVector.mk_sub z3_ctx e1 e2 in 
    let eq_req = [ Z3.Boolean.mk_eq z3_ctx (sub_helper l1 l2) zero; Z3.Boolean.mk_eq z3_ctx (sub_helper r1 r2) zero ] in
    let le_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper r1 l2) zero ] in
    let ge_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper r2 l1) zero ] in
    let subset_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper l2 l1) zero; Z3.BitVector.mk_sle z3_ctx (sub_helper r1 r2) zero ] in
    let supset_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper l1 l2) zero; Z3.BitVector.mk_sle z3_ctx (sub_helper r2 r1) zero ] in
    let loverlap_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper l1 l2) zero; Z3.BitVector.mk_sle z3_ctx (sub_helper l2 r1) zero ] in
    let goverlap_req = [ Z3.BitVector.mk_sle z3_ctx (sub_helper l2 l1) zero; Z3.BitVector.mk_sle z3_ctx (sub_helper l1 r2) zero ] in *)

    (* if (Printf.printf "check eq\n"; check eq_req = SatYes) then Eq
    else if (Printf.printf "check le\n"; check le_req = SatYes) then Le
    else if (Printf.printf "check ge\n"; check ge_req = SatYes) then Ge
    else if (Printf.printf "check sub\n"; check subset_req = SatYes) then Subset
    else if (Printf.printf "check sup\n"; check supset_req = SatYes) then Supset
    else if (Printf.printf "check lo\n"; check loverlap_req = SatYes) then LOverlap
    else if (Printf.printf "check go\n"; check goverlap_req = SatYes) then GOverlap *)

    let result = begin
      match mode with
      | CmpEqSubset (*1*) -> begin
          if check eq_req = SatYes then Eq
          else if check subset_req = SatYes then Subset
          else Other
        end
      | CmpEq -> begin
          if check eq_req = SatYes then Eq
          else Other
        end
      | CmpSubset -> begin
          if check subset_req = SatYes then Subset
          else Other
        end
      | CmpLeGe (*2*) -> begin
          if check le_req = SatYes then Le
          else if check ge_req = SatYes then Ge
          else Other
        end
      | CmpAll -> begin
          if check eq_req = SatYes then Eq
          else if check le_req = SatYes then Le
          else if check ge_req = SatYes then Ge
          else if check subset_req = SatYes then Subset
          else if check supset_req = SatYes then Supset
          else if check loverlap_req = SatYes then LOverlap
          else if check goverlap_req = SatYes then GOverlap
          else Other
        end
      | CmpOverlap -> begin
          if check eq_req = SatYes then Eq
          else if check subset_req = SatYes then Subset
          else if check supset_req = SatYes then Supset
          else if check loverlap_req = SatYes then LOverlap
          else if check goverlap_req = SatYes then GOverlap
          else if check le_req = SatYes then Le
          else if check ge_req = SatYes then Ge
          else Other
        end
      | CmpLeSubsetLoverlap -> begin
          if check le_req = SatYes then Le
          else if check subset_req = SatYes then Subset
          else if check loverlap_req = SatYes then LOverlap
          else Other
        end
      | CmpSupLoverlapRoverlapSub -> begin
          if check supset_req = SatYes then Supset
          else if check loverlap_req = SatYes then LOverlap
          else if check goverlap_req = SatYes then GOverlap
          else if check subset_req = SatYes then Subset
          else Other
        end
    end in
    (* Printf.printf "time elapsed (offset_cmp): %f\n\n" (Unix.gettimeofday () -. stamp_beg); *)
    (* Printf.printf "result: %s\n\n" (off_rel_t_to_string result); *)
    result
    (* TODO: Maybe need to handle Other!!! *)

  
  (* Semantically quick cmp, cmp l r -> cmp (l - r) 0, for heuristic usage only *)
  let offset_quick_cmp = offset_cmp_helper true
  (* Semantically cmp*)
  let offset_full_cmp = offset_cmp_helper false

  let from_range (range_l_r: RangeExp.t * RangeExp.t) : t option =
    match range_l_r with
    | Single l, Single r -> Some (l, r)
    | Single l, Range (_, r, _) -> Some (l, r)
    | Range (l, _, _), Single r -> Some (l, r)
    | Range (l, _, _), Range (_, r, _) -> Some (l, r)
    | _ -> None
    (* | l, r ->
      Printf.printf "from_range cannot convert %s %s\n" (RangeExp.to_string l) (RangeExp.to_string r);
      None *)
      (* mem_offset_error 
        (Printf.sprintf "from_range cannot convert %s %s" (RangeExp.to_string l) (RangeExp.to_string r)) *)

  let from_off_opt (off_opt_l_r: (t option) * (t option)) : t option =
    match off_opt_l_r with
    | Some (l, _), Some (_, r) -> Some (l, r)
    | _ -> None

  (* This function should only be used when inserting new offset*)
  (* let assert_no_overflow
      (smt_ctx: SmtEmitter.t) (off: t) : unit =
    let l, r = off in
    let _ = SmtEmitter.expr_of_single_exp smt_ctx l true in
    let _ = SmtEmitter.expr_of_single_exp smt_ctx r true in
    () *)

  let insert_new_offset_list_helper
      (prefer_to_merge: bool)
      (smt_ctx: SmtEmitter.t)
      (ob_list: (t * bool) list) (new_o_list: t list) : ((t * bool) list) * bool =
    (* TODO: filter new_o_list, only focus on addresses involving rsp *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    (* Printf.printf "\ninsert_new_offset_list:\n\ninitial ob_list begin\n"; *)
    (* List.iter (fun (o, _) -> Printf.printf "%s\n" (to_string o)) ob_list; *)
    (* Printf.printf "\nnew_o_list\n"; *)
    (* List.iter (fun o -> Printf.printf "%s\n" (to_string o)) new_o_list; *)
    (* Printf.printf "\nend of debug output\n"; *)
    (* We assume offset in offset_list is sorted from smaller to larger *)
    let rec insert_one_offset (ob_list: (t * bool) list) (new_o: t) : (t * bool) list * bool =
      match ob_list with
      | [] -> [ (new_o, true) ], true
      | (hd_o, hd_updated) :: tl ->
        (* We just need heuristic quick cmp here since we are going to assert no_overflow after inserting offsets*)
        let cmp_mode = if prefer_to_merge then CmpOverlap else CmpAll in
        begin match offset_quick_cmp smt_ctx new_o hd_o cmp_mode with
        | Eq | Subset -> (hd_o, hd_updated) :: tl, true
        | Supset -> insert_one_offset tl new_o
        | Le -> (new_o, true) :: (hd_o, hd_updated) :: tl, true
        | Ge -> 
          let new_tl, insert_tl_success = insert_one_offset tl new_o in
          (hd_o, hd_updated) :: new_tl, insert_tl_success
          (* (hd_o, hd_updated) :: (insert_one_offset tl new_o) *)
        | LOverlap ->
          let new_l, _ = new_o in
          let _, hd_r = hd_o in
          insert_one_offset tl (new_l, hd_r)
        | GOverlap ->
          let _, new_r = new_o in
          let hd_l, _ = hd_o in
          insert_one_offset tl (hd_l, new_r)
        | Other -> 
          Printf.printf "Warning insert_one_offset fail compare new offset %s with known offset %s\n" 
              (to_string new_o) (to_string hd_o);
          let full_cmp_result = offset_quick_cmp smt_ctx new_o hd_o CmpAll in 
          Printf.printf "full cmp result %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_off_rel_t full_cmp_result));
          (* SmtEmitter.pp_smt_ctx 0 smt_ctx; *)
          (hd_o, hd_updated) :: tl, false
          (* mem_offset_error 
            (Printf.sprintf "insert_one_offset fail compare new offset %s with known offset %s" 
              (to_string new_o) (to_string hd_o)) *)
        end
    in
    let result, success_list = List.fold_left_map insert_one_offset ob_list new_o_list in
    let success = List.fold_left (fun acc x -> acc && x) true success_list in
    (* Printf.printf "\nresult:\n";
    List.iter (fun (o, _) -> Printf.printf "%s\n" (to_string o)) result; *)
    (* Printf.printf "\ntime elapsed (insert_new_offset_list): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result, success

  let insert_new_offset_list (* = insert_new_offset_list_helper false *)
      (smt_ctx: SmtEmitter.t)
      (ob_list: (t * bool) list) (new_o_list: t list) : (t * bool) list =
    (* We assume offset in offset_list is sorted from smaller to larger *)
    (* We assume stack is initialized, so only compare with the smallest slot to update the resevered space for child func*)
    let insert_one_offset (ob_list: (t * bool) list) (new_o: t) : (t * bool) list =
      match ob_list with
      | [] -> [ (new_o, true) ]
      | (hd_o, hd_updated) :: tl ->
        Printf.printf "Cmp \n%s\nand\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t new_o)) (Sexplib.Sexp.to_string_hum (sexp_of_t hd_o));
        begin match offset_quick_cmp smt_ctx new_o hd_o CmpLeSubsetLoverlap with
        | Le -> (new_o, true) :: (hd_o, hd_updated) :: tl
        | Subset -> (hd_o, hd_updated) :: tl
        | LOverlap -> 
          let new_l, _ = new_o in
          let _, hd_r = hd_o in
          ((new_l, hd_r), true) :: tl
        | _ -> ob_list
          (* Printf.printf "Warning insert_one_offset fail compare new offset %s with known offset %s\n" 
          (to_string new_o) (to_string hd_o);
          (* SmtEmitter.pp_smt_ctx 0 smt_ctx; *)
          (hd_o, hd_updated) :: tl *)
        end
    in
    let result = List.fold_left insert_one_offset ob_list new_o_list in
    (* Printf.printf "\nresult:\n";
    List.iter (fun (o, _) -> Printf.printf "%s\n" (to_string o)) result; *)
    (* Printf.printf "\ntime elapsed (insert_new_offset_list): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result

  let is_empty (smt_ctx: SmtEmitter.t) (o: t) : bool =
    let l, r = o in
    SingleCondType.check true smt_ctx [ CondTypeBase.Eq, l, r ] = SatYes

  let diff (smt_ctx: SmtEmitter.t) (o: t) (ro_list: t list) : t list =
    (* o - ro_list *)
    if List.find_opt (fun (x: t) -> has_top x) (o :: ro_list) <> None then [ o ] else
    if is_empty smt_ctx o then [] else
    let remain_list, remain_off =
      List.fold_left (
        fun (acc: (t list) * (t option)) (r_o: t) ->
          let remain_list, off_opt = acc in
          match off_opt with
          | None -> acc
          | Some off ->
            begin match offset_quick_cmp smt_ctx r_o off CmpSupLoverlapRoverlapSub with
            | Supset -> remain_list, None
            | LOverlap -> remain_list, Some (snd r_o, snd off)
            | GOverlap -> (fst off, fst r_o) :: remain_list, None
            | Subset -> (fst off, fst r_o) :: remain_list, Some (snd r_o, snd off)
            | Other -> acc
            | _ -> mem_offset_error "should not have this cmp result"
            end
      ) ([], Some o) ro_list
    in
    match remain_off with
    | Some remain_off -> List.rev (remain_off :: remain_list)
    | None -> List.rev remain_list

  let insert_new_offset_list_merge = insert_new_offset_list_helper true

  let get_vars (o: t) : SingleExp.SingleVarSet.t =
    let l, r = o in
    SingleExp.SingleVarSet.union (SingleExp.get_vars l) (SingleExp.get_vars r)

  let is_val (global_var: SingleExp.SingleVarSet.t) (o: t) : bool =
    let l, r = o in SingleExp.is_val global_var l && SingleExp.is_val global_var r

  let repl_var_helper (repl_func: SingleExp.t -> SingleExp.t) (o: t) : t =
    let l, r = o in repl_func l, repl_func r

  let repl_local_var (local_var_map: SingleExp.local_var_map_t) (o: t) : t =
    repl_var_helper (SingleExp.repl_local_var local_var_map) o
    (* let l, r = o in SingleExp.repl_local_var local_var_map l, SingleExp.repl_local_var local_var_map r *)

  let repl_context_var (local_var_map: SingleExp.local_var_map_t) (o: t) : t =
    repl_var_helper (SingleExp.repl_context_var local_var_map) o
    (* let l, r = o in SingleExp.repl_context_var local_var_map l, SingleExp.repl_context_var local_var_map r *)

  let repl_var (local_var_map: SingleExp.local_var_map_t) (o: t) : t =
    repl_var_helper (SingleExp.repl_var local_var_map) o

  let repl
      (repl_func: (SingleExp.t * int) -> SingleExp.t)
      (pc: int) (o: t) : t = 
    let l, r = o in repl_func (l, pc), repl_func (r, pc)

  let repl_var_exp (e: t) (v_idx_exp: IsaBasic.imm_var_id * SingleExp.t) : t =
    let l, r = e in SingleExp.repl_var_exp l v_idx_exp, SingleExp.repl_var_exp r v_idx_exp

  let eval_helper 
      (single_eval_helper: SingleExp.t -> SingleExp.t)
      (off: t) : t =
    let l, r = off in
    single_eval_helper l, single_eval_helper r

  let add_base (base_ptr: SingleExp.t) (o: t) : t =
    let l, r = o in
    SingleExp.eval (SingleBExp (SingleAdd, base_ptr, l)), 
    SingleExp.eval (SingleBExp (SingleAdd, base_ptr, r))

  let pp_off_list (lvl: int) (off_list: t list) =
    PP.print_lvl lvl "MemOffset list\n";
    List.iter (
      fun off ->
        PP.print_lvl (lvl + 1) "%s\n"(to_string off)
    ) off_list

  let pp_unknown_list (lvl: int) (unknown_list: (t * int) list) =
    PP.print_lvl lvl "Unknown list len %d\n" (List.length unknown_list);
    Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_t_pc unknown_list))
    (* List.iter (
      fun (off, pc) ->
        PP.print_lvl (lvl + 1) "%d,\n%s\n" pc (Sexplib.Sexp.to_string_hum (sexp_of_t off))
    ) unknown_list *)
    
  let get_size (off: t) : int64 option =
    let l, r = off in
    match SingleExp.eval (SingleBExp (SingleSub, r, l)) with
    | SingleConst diff -> Some diff
    | _ -> None

  let is_8byte_slot (o: t) : bool =
    SingleExp.cmp (snd o) (SingleExp.SingleBExp (SingleAdd, (fst o), SingleConst (IsaBasic.get_gpr_full_size ()))) = 0
  
end

module MemOffsetSet = struct
  include Set.Make (
    struct
      let compare = MemOffset.cmp
      type t = MemOffset.t
    end
  )

  let t_of_sexp (s_exp: Sexp.t) : t = 
    of_list (list_of_sexp MemOffset.t_of_sexp s_exp)

  let sexp_of_t (s: t) : Sexp.t = 
    sexp_of_list MemOffset.sexp_of_t (elements s)

end

module MemOffsetMap = Map.Make (
  struct
    let compare = MemOffset.cmp
    type t = MemOffset.t
  end
)

module MemRange = struct
  exception MemRangeError of string
  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type range_var_id = int
  [@@deriving sexp]

  type t = 
    | RangeConst of MemOffset.t list
    | RangeVar of range_var_id
    | RangeExp of range_var_id * (MemOffset.t list)
  [@@deriving sexp]

  type local_var_map_t = (range_var_id * (MemOffset.t list)) list
  [@@deriving sexp]

  let to_ocaml_string (r: t) : string =
    match r with
    | RangeConst o ->
      Printf.sprintf "RangeConst [%s]" (String.concat "; " (List.map MemOffset.to_ocaml_string o))
    | RangeVar v -> Printf.sprintf "RangeVar (%d)" v
    | RangeExp (v, o) -> 
      Printf.sprintf "RangeExp (%d, [%s])" 
        v (String.concat "; " (List.map MemOffset.to_ocaml_string o))

  let to_string (r: t) : string = 
    match r with
    | RangeConst o ->
      Printf.sprintf "RangeConst [%s]" (String.concat "; " (List.map MemOffset.to_string o))
    | RangeVar v -> Printf.sprintf "RangeVar (%d)" v
    | RangeExp (v, o) -> 
      Printf.sprintf "RangeExp (%d, [%s])" 
        v (String.concat "; " (List.map MemOffset.to_string o))

  let get_uninit_range () : t = RangeConst []

  let next_var (e: t) : t =
    match e with
    | RangeVar v -> RangeVar (v + 1)
    | _ -> mem_range_error "next_var should only be called on mem range var"

  let get_range_var (r: t) : range_var_id option =
    match r with
    | RangeConst _ -> None
    | RangeVar v -> Some v
    | RangeExp (v, _) -> Some v

  let get_vars (r: t) : SingleExp.SingleVarSet.t =
    let helper (x: MemOffset.t list) : SingleExp.SingleVarSet.t =
      let var_list = List.map MemOffset.get_vars x in
      List.fold_left 
        (fun acc x -> SingleExp.SingleVarSet.union acc x) 
        SingleExp.SingleVarSet.empty var_list
    in
    match r with
    | RangeConst x | RangeExp (_, x) -> helper x
    | _ -> SingleExp.SingleVarSet.empty

  let is_val (global_var: SingleExp.SingleVarSet.t) (r: t) : bool =
    match r with
    | RangeConst o ->
      List.fold_left (
        fun acc o -> acc && (MemOffset.is_val global_var o)
      ) true o
    | _ -> false

  let repl_var_helper (repl_func: SingleExp.t -> SingleExp.t) (r: t) : t =
    match r with
    | RangeConst o -> RangeConst (List.map (MemOffset.repl_var_helper repl_func) o)
    | RangeVar _ -> r
    | RangeExp (v, o) -> RangeExp (v, List.map (MemOffset.repl_var_helper repl_func) o)

  let repl_local_var (local_var_map: SingleExp.local_var_map_t) (r: t) : t =
    repl_var_helper (SingleExp.repl_local_var local_var_map) r
    (* match r with
    | RangeConst o -> RangeConst (List.map (MemOffset.repl_local_var local_var_map) o)
    | RangeVar _ -> r
    | RangeExp (v, o) -> RangeExp (v, List.map (MemOffset.repl_local_var local_var_map) o) *)

  let repl_context_var (local_var_map: SingleExp.local_var_map_t) (r: t) : t =
    repl_var_helper (SingleExp.repl_context_var local_var_map) r
    (* match r with
    | RangeConst o -> RangeConst (List.map (MemOffset.repl_context_var local_var_map) o)
    | RangeVar _ -> r
    | RangeExp (v, o) -> RangeExp (v, List.map (MemOffset.repl_context_var local_var_map) o) *)

  let repl_var (local_var_map: SingleExp.local_var_map_t) (r: t) : t =
    repl_var_helper (SingleExp.repl_var local_var_map) r

  let add_base (base_ptr: SingleExp.t) (r: t) : t =
    match r with
    | RangeConst o_list -> RangeConst (List.map (MemOffset.add_base base_ptr) o_list)
    | _ -> 
      mem_range_error 
        (Printf.sprintf "Cannot add base %s to %s" (SingleExp.to_string base_ptr) (to_string r))

  let merge (smt_ctx: SmtEmitter.t) (r1: t) (r2: t) : t * bool =
    (* TODO: re-implement this later!!! *)
    let helper (o1_list: MemOffset.t list) (o2_list: MemOffset.t list) : MemOffset.t list * bool =
      let ob_list = List.map (fun x -> (x, false)) o1_list in
      let ob_list, success = MemOffset.insert_new_offset_list_merge smt_ctx ob_list (List.rev o2_list) in
      List.map (fun (x, _) -> x) ob_list, success
    in
    match r1, r2 with
    | RangeConst o1, RangeConst o2 -> 
      let merged_o, merge_success = helper o1 o2 in
      RangeConst merged_o, merge_success
      (* RangeConst (helper o1 o2) *)
    | RangeVar v, RangeConst o | RangeConst o, RangeVar v -> 
      if List.is_empty o then RangeVar v, true else RangeExp (v, o), true
    | RangeExp (v, o1), RangeConst o2 | RangeConst o1, RangeExp (v, o2) -> 
      let merged_o, merge_success = helper o1 o2 in
      if List.is_empty merged_o then RangeVar v, merge_success else RangeExp (v, merged_o), merge_success
    | _ -> mem_range_error (Printf.sprintf "Cannot merge %s and %s" (to_string r1) (to_string r2))
    (* let _ = smt_ctx in r1 @ r2 *)

  let repl
      (repl_func: (SingleExp.t * int) -> SingleExp.t)
      (pc: int) (r: t) : t =
    match r with
    | RangeConst off_list -> RangeConst (List.map (MemOffset.repl repl_func pc) off_list)
    | RangeVar _ -> r
    | RangeExp (v, off_list) -> RangeExp (v, List.map (MemOffset.repl repl_func pc) off_list)

  let repl_range_sol
      (smt_ctx: SmtEmitter.t)
      (single_sol_sub_sol_offset_to_offset_list_helper: (MemOffset.t * int) -> MemOffset.t list option)
      (* (single_sol_repl_helper: (SingleExp.t * int) -> SingleExp.t) *)
      (range_sol: ((range_var_id * int) * t) list) 
      (r_pc: t * int) : t =
      (* (r_pc: t * int) : t = *)
    let r, pc = r_pc in
    (* let assert_const_validity_helper
        (const_part: MemOffset.t list) : unit =
      let cond_list = List.map (fun (l, r) -> (SingleCondType.Lt, l, r)) const_part in
      SingleCondType.add_assertions smt_ctx cond_list
    in *)
    let find_var =
      match r with
      | RangeConst _ -> None
      | RangeVar v -> Some (v, [])
      | RangeExp (v, const_part) -> Some (v, const_part)
    in
    match find_var with
    | None -> r
    | Some (v, const_part) ->
      let find_sol =
        List.find_map (
          fun ((idx, _), sol) ->
            if idx = v then Some sol else None
        ) range_sol
      in
      begin match find_sol with
      | Some sol -> 
        (* let sol = repl single_sol_repl_helper pc sol in *)
        (* if v = 103 then
          Printf.printf "!!! merge %s %s\n" (to_string sol) (to_string (RangeConst const_part));
        SmtEmitter.push smt_ctx;
        assert_const_validity_helper const_part; *)
        let result, merge_success = merge smt_ctx sol (RangeConst const_part) in
        (* SmtEmitter.pop smt_ctx 1; *)
        if merge_success then result else
        begin match sol, const_part with
        | RangeConst [off1], [off2] -> 
          begin match single_sol_sub_sol_offset_to_offset_list_helper (off1, pc), single_sol_sub_sol_offset_to_offset_list_helper (off2, pc) with
          | Some off1_list, Some off2_list ->
            let simp_opt =
              List.find_map (
                fun (off1_l, off1_r) ->
                  List.find_map (
                    fun (off2_l, off2_r) ->
                      if SingleExp.cmp off1_r off2_l = 0 then
                        Some (fst off1, snd off2)
                      else if SingleExp.cmp off2_r off1_l = 0 then
                        Some (fst off2, snd off1)
                      else None
                  ) off2_list
              ) off1_list
            in
            begin match simp_opt with
            | Some simp_off -> 
              Printf.printf "repl_range_sol heuristically merge %s %s\n" (to_string sol) (to_string (RangeConst const_part));
              Printf.printf "get %s\n" (MemOffset.to_string simp_off);
              RangeConst [simp_off]
            | None ->
              Printf.printf "Warning: repl_range_sol fail to merge %s %s\n" (to_string sol) (to_string (RangeConst const_part));
              sol
            end
          | _ ->
            Printf.printf "Warning: repl_range_sol fail to merge %s %s\n" (to_string sol) (to_string (RangeConst const_part));
            sol
          end
        | _ -> 
          Printf.printf "Warning: repl_range_sol fail to merge %s %s\n" (to_string sol) (to_string (RangeConst const_part));
          sol
        end
      | None -> r
      end
    
end

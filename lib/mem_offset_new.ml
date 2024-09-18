open Single_exp
open Cond_type_new
open Range_exp
open Pretty_print
open Smt_emitter

module MemOffset = struct
  exception MemOffsetError of string

  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = SingleExp.t * SingleExp.t

  type off_rel_t =
    | Eq 
    | Subset | Supset 
    | Le | Ge 
    | LOverlap | GOverlap
    | Other

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

  (* Syntatically cmp *)
  let cmp (o1: t) (o2: t) : int =
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let cmp_l = SingleExp.cmp l1 l2 in
    if cmp_l = 0 then
      SingleExp.cmp r1 r2
    else cmp_l

  let to_smt_expr (smt_ctx: SmtEmitter.t) (o: t) : SmtEmitter.exp_t * SmtEmitter.exp_t =
    let l, r = o in
    (* TODO: This use add_no_overflow everytime offset is used for comparison. Consider to change to false for optimization!!! *)
    SmtEmitter.expr_of_single_exp smt_ctx l true,
    SmtEmitter.expr_of_single_exp smt_ctx r true

  (* Semantically cmp helper*)
  let offset_cmp_helper (is_quick: bool) (smt_ctx: SmtEmitter.t) (o1: t) (o2: t) (mode: int) : off_rel_t =
    (* Printf.printf "offset_cmp (mode = %d):\n%s\n%s\n" mode (to_string o1) (to_string o2); *)
    (* let stamp_beg = Unix.gettimeofday () in *)

    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let eq_req = [ (SingleCondType.Eq, l1, l2); (SingleCondType.Eq, r1, r2) ] in
    let le_req = [ (SingleCondType.Le, r1, l2) ] in
    let ge_req = [ (SingleCondType.Le, r2, l1) ] in
    let subset_req = [ (SingleCondType.Le, l2, l1); (SingleCondType.Le, r1, r2) ] in
    let supset_req = [ (SingleCondType.Le, l1, l2); (SingleCondType.Le, r2, r1) ] in
    let loverlap_req = [ (SingleCondType.Le, l1, l2); (SingleCondType.Le, l2, r1) ] in
    let goverlap_req = [ (SingleCondType.Le, l2, l1); (SingleCondType.Le, l1, r2) ] in
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
      | 1 -> begin
          if check eq_req = SatYes then Eq
          else if check subset_req = SatYes then Subset
          else Other
        end
      | 2 -> begin
          if check le_req = SatYes then Le
          else if check ge_req = SatYes then Ge
          else Other
        end
      | _ -> begin
          if check eq_req = SatYes then Eq
          else if check le_req = SatYes then Le
          else if check ge_req = SatYes then Ge
          else if check subset_req = SatYes then Subset
          else if check supset_req = SatYes then Supset
          else if check loverlap_req = SatYes then LOverlap
          else if check goverlap_req = SatYes then GOverlap
          else Other
        end
    end in
    (* Printf.printf "time elapsed (offset_cmp): %f\n\n" (Unix.gettimeofday () -. stamp_beg); *)
    (* Printf.printf "result: %s\n" (off_rel_t_to_string result); *)
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

  (* This function should only be used when inserting new offset*)
  (* let assert_no_overflow
      (smt_ctx: SmtEmitter.t) (off: t) : unit =
    let l, r = off in
    let _ = SmtEmitter.expr_of_single_exp smt_ctx l true in
    let _ = SmtEmitter.expr_of_single_exp smt_ctx r true in
    () *)

  let insert_new_offset_list
      (smt_ctx: SmtEmitter.t)
      (ob_list: (t * bool) list) (new_o_list: t list) : (t * bool) list =
    (* TODO: filter new_o_list, only focus on addresses involving rsp *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    (* Printf.printf "\ninsert_new_offset_list:\n\ninitial ob_list begin\n"; *)
    (* List.iter (fun (o, _) -> Printf.printf "%s\n" (to_string o)) ob_list; *)
    (* Printf.printf "\nnew_o_list\n"; *)
    (* List.iter (fun o -> Printf.printf "%s\n" (to_string o)) new_o_list; *)
    (* Printf.printf "\nend of debug output\n"; *)
    (* We assume offset in offset_list is sorted from smaller to larger *)
    let rec insert_one_offset (ob_list: (t * bool) list) (new_o: t) : (t * bool) list =
      match ob_list with
      | [] -> [ (new_o, true) ]
      | (hd_o, hd_updated) :: tl ->
        (* We just need heuristic quick cmp here since we are going to assert no_overflow after inserting offsets*)
        begin match offset_quick_cmp smt_ctx new_o hd_o 0 with
        | Eq | Subset -> (hd_o, hd_updated) :: tl
        | Supset -> insert_one_offset tl new_o
        | Le -> (new_o, true) :: (hd_o, hd_updated) :: tl
        | Ge -> (hd_o, hd_updated) :: (insert_one_offset tl new_o)
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
          (* SmtEmitter.pp_smt_ctx 0 smt_ctx; *)
          (hd_o, hd_updated) :: tl
          (* mem_offset_error 
            (Printf.sprintf "insert_one_offset fail compare new offset %s with known offset %s" 
              (to_string new_o) (to_string hd_o)) *)
        end
    in
    (* We need to manually tell SMT solver that here has no overflow! *)
    (* let solver = snd smt_ctx in
    Z3.Solver.push solver;
    List.fold_left (fun _ x -> assert_no_overflow smt_ctx x) () new_o_list; *)
    let result = List.fold_left insert_one_offset ob_list new_o_list in
    (* Z3.Solver.pop solver 1; *)
    (* Printf.printf "\nresult:\n";
    List.iter (fun (o, _) -> Printf.printf "%s\n" (to_string o)) result; *)
    (* Printf.printf "\ntime elapsed (insert_new_offset_list): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result

  let get_vars (o: t) : SingleExp.SingleVarSet.t =
    let l, r = o in
    SingleExp.SingleVarSet.union (SingleExp.get_vars l) (SingleExp.get_vars r)

  let is_val (global_var: SingleExp.SingleVarSet.t) (o: t) : bool =
    let l, r = o in SingleExp.is_val global_var l && SingleExp.is_val global_var r

  let repl_local_var (local_var_map: SingleExp.local_var_map_t) (o: t) : t =
    let l, r = o in SingleExp.repl_local_var local_var_map l, SingleExp.repl_local_var local_var_map r

  let repl_context_var (local_var_map: SingleExp.local_var_map_t) (o: t) : t =
    let l, r = o in SingleExp.repl_context_var local_var_map l, SingleExp.repl_context_var local_var_map r

  let pp_off_list (lvl: int) (off_list: t list) =
    PP.print_lvl lvl "MemOffset list\n";
    List.iter (
      fun off ->
        PP.print_lvl (lvl + 1) "%s\n"(to_string off)
    ) off_list

  let pp_unknown_list (lvl: int) (unknown_list: (t *  int) list) =
    PP.print_lvl lvl "Unknown list\n";
    List.iter (
      fun (off, pc) ->
        PP.print_lvl (lvl + 1) "%d %s\n" pc (to_string off)
    ) unknown_list
  
end

module MemRange = struct
  exception MemRangeError of string
  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type t = MemOffset.t list

  let to_ocaml_string (r: t) : string =
    Printf.sprintf "[%s]" (String.concat "; " (List.map MemOffset.to_ocaml_string r))

  let get_vars (r: t) : SingleExp.SingleVarSet.t =
    let var_list = List.map MemOffset.get_vars r in
    List.fold_left 
      (fun acc x -> SingleExp.SingleVarSet.union acc x) 
      SingleExp.SingleVarSet.empty var_list

  let is_val (global_var: SingleExp.SingleVarSet.t) (r: t) : bool =
    List.fold_left (
      fun acc o -> acc && (MemOffset.is_val global_var o)
    ) true r

  let repl_local_var (local_var_map: SingleExp.local_var_map_t) (r: t) : t =
    List.map (MemOffset.repl_local_var local_var_map) r

  let repl_context_var (local_var_map: SingleExp.local_var_map_t) (r: t) : t =
    List.map (MemOffset.repl_context_var local_var_map) r

  let merge (smt_ctx: SmtEmitter.t) (r1: t) (r2: t) : t =
    (* TODO: re-implement this later!!! *)
    let _ = smt_ctx in r1 @ r2

end

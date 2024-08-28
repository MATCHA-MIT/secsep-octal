open Single_exp
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

  let to_string (e: t) : string =
    let l, r = e in
    Printf.sprintf "[%s, %s]" (SingleExp.to_string l) (SingleExp.to_string r)

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

  (* Semantically cmp*)
  let offset_cmp (smt_ctx: SmtEmitter.t) (o1: t) (o2: t) : off_rel_t =
    let z3_ctx, _ = smt_ctx in
    let l1, r1 = to_smt_expr smt_ctx o1 in
    let l2, r2 = to_smt_expr smt_ctx o2 in
    let check = SmtEmitter.check_compliance smt_ctx in
    let eq_req = [ Z3.Boolean.mk_eq z3_ctx l1 l2; Z3.Boolean.mk_eq z3_ctx r1 r2 ] in
    let le_req = [ Z3.BitVector.mk_sle z3_ctx r1 l2 ] in
    let ge_req = [ Z3.BitVector.mk_sle z3_ctx l1 r2 ] in
    let subset_req = [Z3.BitVector.mk_sle z3_ctx l2 l1; Z3.BitVector.mk_sle z3_ctx r1 r2 ] in
    let supset_req = [Z3.BitVector.mk_sle z3_ctx l1 l2; Z3.BitVector.mk_sle z3_ctx r2 r1 ] in
    let loverlap_req = [Z3.BitVector.mk_sle z3_ctx l1 l2; Z3.BitVector.mk_sle z3_ctx l2 r1] in
    let roverlap_req = [Z3.BitVector.mk_sle z3_ctx l2 l1; Z3.BitVector.mk_sle z3_ctx l1 r2] in
    if check eq_req = SatYes then Eq
    else if check le_req = SatYes then Le
    else if check ge_req = SatYes then Ge
    else if check subset_req = SatYes then Subset
    else if check supset_req = SatYes then Supset
    else if check loverlap_req = SatYes then LOverlap
    else if check roverlap_req = SatYes then GOverlap
    else Other
    (* TODO: Maybe need to handle Other!!! *)

  let from_range (range_l_r: RangeExp.t * RangeExp.t) : t =
    match range_l_r with
    | Single l, Single r -> l, r
    | Single l, Range (_, r, _) -> l, r
    | Range (l, _, _), Single r -> l, r
    | Range (l, _, _), Range (_, r, _) -> l, r
    | l, r -> 
      mem_offset_error 
        (Printf.sprintf "from_range cannot convert %s %s" (RangeExp.to_string l) (RangeExp.to_string r))

  let insert_new_offset_list
      (smt_ctx: SmtEmitter.t)
      (ob_list: (t * bool) list) (new_o_list: t list) : (t * bool) list =
    (* We assume offset in offset_list is sorted from smaller to larger *)
    let rec insert_one_offset (ob_list: (t * bool) list) (new_o: t) : (t * bool) list =
      match ob_list with
      | [] -> [ (new_o, true) ]
      | (hd_o, hd_updated) :: tl ->
        begin match offset_cmp smt_ctx new_o hd_o with
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
          mem_offset_error 
            (Printf.sprintf "insert_one_offset fail compare new offset %s with known offset %s" 
              (to_string new_o) (to_string hd_o))
        end
    in
    List.fold_left insert_one_offset ob_list new_o_list

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

end

open Type.Ptr_info
open Type.Smt_emitter
open Type.Set_sexp
open Basic_type
open Mem_anno
open Z3
open Sexplib.Std

module MemOffset = struct
  exception MemOffsetError of string

  let mem_offset_error msg = raise (MemOffsetError ("[Mem Offset Error] " ^ msg))

  type t = DepType.exp_t * DepType.exp_t
  [@@deriving sexp]

  type off_rel_t =
    | Eq 
    | Subset | Supset 
    | Le | Ge 
    | LOverlap (* l1 <= l2 <= r1 <= r2 or l1 <= l2 <= r2 <= r1*)
    | GOverlap (* l2 <= l1 <= r2 <= r1 or l1 <= l2 <= r2 <= r1*)
    | Other
  [@@deriving sexp]

  type off_cmp_mode =
    | CmpEq
    | CmpSubset
    | CmpEqSubset
    | CmpOverlap
  [@@deriving sexp]

  let check_empty
      (smt_ctx: SmtEmitter.t)
      (o: t) : bool =
    let l, r = o in
    let ctx, _ = smt_ctx in
    let ge_req = [ BitVector.mk_sge ctx l r ] in
    let check = SmtEmitter.check_compliance smt_ctx in
    check ge_req = SatYes

  let check_adjacent
      (smt_ctx: SmtEmitter.t)
      (o1: t) (o2: t) : bool =
    let ctx, _ = smt_ctx in
    let _, r1 = o1 in
    let l2, _ = o2 in
    let r1p1 = BitVector.mk_add ctx r1 (DepType.get_const_exp ctx 1L (DepType.get_exp_bit_size r1)) in
    let check = SmtEmitter.check_compliance smt_ctx in
    check [ Boolean.mk_eq ctx r1p1 l2 ] = SatYes

  let offset_cmp
      (smt_ctx: SmtEmitter.t)
      (mode: off_cmp_mode)
      (o1: t) (o2: t) : off_rel_t =
    let ctx, _ = smt_ctx in
    let l1, r1 = o1 in
    let l2, r2 = o2 in
    let eq_req = [ Boolean.mk_eq ctx l1 l2; Boolean.mk_eq ctx r1 r2; ] in
    let le_req = [ BitVector.mk_sle ctx r1 l2 ] in
    let ge_req = [ BitVector.mk_sle ctx r2 l1 ] in
    let subset_req = [ BitVector.mk_sle ctx l2 l1; BitVector.mk_sle ctx r1 r2; ] in
    let supset_req = [ BitVector.mk_sle ctx l1 l2; BitVector.mk_sle ctx r2 r1 ] in
    let loverlap_req = [ BitVector.mk_sle ctx l1 l2; BitVector.mk_sle ctx l2 r1 ] in
    let goverlap_req = [ BitVector.mk_sle ctx l1 r2; BitVector.mk_sle ctx r2 r1 ] in

    let check = SmtEmitter.check_compliance smt_ctx in

    match mode with
    | CmpEq ->
      if check eq_req = SatYes then Eq
      else Other
    | CmpSubset ->
      if check subset_req = SatYes then Subset
      else Other
    | CmpEqSubset ->
      if check eq_req = SatYes then Eq
      else if check subset_req = SatYes then Subset
      else begin
        (* let e1 = List.hd subset_req in
        let e2 = List.hd (List.tl subset_req) in
        let result1 = check [e1] in
        let result2 = check [e2] in
        Printf.printf "subset(1): %s -> %s\nsubset(2): %s -> %s\n" 
          (Expr.to_string e1) (SmtEmitter.sexp_of_sat_result_t result1 |> Sexplib.Sexp.to_string_hum)
          (Expr.to_string e2) (SmtEmitter.sexp_of_sat_result_t result2 |> Sexplib.Sexp.to_string_hum); *)
        Other
      end
    | CmpOverlap ->
      if check eq_req = SatYes then Eq
      else if check subset_req = SatYes then Subset
      else if check supset_req = SatYes then Supset
      else if check loverlap_req = SatYes then LOverlap
      else if check goverlap_req = SatYes then GOverlap
      else if check le_req = SatYes then Le
      else if check ge_req = SatYes then Ge
      else Other

  let insert_new_offset_list_merge
      (smt_ctx: SmtEmitter.t)
      (old_list: t list) (new_list: t list) : t list =
    let rec insert_one_offset (old_list: t list) (new_o: t) : t list =
      match old_list with
      | [] -> [ new_o ]
      | hd_o :: tl ->
        begin match offset_cmp smt_ctx CmpOverlap new_o hd_o with
        | Eq | Subset -> hd_o :: tl
        | Supset -> insert_one_offset tl new_o
        | Le -> new_o :: hd_o :: tl
        | Ge -> hd_o :: (insert_one_offset tl new_o)
        | LOverlap ->
          let new_l, _ = new_o in
          let _, hd_r = hd_o in
          insert_one_offset tl (new_l, hd_r)
        | GOverlap ->
          let _, new_r = new_o in
          let hd_l, _ = hd_o in
          insert_one_offset tl (hd_l, new_r)
        | Other ->
          let hd_l, hd_r = hd_o in
          let new_l, new_r = new_o in
          let get_max (x: Z3.Expr.expr) (y: Z3.Expr.expr) : Z3.Expr.expr =
            let ctx = fst smt_ctx in
            let v = Z3.Expr.mk_fresh_const ctx "max" (Z3.Expr.get_sort x) in
            SmtEmitter.add_assertions smt_ctx [ Boolean.mk_and (fst smt_ctx) [
              Z3.BitVector.mk_sge ctx v x;
              Z3.BitVector.mk_sge ctx v y;
              Boolean.mk_or ctx [
                Boolean.mk_eq ctx v x;
                Boolean.mk_eq ctx v y;
              ]
            ]];
            v
          in
          let get_min (x: Z3.Expr.expr) (y: Z3.Expr.expr) : Z3.Expr.expr =
            let ctx = fst smt_ctx in
            let v = Z3.Expr.mk_fresh_const ctx "min" (Z3.Expr.get_sort x) in
            SmtEmitter.add_assertions smt_ctx [ Boolean.mk_and (fst smt_ctx) [
              Z3.BitVector.mk_sle ctx v x;
              Z3.BitVector.mk_sle ctx v y;
              Boolean.mk_or ctx [
                Boolean.mk_eq ctx v x;
                Boolean.mk_eq ctx v y;
              ]
            ]];
            v
          in
          let merge_l, merge_r = if offset_cmp smt_ctx CmpSubset (new_l, new_l) hd_o = Subset then
            (* let _ = Printf.printf "ok1: hd_l, max(hd_r, new_r) \n" in *)
            hd_l, get_max hd_r new_r
          else if offset_cmp smt_ctx CmpSubset (hd_l, hd_l) new_o = Subset then
            (* let _ = Printf.printf "ok2: new_l, max(hd_r, new_r) \n" in *)
            new_l, get_max hd_r new_r
          else if offset_cmp smt_ctx CmpSubset (new_r, new_r) hd_o = Subset then
            (* let _ = Printf.printf "ok3: min(new_l, hd_l), hd_r \n" in *)
            get_min new_l hd_l, hd_r
          else if offset_cmp smt_ctx CmpSubset (hd_r, hd_r) new_o = Subset then
            (* let _ = Printf.printf "ok4: min(new_l, hd_l), new_r \n" in *)
            get_min new_l hd_l, new_r
          else
            mem_offset_error "insert_new_offset_list_merge fail"
          in
          insert_one_offset tl (merge_l, merge_r)
        end
    in
    List.fold_left insert_one_offset old_list new_list

  let substitute
      (substitute_func: DepType.exp_t -> DepType.exp_t)
      (off: t) : t =
    let l, r = off in
    substitute_func l, substitute_func r

  let addr_size_to_offset
      (ctx: context)
      (addr_exp: DepType.exp_t) (size: int64) : t =
    addr_exp,
    BitVector.mk_add ctx addr_exp (DepType.get_const_exp ctx size 64)

  let offset_get_start_len
      (smt_ctx: SmtEmitter.t)
      (base: DepType.exp_t)
      (off: t) : (int64 * int64) option =
    let ctx, _ = smt_ctx in
    let l, r = off in
    let start = BitVector.mk_sub ctx l base in
    let len = BitVector.mk_sub ctx r l in
    match DepType.to_common_const smt_ctx start with
    | Some start_int ->
      begin match DepType.to_common_const smt_ctx len with
      | Some len_int -> Some (start_int, len_int)
      | None -> None
      end
    | _ -> None

  let get_offset_top
      (smt_ctx: SmtEmitter.t)
      (off: t) : DepType.t =
    (* Get dep type top with proper size. If size is not const, return Top 0 *)
    let ctx, _ = smt_ctx in
    let l, r = off in
    let size_exp = BitVector.mk_sub ctx r l in
    match DepType.to_common_const smt_ctx size_exp with
    | Some size_byte -> Top ((Int64.to_int size_byte) * 8)
    | None -> Top DepType.top_unknown_size

  let sort_offset_list
      (smt_ctx: SmtEmitter.t)
      (off_list: t list) : t list =
    let off_cmp (x: t) (y: t) =
      match offset_cmp smt_ctx CmpOverlap x y with
      | Eq | Subset | Supset | LOverlap | GOverlap | Other -> 0
      | Le -> -1
      | Ge -> 1
    in
    List.sort off_cmp off_list

  let check_offset_non_change
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (base_ptr: int) (off: t) : bool =
    (* Goal: check isNonChangeExp (l - ptr) and isNonChangeExp (l - r) *)
    let ctx, _ = smt_ctx in
    let l, r = off in
    let ptr = Z3.Expr.mk_const_s ctx ("s" ^ (string_of_int base_ptr)) (Z3.BitVector.mk_sort ctx (8 * 8)) in
    let off_exp = Z3.BitVector.mk_sub ctx l ptr in
    let len_exp = Z3.BitVector.mk_sub ctx r l in
    let off_non_change = is_non_change_exp off_exp in
    let len_non_change = is_non_change_exp len_exp in
    (* Printf.printf "Ptr: %d, Off:\n%s\n" base_ptr (Sexplib.Sexp.to_string_hum (sexp_of_t off)); *)
    if not (off_non_change && len_non_change) then begin
      Printf.printf "Ptr: %d, Off:\n%s\n" base_ptr (Sexplib.Sexp.to_string_hum (sexp_of_t off));
      mem_offset_error "check_offset_non_change fail"
    end else true

end

module MemRange = struct
  exception MemRangeError of string

  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type t = MemOffset.t list
  [@@deriving sexp]

  let merge (smt_ctx: SmtEmitter.t) (r1: t) (r2: t) : t =
    (* Printf.printf "merge:\n%s\n%s\n"
      (sexp_of_t r1 |> Sexplib.Sexp.to_string_hum)
      (sexp_of_t r2 |> Sexplib.Sexp.to_string_hum); *)
    MemOffset.insert_new_offset_list_merge smt_ctx r1 r2

  let sanitize (smt_ctx: SmtEmitter.t) (ranges: t) : t =
    let ranges = List.filter_map (fun range ->
      if MemOffset.check_empty smt_ctx range then None (* remove empty ranges *)
      else Some range
    ) ranges
    in
    let ranges = MemOffset.sort_offset_list smt_ctx ranges in
    (* Printf.printf "ranges before merge:\n%s\n" (sexp_of_t ranges |> Sexplib.Sexp.to_string_hum); *)
    (* combine adjacent ranges *)
    let rec helper (o_list: t) : t =
      match o_list with
      | [] -> []
      | [x] -> [x]
      | x1 :: x2 :: tl ->
        if MemOffset.check_adjacent smt_ctx x1 x2 then begin
          (fst x1, snd x2) :: helper tl
        end else begin
          x1 :: helper (x2 :: tl)
        end
    in
    let ranges = helper ranges in
    (* Printf.printf "ranges after merge:\n%s\n" (sexp_of_t ranges |> Sexplib.Sexp.to_string_hum); *)
    ranges

  let substitute
      (smt_ctx: SmtEmitter.t)
      (substitute_func: DepType.exp_t -> DepType.exp_t)
      (ranges: t) : t =
    let ranges = List.map (MemOffset.substitute substitute_func) ranges in
    sanitize smt_ctx ranges

  let is_empty (r: t) : bool = r = []

  let get_boundary (r: t) : MemOffset.t =
    match r with
    | [] -> mem_range_error "get_boundary on empty range"
    | [x] -> x
    | l :: rest ->
      let r = List.nth rest ((List.length rest) - 1) in
      fst l, snd r

  let check_subset
      (smt_ctx: SmtEmitter.t)
      (r1: t) (r2: t) : bool =
    let r1 = sanitize smt_ctx r1 in
    let r2 = sanitize smt_ctx r2 in
    (* Check whether r1 is a subset of r2*)
    let off_check_subset = MemOffset.offset_cmp smt_ctx CmpSubset in
    let remain_off_list =
      List.fold_left (
        fun (remain_off_list: t) (sup_off: MemOffset.t) ->
          let _, remain_off_opt_list =
            List.fold_left_map (
              fun (acc: bool) (sub_off: MemOffset.t) ->
                if acc then
                  if off_check_subset sub_off sup_off = Subset then
                    true, None
                  else false, Some sub_off
                else false, Some sub_off
            ) true remain_off_list
          in
          List.filter_map (fun x -> x) remain_off_opt_list
      ) r1 r2
    in
    remain_off_list = []

  let check_single_slot_eq
      (smt_ctx: SmtEmitter.t)
      (r1: t) (r2: t) : bool =
    (* Only in this case we will track detailed dep types of memory slots *)
    match r1, r2 with
    | [], [] -> true
    | [ o1 ], [ o2 ] ->
      MemOffset.offset_cmp smt_ctx CmpEq o1 o2 = Eq
    | _ -> false

end

module MemType = struct
  exception MemTypeError of string

  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type 'a mem_slot = MemOffset.t * bool * MemRange.t * 'a
  [@@deriving sexp]

  type 'a mem_part = PtrInfo.t * (('a mem_slot) list)
  [@@deriving sexp]

  type 'a mem_content = ('a mem_part) list
  [@@deriving sexp]

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = entry_t mem_content
  [@@deriving sexp]

  (* making debug output easier *)

  type entry_mem_slot = entry_t mem_slot
  [@@deriving sexp]

  type mem_anno_mem_slot = MemAnno.slot_t mem_slot
  [@@deriving sexp]

  let map_full
      (func: 'a mem_slot -> 'b mem_slot)
      (mem: 'a mem_content) : 'b mem_content =
    let helper_outer
        (entry: 'a mem_part) :
        'b mem_part =
      let ptr, part_mem = entry in
      ptr, List.map func part_mem
    in
    List.map helper_outer mem

  let fold_left_full
      (func: 'acc -> 'a mem_slot -> 'acc)
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc =
    let helper_outer
        (acc: 'acc)
        (entry: 'a mem_part) : 'acc =
      let _, l = entry in
      List.fold_left func acc l
    in
    List.fold_left helper_outer acc mem

  let fold_left2_full
      (func: 'acc -> 'a mem_slot -> 'b mem_slot -> 'acc)
      (acc: 'acc)
      (mem1: 'a mem_content)
      (mem2: 'b mem_content) : 'acc =
    let helper_outer
        (acc: 'acc)
        (entry1: 'a mem_part)
        (entry2: 'b mem_part) : 'acc =
      let (v1, _), l1 = entry1 in
      let (v2, _), l2 = entry2 in
      if v1 = v2 then List.fold_left2 func acc l1 l2
      else mem_type_error "[fold_left2_full] ptr does not match"
    in
    List.fold_left2 helper_outer acc mem1 mem2

  let get_single_slot
      (mem: 'a mem_content)
      (slot_info: MemAnno.slot_t) :
      PtrInfo.t * 'a mem_slot =
    let slot_ptr, slot_idx, _, num_slot = slot_info in
    if num_slot <> 1 then mem_type_error "get_single_slot cannot get multiple slots"
    else
      match List.find_opt (fun ((ptr, _), _) -> ptr = slot_ptr) mem with
      | None -> mem_type_error (Printf.sprintf "get_single_slot cannot find ptr %d" slot_ptr)
      | Some (ptr_info, part_mem) ->
        ptr_info,
        List.nth part_mem slot_idx

  let get_single_slot_with_info
      (mem: 'a mem_content)
      (slot_info: MemAnno.slot_t) :
      PtrInfo.t * 'a mem_slot =
    let ptr_info, (off, forget_type, range, entry) = get_single_slot mem slot_info in
    ptr_info, (off, forget_type, range, entry)

  let check_non_overlap
      (smt_ctx: SmtEmitter.t)
      (mem: 'a mem_content) : bool =
    (* The following logic is copied from infer's get_mem_non_overlap_constraint *)
    let ctx, _ = smt_ctx in
    let get_off_list_begin_end (slot_list: 'a mem_slot list) : MemOffset.t =
      match slot_list with
      | [] -> mem_type_error "get empty slot list"
      | (hd, _, _, _) :: [] -> hd
      | ((l, _), _, _, _) :: tl ->
        let (_, r), _, _, _ = List.nth tl ((List.length tl) - 1) in
        l, r
    in
    let get_two_off_non_overlap_constraint
        (off1: MemOffset.t) (off2: MemOffset.t) :
        DepType.exp_t =
      let l1, r1 = off1 in
      let l2, r2 = off2 in
      Boolean.mk_or ctx [
        (BitVector.mk_sle ctx r1 l2);
        (BitVector.mk_sle ctx r2 l1);
      ]
    in
    let get_part_mem_non_overlap_constraint_helper
        (mem1: MemOffset.t)
        (overlap_info_map: PtrInfo.InfoMap.t)
        (ptr2: int)
        (mem2: 'a mem_slot list) : DepType.exp_t list =
      let add_to_list_if_not_none
          (x: 'b option) (other: 'b list) : 'b list =
        match x with
        | None ->  other
        | Some x -> x :: other
      in
      match PtrInfo.InfoMap.find_opt ptr2 overlap_info_map with
      | None -> (* all slots in mem2 should not be overlapped with mem1 *)
        [ get_two_off_non_overlap_constraint mem1 (get_off_list_begin_end mem2) ]
      | Some All -> [] (* all slots in mem2 are overlapped with mem1 *)
      | Some (SlotSet overlap_slot_set) ->
        let _, curr_group_opt, mem2_non_overlap_list =
          List.fold_left (
            fun (acc: int * (MemOffset.t option) * (MemOffset.t list)) (slot: 'a mem_slot) ->
              let idx, curr_group_opt, acc_list = acc in
              let off, _, _, _ = slot in
              if IntSet.mem idx overlap_slot_set then
                (idx + 1, None, add_to_list_if_not_none curr_group_opt acc_list)
              else
                (idx + 1, Option.fold ~none:(Some off) ~some:(fun curr_group -> Some (fst curr_group, snd off)) curr_group_opt, acc_list)
              (* match curr_group_opt with
              | None -> 
                if IntSet.mem idx overlap_slot_set then
                  idx + 1, None, acc_list
                else
                  idx + 1, Some off, acc_list
              | Some curr_group ->
                if IntSet.mem idx overlap_slot_set then
                  idx + 1, None, curr_group :: acc_list
                else
                  idx + 1, Some (fst curr_group, snd off), acc_list *)
          ) (0, None, []) mem2
      in
      let mem2_non_overlap_list = add_to_list_if_not_none curr_group_opt mem2_non_overlap_list in
      List.map (get_two_off_non_overlap_constraint mem1) mem2_non_overlap_list
    in
    let get_part_mem_non_overlap_constraint
        (mem1: 'a mem_part) (mem2: 'a mem_part) : DepType.exp_t list =
      let ((ptr1, info1), slot_list1) = mem1 in
      let ((ptr2, info2), slot_list2) = mem2 in
      match info1, info2 with
      | Unified (overlap_info_map1, _, _), _ ->
        get_part_mem_non_overlap_constraint_helper (get_off_list_begin_end slot_list1) overlap_info_map1 ptr2 slot_list2
      | _, Unified (overlap_info_map2, _, _) ->
        get_part_mem_non_overlap_constraint_helper (get_off_list_begin_end slot_list2) overlap_info_map2 ptr1 slot_list1
      | Separate info_list1, _ ->
        List.map2 (
          fun (overlap_info_map1, _, _) (off, _, _, _) ->
            get_part_mem_non_overlap_constraint_helper off overlap_info_map1 ptr2 slot_list2
        ) info_list1 slot_list1 |> List.concat
    in
    let rec get_mem_non_overlap_constraint 
        (mem_type: t) : DepType.exp_t list =
      (* get_mem_non_overlap_constraint_helper [] (get_mem_boundary_list mem_type) *)
      match mem_type with
      | [] -> []
      | hd :: tl ->
        get_mem_non_overlap_constraint tl @
        (List.concat_map (get_part_mem_non_overlap_constraint hd) tl)
    in

    let constraint_list = get_mem_non_overlap_constraint mem in

    (* Printf.printf "check_non_overlap: constraint list:\n";
    List.iter (fun x -> Printf.printf "\t%s\n" (Expr.to_string x)) constraint_list;
    Printf.printf "solver:\n%s\n" (SmtEmitter.to_string smt_ctx); *)
    SmtEmitter.check_compliance smt_ctx constraint_list = SatYes

  let check_valid_region
      (smt_ctx: SmtEmitter.t)
      (mem: 'a mem_content) : bool =
    List.find_opt (
      fun (_, part_mem) ->
        List.find_opt (
          fun (off, _, range, _) ->
            not (MemRange.check_subset smt_ctx range [off])
        ) part_mem <> None
    ) mem = None

  let check_slot_subtype
      (smt_ctx: SmtEmitter.t)
      (off_must_eq: bool)
      (is_non_change_exp: DepType.exp_t -> bool)
      (sub_slot: entry_t mem_slot)
      (sup_slot: entry_t mem_slot) : bool =
    (* for offset / initialized ranges, sup_slot is the subset of sub_slot *)
    (* for types, sub_slot is the subset of sup_slot *)
    let offset_cmp = MemOffset.offset_cmp smt_ctx in
    let sub_off, sub_forget_type, sub_range, (sub_dep, sub_taint) = sub_slot in
    let sup_off, sup_forget_type, sup_range, (sup_dep, sup_taint) = sup_slot in

    (*
    Printf.printf "check_slot_subtype %b:\n" off_must_eq;
    Printf.printf "sub_slot: %s %s %s %s %s\n" (MemOffset.sexp_of_t sub_off |> Sexplib.Sexp.to_string_hum)
      (string_of_bool sub_forget_type)
      (MemRange.sexp_of_t sub_range |> Sexplib.Sexp.to_string_hum)
      (DepType.sexp_of_t sub_dep |> Sexplib.Sexp.to_string_hum)
      (TaintType.sexp_of_t sub_taint |> Sexplib.Sexp.to_string_hum);
    Printf.printf "sup_slot: %s %s %s %s %s\n" (MemOffset.sexp_of_t sup_off |> Sexplib.Sexp.to_string_hum)
      (string_of_bool sup_forget_type)
      (MemRange.sexp_of_t sup_range |> Sexplib.Sexp.to_string_hum)
      (DepType.sexp_of_t sup_dep |> Sexplib.Sexp.to_string_hum)
      (TaintType.sexp_of_t sup_taint |> Sexplib.Sexp.to_string_hum);
    *)
    (* Printf.printf "context:\n%s\n" (SmtEmitter.to_string smt_ctx); *)

    (* We add strongest constraint for whether corressponding slots should allow forget type - they should agree on this property. *)
    if not sub_forget_type = sup_forget_type then false else
    let check_taint () : bool = TaintType.check_subtype smt_ctx true sub_taint sup_taint in
    let offset_cmp_option, offset_cmp_goal =
      if off_must_eq then MemOffset.CmpEq, MemOffset.Eq
      else MemOffset.CmpSubset, MemOffset.Subset
    in
    if offset_cmp offset_cmp_option sup_off sub_off = offset_cmp_goal then
      if MemRange.is_empty sup_range then
        (* In this case we do not check dep *)
        (* Empty range => overwrite in sup block or future => taint can be anything if allow forget valid range/taint (e.g., for spill) *)
        if sub_forget_type then true
        else check_taint ()
      else if MemRange.check_single_slot_eq smt_ctx sub_range sup_range then
        DepType.check_subtype smt_ctx is_non_change_exp sub_dep sup_dep &&
        check_taint ()
      else
        let check_dep =
          (* TODO: Double check this fix! Originally we check sub_dep is top by mistake *)
          match sub_dep, sup_dep with
          | Top _, Top _ -> true
          | Exp sub_e, Top _ -> is_non_change_exp sub_e
          | _ -> false
        in
        MemRange.check_subset smt_ctx sup_range sub_range &&
        check_dep &&
        check_taint ()
    else false

  let imply_helper (sub: bool) (sup: bool) : bool =
      (* sub => sup *)
      (not sub) || sup

  let check_subtype_no_map
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (sub_m_type: t) (sup_m_type: t) : bool =
    (* Items to check:
       1. Ptr info
       2. MemRange (s_val)
       3. BasicType *)
    List.fold_left2 (
      fun (acc: bool) (sub: entry_t mem_part) (sup: entry_t mem_part) ->
        if not acc then acc else
        let (sub_ptr, sub_info), sub_part_mem = sub in
        let (sup_ptr, sup_info), sup_part_mem = sup in
        sub_ptr = sup_ptr &&
        PtrInfo.check_subtype_info false sub_info sup_info && 
        (* (1) might overlap map is checked to be the same; (2) permission is checked to satisfy target (sup) => current (sup) *)
        List.fold_left2 (
          fun (acc: bool * int) (sub_slot: entry_t mem_slot) (sup_slot: entry_t mem_slot) ->
            let acc_check, slot_idx = acc in
            if not acc_check then acc_check, slot_idx + 1
            else 
              let slot_result = check_slot_subtype smt_ctx true is_non_change_exp sub_slot sup_slot in
              (* Printf.printf "check_subtype_map: result %b\n" slot_result; *)
              slot_result, slot_idx + 1
        ) (true, 0) sub_part_mem sup_part_mem |> fst
    ) true sub_m_type sup_m_type

  let apply_mem_map
      (mem_type: 'a mem_content)
      (mem_map: MemAnno.slot_t mem_content) :
      'a mem_content =
    (* In this function, we only guarantee the returned info has correct read/write permission, but may have unreasonable might_overlap_map!!! *)
    List.map (
      fun (_, part_mem_map) ->
        let ptr_info_opt, new_slot_info_part_mem =
          List.fold_left_map (
            fun (acc: PtrInfo.t option) (_, _, _, slot_info) ->
              let (curr_ptr, curr_ptr_info), new_slot = get_single_slot_with_info mem_type slot_info in
              let _, slot_idx, _, _ = slot_info in
              let slot_one_info = PtrInfo.get_nth_slot_one_info curr_ptr_info slot_idx in
              match acc with
              | None -> Some (curr_ptr, curr_ptr_info), (slot_one_info, new_slot)
              | Some (acc_ptr, _) ->
                if acc_ptr = curr_ptr then acc, (slot_one_info, new_slot)
                else mem_type_error "apply_mem_map corresponding ptr of slots in one part_mem do not match"
          ) None part_mem_map
        in
        let slot_info_list, new_part_mem = List.split new_slot_info_part_mem in
        let ptr, orig_ptr_info (* info before map, contain info for all slots *) = Option.get ptr_info_opt in
        (ptr, PtrInfo.group_slot_one_info orig_ptr_info slot_info_list), new_part_mem
        (* Option.get ptr_info_opt, new_part_mem *)
    ) mem_map

  let check_subtype_map
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (sub_m_type: t) (sup_m_type: t)
      (mem_map: MemAnno.slot_t mem_content) : bool =
    (*
    Printf.printf "check_subtype_map:\n";
    Printf.printf "sub_m_type:\n%s\n" (sexp_of_t sub_m_type |> Sexplib.Sexp.to_string_hum);
    Printf.printf "sup_m_type:\n%s\n" (sexp_of_t sup_m_type |> Sexplib.Sexp.to_string_hum);
    *)
    (* This is used for function call.
       Items to check:
       1. Ptr info
          1. Ptr -> not check
             1. The ptr is not substituted with the context map (we can, but it requires extra calculation)
             2. If offset matches, then ptr (after context map) is automatically checked.
                Ptr is something for quick reference or the ease of gathering information like non-overlap, read/write.
          2. Ptr overlap set ->
             1. The information is encoded in func interface's in_context, 
                checked by func interface well formness <TODO>
             2. The in_context is checked in arch_type subtype check\
          3. read, write -> check
       2. MemOffset (s_alloc) and MemRange (s_val)
       3. BasicType *)
    (* Note: apply_mem_map ruin the overlap ptr set field,
       since the result sub_map_m_type may have repeated ptr, while the original overlap ptr set
       cannot represent the possibly overlap/non-overlap relation.
       But the good thing is that we do not need to check overlap issue here. It is included in the target context *)
    let sub_map_m_type = apply_mem_map sub_m_type mem_map in
    (* Printf.printf "sub_map_m_type:\n%s\n" (sexp_of_t sub_map_m_type |> Sexplib.Sexp.to_string_hum); *)
    List.fold_left2 (
      fun (acc: bool) (sub: entry_t mem_part) (sup: entry_t mem_part) ->
        if not acc then acc else
        let (sub_ptr, sub_info), sub_part_mem = sub in
        let (_, sup_info), sup_part_mem = sup in
        (* We only check permission is checked to satisfy target (sup) => current (sup) *)
        let check_imp = PtrInfo.check_subtype_info true sub_info sup_info in
        let check_mem = List.fold_left2 (
          fun (acc_check: bool) 
              (sub_slot: entry_t mem_slot)
              (sup_slot: entry_t mem_slot) ->
            if not acc_check then acc_check
            else
              (* Check sup slot off - sup ptr is non change *)
              let sup_off, _, _, _ = sup_slot in
              let non_change_off = MemOffset.check_offset_non_change smt_ctx is_non_change_exp sub_ptr sup_off in
              (* Check slot subtype *)
              let slot_result = check_slot_subtype smt_ctx false is_non_change_exp sub_slot sup_slot in
              (* Printf.printf "check_subtype_map: result %b\n" slot_result; *)
              slot_result && non_change_off
        ) true sub_part_mem sup_part_mem
        in
        (* Printf.printf "check_subtype_map: ptr %d check_imp1 %b, check_imp2 %b, check_mem %b\n" (fst (fst sub)) check_imp1 check_imp2 check_mem; *)
        check_imp && check_mem
    ) true sub_map_m_type sup_m_type

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (sub_m_type: t) (sup_m_type: t)
      (mem_map_opt: MemAnno.slot_t mem_content option) : bool =
    match mem_map_opt with
    | None -> check_subtype_no_map smt_ctx is_non_change_exp sub_m_type sup_m_type
    | Some mem_map -> check_subtype_map smt_ctx is_non_change_exp sub_m_type sup_m_type mem_map

  let find_part_mem_helper
      (mem_type: t) (slot_info: MemAnno.slot_t) : entry_t mem_part =
    let slot_ptr, _, _, _ = slot_info in
    let part_mem_opt =
      List.find_opt (fun ((ptr, _), _) -> ptr = slot_ptr) mem_type
    in
    match part_mem_opt with
    | None -> mem_type_error (Printf.sprintf "Cannot find ptr %d" slot_ptr)
    | Some part_mem -> part_mem

  let get_one_slot_type
      (smt_ctx: SmtEmitter.t)
      (slot: entry_t mem_slot)
      (addr_off: MemOffset.t) : entry_t option =
    (* Printf.printf "get_one_slot_type:\ntarget address: %s\nslot: %s\n"
      (addr_off |> MemOffset.sexp_of_t |> Sexplib.Sexp.to_string_hum)
      (slot |> sexp_of_entry_mem_slot |> Sexplib.Sexp.to_string_hum); *)
    let ctx, _ = smt_ctx in
    let _, _, slot_range, (slot_dep, slot_taint) = slot in
    match slot_range with
    | [] ->
      Printf.printf "Warning: read from an invalid entry\n";
      None
    | [ valid_l, valid_r ] ->
      let cmp_off = 
        MemOffset.offset_cmp smt_ctx CmpEqSubset addr_off (valid_l, valid_r)
      in
      begin match cmp_off with
      | Eq -> Some (slot_dep, slot_taint)
      | Subset ->
        begin match MemOffset.offset_get_start_len smt_ctx valid_l addr_off with
        | None -> Some (MemOffset.get_offset_top smt_ctx addr_off, slot_taint)
        | Some (start, len) ->
          Some (DepType.get_start_len ctx start len slot_dep, slot_taint)
        end
      | _ ->
        Printf.printf "Warning: get_one_slot_type failed, relationship=%s\naddr_off=%s\n"
          (MemOffset.sexp_of_off_rel_t cmp_off |> Sexplib.Sexp.to_string_hum)
          (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off));
        Printf.printf "%s\n" (SmtEmitter.to_string smt_ctx);
        None
      end
    | _ ->
      if MemRange.check_subset smt_ctx [addr_off] slot_range then
        Some (MemOffset.get_offset_top smt_ctx addr_off, slot_taint)
      else begin
        Printf.printf "Warning: get_one_slot_type addr_off %s does not belongs to entry range %s\n"
          (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off))
          (Sexplib.Sexp.to_string_hum (MemRange.sexp_of_t slot_range));
        None
      end

  let get_mult_slot_type
      (smt_ctx: SmtEmitter.t)
      (slot_list: entry_t mem_slot list)
      (addr_off: MemOffset.t) : entry_t option =
    let get_one_entry
        (acc: (DepType.exp_t * entry_t) option) 
        (* right boundary of last off,
           merged type *)
        (slot: entry_t mem_slot) :
        (DepType.exp_t * entry_t) option =
      match acc with
      | None -> None
      | Some (acc_r, acc_merged_type) ->
        let _, _, slot_range, slot_type = slot in
        begin match slot_range with
        | [ slot_l, slot_r ] ->
          let slot_continuous = DepType.check_eq smt_ctx acc_r slot_l in
          if slot_continuous then
            match BasicType.concat_same_taint smt_ctx slot_type acc_merged_type with
            | Some merged_type -> Some (slot_r, merged_type)
            | None -> 
              Printf.printf "Warning: get_mult_slot_type has different taint types\n";
              None
          else begin
            Printf.printf "Warning: get_mult_slot_type slot not continuous\n";
            None
          end
        | _ ->
          Printf.printf "Warning: get_mult_slot_type valid range format unrecognized\n";
          None
        end
    in
    let addr_l, addr_r = addr_off in
    match slot_list with
    | [] -> mem_type_error "get_mult_slot_type: should not have empty slot_list"
    | (_, _, [ hd_l, hd_r ], hd_type) :: tl ->
      let slot_left_aligned = DepType.check_eq smt_ctx addr_l hd_l in
      if slot_left_aligned then begin
        let result = List.fold_left get_one_entry (Some (hd_r, hd_type)) tl in
        match result with
        | None -> None
        | Some (acc_r, merged_type) ->
          let slot_right_aligned = DepType.check_eq smt_ctx addr_r acc_r in
          if slot_right_aligned then Some merged_type
          else begin
            Printf.printf "Warning: get_mult_slot_type last entry not right aligned with addr_off\n";
            None
          end      
      end else begin
        Printf.printf "Warning: get_mult_slot_type first entry not left aligned with addr_off\n";
        None
      end
    | _ -> None

  let get_part_mem
      (smt_ctx: SmtEmitter.t)
      (part_mem: entry_t mem_slot list)
      (slot_info: MemAnno.slot_t)
      (addr_off: MemOffset.t) : entry_t option =
    let _, slot_idx, _, num_slots = slot_info in
    if List.length part_mem < slot_idx + num_slots then begin
      Printf.printf "Warning: get_part_mem cannot find %d slot(s) starting from idx %d\n" 
        num_slots slot_idx;
      None
    end else
    if num_slots = 1 then get_one_slot_type smt_ctx (List.nth part_mem slot_idx) addr_off
    else
      let matching_slots =
        List.filteri (fun i _ -> i >= slot_idx && i < slot_idx + num_slots) part_mem
      in
      get_mult_slot_type smt_ctx matching_slots addr_off

  let get_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (mem_type: t)
      (addr_off: MemOffset.t) 
      (slot_info: MemAnno.slot_t) : entry_t option =
    (* 1. Check read permission; 
       2. get entry with slot; 
       3. check add_off *)
    let ptr_info, part_mem = find_part_mem_helper mem_type slot_info in
    let _, slot_idx, _, slot_num = slot_info in (* ptr = slot_ptr *)
    if not (PtrInfo.can_read_slot_range slot_idx slot_num ptr_info) then begin
      Printf.printf "Warning: get_mem_type read slot %s permission is not satisfied.\n"
        (Sexplib.Sexp.to_string_hum (MemAnno.sexp_of_slot_t slot_info));
      None
    end else if not (MemOffset.check_offset_non_change smt_ctx is_non_change_exp (fst ptr_info) addr_off) then None 
    else
      (* Check addr_off and get entry *)
      get_part_mem smt_ctx part_mem slot_info addr_off

  let set_one_slot_type
      (smt_ctx: SmtEmitter.t)
      (slot: entry_t mem_slot)
      (addr_off: MemOffset.t)
      (is_slot_full_mapped: bool)
      (new_type: BasicType.t) : entry_t mem_slot option =
    (* 1. Update entry (valid region and type)
       2. Check slot_off, taint constraint *)
    let slot_off, slot_forget_type, slot_range, (_, slot_taint) = slot in
    (* Printf.printf "set_one_slot_type:\n\tslot off: %s\n\tslot range: %s\n\taddr_off: %s\n"
      (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t slot_off))
      (Sexplib.Sexp.to_string_hum (MemRange.sexp_of_t slot_range))
      (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off)); *)
    if slot_forget_type then begin
      (* Check slot_off *)
      if MemOffset.offset_cmp smt_ctx CmpSubset addr_off slot_off = Subset then begin
        (* For slot that allow forget valid range/taint (e.g., spill), we overwrite the valid region with write range. *)
        Some (slot_off, slot_forget_type, [ addr_off ], new_type)
      end else begin
        Printf.printf "Warning: %s is not subset of %s\n" 
          (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off)) 
          (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t slot_off));
        None
      end
    end else begin
      (* Check slot_off *)
      let cmp_off = MemOffset.offset_cmp smt_ctx CmpEqSubset addr_off slot_off in
      (* Check taint *)
      let _, new_taint = new_type in
      let check_taint = TaintType.check_subtype smt_ctx true new_taint slot_taint in
      if not check_taint then begin
        Printf.printf "Warning: new taint not equal to old taint %s <> %s\n"
        (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t new_taint))
        (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t slot_taint));
        None
      end else begin
        if cmp_off = Eq then begin
          Some (slot_off, slot_forget_type, [ slot_off ], new_type)
        end else if cmp_off = Subset then begin
          let new_range = if is_slot_full_mapped then
            let _ = Printf.printf "override range with %s\n"
              (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off)) in
            [addr_off]
          else
            MemRange.merge smt_ctx slot_range [addr_off]
          in
          Some (slot_off, slot_forget_type, new_range, (Top DepType.top_unknown_size, new_taint))
        end else begin
          Printf.printf "Warning: %s is not subset of %s\n" 
            (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t addr_off)) 
            (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t slot_off));
          None
        end
      end
    end

  let set_mult_slot_type
      (smt_ctx: SmtEmitter.t)
      (set_range: int * int)
      (slot_list: entry_t mem_slot list)
      (addr_off: MemOffset.t)
      (new_type: BasicType.t) : (entry_t mem_slot list) option =
    let addr_l, addr_r = addr_off in
    let new_dep, new_taint = new_type in
    let is_set (idx: int) : bool =
      idx >= fst set_range && idx < snd set_range
    in
    let set_one_slot
        (acc: (int * DepType.exp_t) option)
        (slot: entry_t mem_slot) : ((int * DepType.exp_t) option) * (entry_t mem_slot) =
      match acc with
      | None -> None, slot
      | Some (idx, acc_r) ->
        if is_set idx then begin
          (* Here we ignore slot_forget_type for simplicity since it always overwrites the full slot,
            and we do not allow change of taint for simplicity. 
            If needed, we can skip check taint and directly overwrite taint if slot_forget_type is true. *)
          let (slot_l, slot_r), slot_forget_type, _, (_, slot_taint) = slot in
          let slot_continuous = DepType.check_eq smt_ctx acc_r slot_l in
          if slot_continuous then begin
            let check_taint = TaintType.check_subtype smt_ctx true new_taint slot_taint in
            if not check_taint then begin
              Printf.printf "Warning: new taint not equal to old taint %s <> %s\n"
                (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t new_taint))
                (Sexplib.Sexp.to_string_hum (TaintType.sexp_of_t slot_taint));
                None, slot
            end else begin
              match MemOffset.offset_get_start_len smt_ctx addr_l (slot_l, slot_r) with
              | Some (start_byte, len_byte) ->
                Some (idx + 1, slot_r),
                ((slot_l, slot_r), slot_forget_type, [slot_l, slot_r], 
                (DepType.get_start_len (fst smt_ctx) start_byte len_byte new_dep, new_taint))
              | None ->
                Some (idx + 1, slot_r),
                ((slot_l, slot_r), slot_forget_type, [slot_l, slot_r], (Top DepType.top_unknown_size, new_taint))
            end
          end else begin
            Printf.printf "Warning: set_mult_slot_type slot not continuous\n";
            None, slot
          end
        end else Some (idx + 1, acc_r), slot
    in
    let acc, new_slot_list =
      List.fold_left_map set_one_slot (Some (0, addr_l)) slot_list
    in
    match acc with
    | None -> None
    | Some (_, acc_r) ->
      if DepType.check_eq smt_ctx addr_r acc_r then
        Some new_slot_list
      else None

  let set_part_mem
      (smt_ctx: SmtEmitter.t)
      (part_mem: entry_t mem_slot list)
      (addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: BasicType.t) : (entry_t mem_slot list) option =
    (* 1. Update entry
       2. Check slot_info, taint constraint *)
    let _, slot_idx, is_slot_full_mapped, num_slots = slot_info in
    if List.length part_mem < slot_idx + num_slots then begin
      Printf.printf "Warning: set_part_mem cannot find %d slot(s) starting from idx %d\n" 
        num_slots slot_idx;
      None
    end else
    if num_slots = 1 then
      let acc, new_part_mem =
        List.fold_left_map (
          fun (acc: int option) (slot: entry_t mem_slot) ->
            match acc with
            | None -> None, slot
            | Some acc_idx ->
              if acc_idx <> slot_idx then Some (acc_idx + 1), slot
              else begin
                match set_one_slot_type smt_ctx slot addr_off is_slot_full_mapped new_type with
                | None -> None, slot
                | Some new_slot -> Some (acc_idx + 1), new_slot
              end
        ) (Some 0) part_mem
      in
      if acc <> None then Some new_part_mem
      else None
    else
      set_mult_slot_type smt_ctx (slot_idx, slot_idx + num_slots) part_mem addr_off new_type

  let set_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (mem_type: t)
      (addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: BasicType.t) : t option =
    (* 1. Check write permission; 
       2. update entry and all read/write permission; 
       3. check slot_info, taint constraint *)
    let (ptr, ptr_info), part_mem = find_part_mem_helper mem_type slot_info in
    let _, slot_idx, _, slot_num = slot_info in (* ptr = slot_ptr *)
    if not (PtrInfo.can_write_slot_range_info slot_idx slot_num ptr_info) then begin
      Printf.printf "Warning: get_mem_type read slot %s permission is not satisfied.\n"
        (Sexplib.Sexp.to_string_hum (MemAnno.sexp_of_slot_t slot_info));
      None
    end else if not (MemOffset.check_offset_non_change smt_ctx is_non_change_exp ptr addr_off) then None 
    else
      let new_part_mem_opt = set_part_mem smt_ctx part_mem addr_off slot_info new_type in
      match new_part_mem_opt with
      | None -> None
      | Some new_part_mem ->
        let new_mem_type = List.map (
          fun ((p, p_info), p_part_mem) ->
            if p = ptr then (p, p_info), new_part_mem
            else PtrInfo.invalidate_on_write_slot_range ptr slot_idx slot_num (p, p_info), p_part_mem
        ) mem_type in
        (* Printf.printf "mem before:\n%s\nmem_after:\n%s\n"
          (Sexplib.Sexp.to_string_hum (sexp_of_t mem_type))
          (Sexplib.Sexp.to_string_hum (sexp_of_t new_mem_type)); *)
        Some (new_mem_type)

  let set_mem_type_with_other
      (smt_ctx: SmtEmitter.t)
      (is_non_change_exp: DepType.exp_t -> bool)
      (mem_type: t)
      (update_mem_type: t)
      (mem_map: MemAnno.slot_t mem_content) : t option =
    let inner_helper
        (acc: t option)
        (update_slot: entry_t mem_slot)
        (slot_map: MemAnno.slot_t mem_slot) : t option =
      match acc with
      | None -> None
      | Some mem_type ->
        let off1, update_slot_forget_type, update_range, update_type = update_slot in
        let off2, slot_forget_type, slot_range, slot_info = slot_map in
        (* Printf.printf "set_mem_type:\ncurr mem:%s\nupdate_slot:%s\nslot_map:%s\n"
          (mem_type |> sexp_of_t |> Sexplib.Sexp.to_string_hum)
          (update_slot |> sexp_of_entry_mem_slot |> Sexplib.Sexp.to_string_hum)
          (slot_map |> sexp_of_mem_anno_mem_slot |> Sexplib.Sexp.to_string_hum); *)
        (* Check whether the two slots agree on whether to forget valid range/taint. *)
        if slot_forget_type <> update_slot_forget_type then begin
          Printf.printf "Warning: (%s, %s) set_mem_type_with_other forget type field mismatch (%b, %b)\n"
            (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t off1))
            (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t off2))
            update_slot_forget_type slot_forget_type;
          None
        end else
        match update_range with
        | [] ->
          if update_slot_forget_type then (* There must also be slot_forget_type = true according to the previous check *)
            (* In this case we need to update the slot valid range to 
               (slot_range - udpate_off) to clear valid range that might be changed by update_off. *)
            if List.is_empty slot_range then acc
            else begin
              (* This case usually should not happen, so I skipped its implementation. *)
              Printf.printf "Warning: Not implemented\n";
              None
            end
          else 
            (* If update_slot_forget_type = false and update valid range = [], then the child function does not write to the slot at all. *)
            acc
        | [ update_off ] ->
          (* We only implement for the case where slot_forget_type = update_slot_forget_type = false *)
          if slot_forget_type then begin
            Printf.printf "Warning: Not implemented\n"; (* It is a bit complex to handle update slot is a subsetneq of parent/orig slot. *)
            None
          end else
          set_mem_type smt_ctx is_non_change_exp mem_type update_off slot_info update_type
        | _ -> 
          Printf.printf "Warning: set_mem_type_with_other range format is not single offset slot\n";
          None
    in
    let outer_helper
        (acc: t option)
        (update_part_mem: entry_t mem_part)
        (part_mem_map: MemAnno.slot_t mem_part) : t option =
      match acc with
      | None -> None
      | Some _ ->
        let update_ptr_info, update_slot_list = update_part_mem in
        let _, slot_map_list = part_mem_map in
        (* Filter write entries *) 
        let update_slot_list = PtrInfo.filter_write_permission update_ptr_info update_slot_list in
        let slot_map_list = PtrInfo.filter_write_permission update_ptr_info slot_map_list in
        List.fold_left2 inner_helper acc update_slot_list slot_map_list
        (* if PtrInfo.can_write update_ptr_info then begin
          List.fold_left2 inner_helper acc update_slot_list (snd part_mem_map)
        end else acc *)
    in
    (*
    Printf.printf "parent_mem_type: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t mem_type));
    Printf.printf "update_mem_type: %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t update_mem_type));
    *)
    List.fold_left2 outer_helper (Some mem_type) update_mem_type mem_map

end


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
      else Other
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
        | Other -> mem_offset_error "insert_new_offset_list_merge fail"
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

end

module MemRange = struct
  exception MemRangeError of string

  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type t = MemOffset.t list
  [@@deriving sexp]

  let merge (smt_ctx: SmtEmitter.t) (r1: t) (r2: t) : t =
    MemOffset.insert_new_offset_list_merge smt_ctx r1 r2

  let substitute
      (substitute_func: DepType.exp_t -> DepType.exp_t)
      (range: t) : t =
    List.map (MemOffset.substitute substitute_func) range

  let is_empty (r: t) : bool = r = []

  let check_subset
      (smt_ctx: SmtEmitter.t)
      (r1: t) (r2: t) : bool =
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
    | [ o1 ], [ o2 ] ->
      MemOffset.offset_cmp smt_ctx CmpEq o1 o2 = Eq
    | _ -> false

end

module MemType = struct
  exception MemTypeError of string

  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type 'a mem_slot = MemOffset.t * MemRange.t * 'a
  [@@deriving sexp]

  type 'a mem_part = PtrInfo.t * (('a mem_slot) list)
  [@@deriving sexp]

  type 'a mem_content = ('a mem_part) list
  [@@deriving sexp]

  type entry_t = BasicType.t
  [@@deriving sexp]

  type t = entry_t mem_content
  [@@deriving sexp]

  let map_full
      (func: MemOffset.t * MemRange.t * 'a -> MemOffset.t * MemRange.t * 'b)
      (mem: 'a mem_content) : 'b mem_content =
    let helper_outer
        (entry: 'a mem_part) :
        'b mem_part =
      let ptr, part_mem = entry in
      ptr, List.map func part_mem
    in
    List.map helper_outer mem

  let fold_left2_full
      (func: 'acc -> MemOffset.t * MemRange.t * 'a -> MemOffset.t * MemRange.t * 'b -> 'acc)
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
      PtrInfo.t * (MemAnno.slot_t * 'a) mem_slot =
    let ptr_info, (off, range, entry) = get_single_slot mem slot_info in
    ptr_info, (off, range, (slot_info, entry))

  let get_mem_boundary_list 
      (mem_type: t) : (PtrInfo.t * MemOffset.t) list =
    let helper 
        (part_mem: 'a mem_part) :
        (PtrInfo.t * MemOffset.t) option =
      let ptr_info, offset_list = part_mem in
      match offset_list with
      | [] -> None
      | ((l, r), _, _) :: [] ->
        Some (ptr_info, (l, r))
      | ((l, _), _, _) :: tl ->
        let (_, r), _, _ = List.nth tl ((List.length tl) - 1) in
        Some (ptr_info, (l, r))
    in
    List.filter_map helper mem_type

  let check_non_overlap
      (smt_ctx: SmtEmitter.t)
      (mem: 'a mem_content) : bool =
    let ctx, _ = smt_ctx in
    let rec get_mem_non_overlap_constraint_helper
        (acc: DepType.exp_t list)
        (boundary_list: (PtrInfo.t * MemOffset.t) list) :
        DepType.exp_t list =
      match boundary_list with
      | [] -> acc
      | ((_, (hd_overlap_set, _, _)), (hd_l, hd_r)) :: tl ->
        let acc = get_mem_non_overlap_constraint_helper acc tl in
        List.fold_left (
            fun (acc: DepType.exp_t list) ((ptr, _), (l, r)) ->
              if IntSet.mem ptr hd_overlap_set then acc
              else 
                (Boolean.mk_or ctx [
                  (BitVector.mk_sle ctx hd_r l);
                  (BitVector.mk_sle ctx r hd_l);
                ]) :: acc
          ) acc tl
    in
    let boundary_list = get_mem_boundary_list mem in
    let constraint_list = get_mem_non_overlap_constraint_helper [] boundary_list in

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
          fun (off, range, _) ->
            not (MemRange.check_subset smt_ctx range [off])
        ) part_mem <> None
    ) mem = None

  let check_slot_subtype
      (smt_ctx: SmtEmitter.t)
      (off_must_eq: bool)
      (is_spill: bool)
      (sub_slot: entry_t mem_slot)
      (sup_slot: entry_t mem_slot) : bool =
    let offset_cmp = MemOffset.offset_cmp smt_ctx in
    let sub_off, sub_range, (sub_dep, sub_taint) = sub_slot in
    let sup_off, sup_range, (sup_dep, sup_taint) = sup_slot in
    let check_taint () : bool = TaintType.check_subtype smt_ctx true sub_taint sup_taint in
    let offset_cmp_option, offset_cmp_goal =
      if off_must_eq then MemOffset.CmpEq, MemOffset.Eq
      else MemOffset.CmpSubset, MemOffset.Subset
    in
    if offset_cmp offset_cmp_option sub_off sup_off = offset_cmp_goal then
      if MemRange.is_empty sup_range then
        (* In this case we do not check dep *)
        if is_spill then true (* taint for spill can be anything if s-val is empty *)
        else check_taint ()
      else if MemRange.check_single_slot_eq smt_ctx sub_range sup_range then
        DepType.check_subtype smt_ctx sub_dep sup_dep &&
        check_taint ()
      else
        let check_dep =
          match sup_dep with
          | Top _ -> true
          | _ -> false
        in
        check_dep &&
        check_taint ()
    else false

  let imply_helper (sub: bool) (sup: bool) : bool =
      (* sub => sup *)
      (not sub) || sup

  let check_subtype_no_map
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: MemAnno.slot_t -> bool)
      (sub_m_type: t) (sup_m_type: t) : bool =
    (* Items to check:
       1. Ptr info
       2. MemRange (s_val)
       3. BasicType *)
    List.fold_left2 (
      fun (acc: bool) (sub: entry_t mem_part) (sup: entry_t mem_part) ->
        if not acc then acc else
        let (sub_ptr, (sub_might_overlap, sub_read, sub_write)), sub_part_mem = sub in
        let (sup_ptr, (sup_might_overlap, sup_read, sup_write)), sup_part_mem = sup in
        sub_ptr = sup_ptr &&
        sub_might_overlap = sup_might_overlap &&
        imply_helper sub_read sup_read &&
        imply_helper sub_write sup_write &&
        List.fold_left2 (
          fun (acc: bool * int) (sub_slot: entry_t mem_slot) (sup_slot: entry_t mem_slot) ->
            let acc_check, slot_idx = acc in
            if not acc_check then acc_check, slot_idx + 1
            else 
              let is_spill = is_spill_func (sub_ptr, slot_idx, false, 1) in
              check_slot_subtype smt_ctx true is_spill sub_slot sup_slot, 
              slot_idx + 1
        ) (true, 0) sub_part_mem sup_part_mem |> fst
    ) true sub_m_type sup_m_type

  let apply_mem_map
      (mem_type: 'a mem_content)
      (mem_map: MemAnno.slot_t mem_content) :
      (MemAnno.slot_t * 'a) mem_content =
    List.map (
      fun (_, part_mem_map) ->
        let ptr_info_opt, new_part_mem =
          List.fold_left_map (
            fun (acc: PtrInfo.t option) (_, _, slot_info) ->
              let curr_ptr_info, new_slot = get_single_slot_with_info mem_type slot_info in
              match acc with
              | None -> Some curr_ptr_info, new_slot
              | Some (acc_ptr, _) ->
                if acc_ptr = fst curr_ptr_info then acc, new_slot
                else mem_type_error "apply_mem_map corresponding ptr of slots in one part_mem do not match"
          ) None part_mem_map
        in
        Option.get ptr_info_opt, new_part_mem
    ) mem_map

  let check_subtype_map
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: MemAnno.slot_t -> bool)
      (sub_m_type: t) (sup_m_type: t)
      (mem_map: MemAnno.slot_t mem_content) : bool =
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
    List.fold_left2 (
      fun (acc: bool) (sub: (MemAnno.slot_t * entry_t) mem_part) (sup: entry_t mem_part) ->
        if not acc then acc else
        let (_, (_, sub_read, sub_write)), sub_part_mem = sub in
        let (_, (_, sup_read, sup_write)), sup_part_mem = sup in
        imply_helper sub_read sup_read &&
        imply_helper sub_write sup_write &&
        List.fold_left2 (
          fun (acc_check: bool) 
              (sub_info_slot: (MemAnno.slot_t * entry_t) mem_slot)
              (sup_slot: entry_t mem_slot) ->
            if not acc_check then acc_check
            else
              let (sub_off, sub_range, (sub_slot_info, sub_entry)) = sub_info_slot in
              if is_spill_func sub_slot_info then
                mem_type_error "check_subtype_map should not map spill slots (at func call)"
              else
                check_slot_subtype smt_ctx false false (sub_off, sub_range, sub_entry) sup_slot
        ) true sub_part_mem sup_part_mem
    ) true sub_map_m_type sup_m_type

  let check_subtype
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: MemAnno.slot_t -> bool)
      (sub_m_type: t) (sup_m_type: t)
      (mem_map_opt: MemAnno.slot_t mem_content option) : bool =
    match mem_map_opt with
    | None -> check_subtype_no_map smt_ctx is_spill_func sub_m_type sup_m_type
    | Some mem_map -> check_subtype_map smt_ctx is_spill_func sub_m_type sup_m_type mem_map

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
    let ctx, _ = smt_ctx in
    let _, slot_range, (slot_dep, slot_taint) = slot in
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
        Printf.printf "Warning: get_one_slot_type off check failed\n";
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
        let _, slot_range, slot_type = slot in
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
    | (_, [ hd_l, hd_r ], hd_type) :: tl ->
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
      (mem_type: t)
      (addr_off: MemOffset.t) 
      (slot_info: MemAnno.slot_t) : entry_t option =
    (* 1. Check read permission; 
       2. get entry with slot; 
       3. check add_off *)
    let ptr_info, part_mem = find_part_mem_helper mem_type slot_info in
    if not (PtrInfo.can_read ptr_info) then begin
      Printf.printf "Warning: get_mem_type read slot %s permission is not satisfied.\n"
        (Sexplib.Sexp.to_string_hum (MemAnno.sexp_of_slot_t slot_info));
      None
    end else
      (* Check addr_off and get entry *)
      get_part_mem smt_ctx part_mem slot_info addr_off

  let set_one_slot_type
      (smt_ctx: SmtEmitter.t)
      (is_spill: bool)
      (slot: entry_t mem_slot)
      (addr_off: MemOffset.t)
      (new_type: BasicType.t) : entry_t mem_slot option =
    (* 1. Update entry (valid region and type)
       2. Check slot_off, taint constraint *)
    let slot_off, slot_range, (_, slot_taint) = slot in
    if is_spill then begin
      (* Check slot_off *)
      if MemOffset.offset_cmp smt_ctx CmpSubset addr_off slot_off = Subset then begin
        (* For spill, we overwrite the valid region with write range. *)
        Some (slot_off, [ addr_off ], new_type)
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
          Some (slot_off, [ slot_off ], new_type)
        end else if cmp_off = Subset then begin
          Some (slot_off, MemRange.merge smt_ctx slot_range [addr_off], (Top DepType.top_unknown_size, new_taint))
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
          let (slot_l, slot_r), _, (_, slot_taint) = slot in
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
                ((slot_l, slot_r), [slot_l, slot_r], 
                (DepType.get_start_len (fst smt_ctx) start_byte len_byte new_dep, new_taint))
              | None ->
                Some (idx + 1, slot_r),
                ((slot_l, slot_r), [slot_l, slot_r], (Top DepType.top_unknown_size, new_taint))
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
      (is_spill_func: MemAnno.slot_t -> bool)
      (part_mem: entry_t mem_slot list)
      (addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: BasicType.t) : (entry_t mem_slot list) option =
    (* 1. Update entry
       2. Check slot_info, taint constraint *)
    let _, slot_idx, _, num_slots = slot_info in
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
                match set_one_slot_type smt_ctx (is_spill_func slot_info) slot addr_off new_type with
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
      (is_spill_func: MemAnno.slot_t -> bool)
      (mem_type: t)
      (addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: BasicType.t) : t option =
    (* 1. Check write permission; 
       2. update entry and all read/write permission; 
       3. check slot_info, taint constraint *)
    let (ptr, ptr_info), part_mem = find_part_mem_helper mem_type slot_info in
    if not (PtrInfo.can_write_info ptr_info) then begin
      Printf.printf "Warning: get_mem_type read slot %s permission is not satisfied.\n"
        (Sexplib.Sexp.to_string_hum (MemAnno.sexp_of_slot_t slot_info));
      None
    end else
      let new_part_mem_opt = set_part_mem smt_ctx is_spill_func part_mem addr_off slot_info new_type in
      match new_part_mem_opt with
      | None -> None
      | Some new_part_mem ->
        Some (List.map (
          fun ((p, p_info), p_part_mem) ->
            if p = ptr then (p, p_info), new_part_mem
            else PtrInfo.invalidate_on_write ptr (p, p_info), p_part_mem
        ) mem_type)

  let set_mem_type_with_other
      (smt_ctx: SmtEmitter.t)
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
        let _, update_range, update_type = update_slot in
        let _, _, slot_info = slot_map in
        match update_range with
        | [ update_off ] ->
          set_mem_type smt_ctx (fun _ -> false) mem_type update_off slot_info update_type
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
        if PtrInfo.can_write update_ptr_info then begin
          List.fold_left2 inner_helper acc update_slot_list (snd part_mem_map)
        end else acc
    in
    List.fold_left2 outer_helper (Some mem_type) update_mem_type mem_map

end


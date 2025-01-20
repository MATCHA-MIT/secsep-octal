open Type.Ptr_info
open Type.Smt_emitter
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
    | LOverlap | GOverlap
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
    let goverlap_req = [ BitVector.mk_sle ctx l2 l1; BitVector.mk_sle ctx l1 r2 ] in

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
      else CmpSubset, Subset
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

end


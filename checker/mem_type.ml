open Type.Ptr_info
open Type.Smt_emitter
open Basic_type
(* open Mem_anno *)
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

end

module MemRange = struct
  exception MemRangeError of string

  let mem_range_error msg = raise (MemRangeError ("[Mem Range Error] " ^ msg))

  type t = MemOffset.t list
  [@@deriving sexp]

  let merge (smt_ctx: SmtEmitter.t) (r1: t) (r2: t) : t =
    MemOffset.insert_new_offset_list_merge smt_ctx r1 r2

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

  type t = BasicType.t mem_content
  [@@deriving sexp]

end


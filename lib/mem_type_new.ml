open Pretty_print
open Smt_emitter
open Isa_basic
open Ptr_info
open Single_exp
open Entry_type
open Mem_offset_new
open Full_mem_anno
open Cond_type_new
open Single_context
open Constraint
(* open Arch_type *)
open Set_sexp
open Sexplib.Std

module MemKeySet = struct
include IntSet
  
  exception MemKeySetError of string
  let mem_key_set_error msg = raise (MemKeySetError ("[Mem Key Set Error] " ^ msg))

  let pp_list (lvl: int) (ptr_list: IsaBasic.imm_var_id list) =
    PP.print_lvl lvl "Ptr list: ";
    List.iter (
      fun x -> Printf.printf "%d " x
    ) ptr_list;
    Printf.printf "\n"

  let pp_set (lvl: int) (ptr_set: t) =
    pp_list lvl (elements ptr_set)

  let rec filter_single_var (addr: SingleExp.t) : t =
    match addr with
    | SingleVar x -> singleton x
    | SingleBExp (SingleExp.SingleAdd, l, r) ->
      let left_ptr = filter_single_var l in
      let right_ptr = filter_single_var r in
      union left_ptr right_ptr
    | _ -> empty

end

module MemTypeBasic = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type 'a mem_slot = MemOffset.t * MemRange.t * 'a
  [@@deriving sexp]

  type 'a mem_part = PtrInfo.t * (('a mem_slot) list)
  [@@deriving sexp]

  type 'a mem_content = ('a mem_part) list
  [@@deriving sexp]

  let pp_mem_type_generic (lvl: int) (mem: 'a mem_content) (str_of_entry: ('a -> string) option) =
    PP.print_lvl lvl "<MemType>\n";
    List.iter (
      fun ((ptr, _), off_list) ->
        PP.print_lvl (lvl + 1) "<Ptr %d>\n" ptr;
        List.iter (
          fun (off, r, entry) ->
            let entry_str = match str_of_entry with
            | None ->  ""
            | Some f -> Printf.sprintf "\t%s" (f entry)
            in
            PP.print_lvl (lvl + 2) "%s\t%s%s\n" (MemOffset.to_string off) (MemRange.to_string r) entry_str
        ) off_list
    ) mem

  let map (func: 'a -> 'b) (mem: 'a mem_content) : 'b mem_content =
    let helper_inner 
        (entry: MemOffset.t * MemRange.t * 'a) : 
        MemOffset.t * MemRange.t * 'b =
      let off, range, e = entry in off, range, func e
    in
    let helper_outer 
        (entry: 'a mem_part) :
        'b mem_part =
      let ptr, part_mem = entry in
      ptr, List.map helper_inner part_mem
    in
    List.map helper_outer mem

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

  let map2 
      (keep_first_ptr_info: bool)
      (func: 'a -> 'b -> 'c) 
      (mem1: 'a mem_content) (mem2: 'b mem_content) : 
      'c mem_content =
    let helper_inner 
        (entry1: MemOffset.t * MemRange.t * 'a)
        (entry2: MemOffset.t * MemRange.t * 'b) : 
        MemOffset.t * MemRange.t * 'c =
      let off1, range, e1 = entry1 in
      let off2, _, e2 = entry2 in
      if MemOffset.cmp off1 off2 = 0 then
        off1, range, func e1 e2
      else
        mem_type_error "[map2] off does not match"
    in
    let helper_outer 
        (entry1: 'a mem_part)
        (entry2: 'b mem_part) :
        'c mem_part =
      let (ptr1, ptr1_info), part_mem1 = entry1 in
      let (ptr2, ptr2_info), part_mem2 = entry2 in
      if ptr1 = ptr2 then
        let ptr_info = if keep_first_ptr_info then ptr1_info else ptr2_info in
        (ptr1, ptr_info), List.map2 helper_inner part_mem1 part_mem2
      else
        mem_type_error "[map2] ptr does not match"
    in
    List.map2 helper_outer mem1 mem2

  let map2_full
      (keep_first_ptr_info: bool)
      (func: MemOffset.t * MemRange.t * 'a -> MemOffset.t * MemRange.t * 'b ->
              MemOffset.t * MemRange.t * 'c)
      (mem1: 'a mem_content) (mem2: 'b mem_content) : 'c mem_content =
    let helper_outer
        (entry1: 'a mem_part)
        (entry2: 'b mem_part) :
        'c mem_part =
      let (ptr1, ptr1_info), part_mem1 = entry1 in
      let (ptr2, ptr2_info), part_mem2 = entry2 in
      if ptr1 = ptr2 then
        let ptr_info = if keep_first_ptr_info then ptr1_info else ptr2_info in
        (ptr1, ptr_info), List.map2 func part_mem1 part_mem2
      else
        mem_type_error "[map2_full] ptr does not match"
    in
    List.map2 helper_outer mem1 mem2

  let fold_left
      (func: 'acc -> 'a -> 'acc)
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc =
    let helper_inner
        (acc: 'acc) (entry: MemOffset.t * MemRange.t * 'a) : 'acc =
      let _, _, e = entry in
      func acc e
    in
    let helper_outer
        (acc: 'acc) (entry: 'a mem_part) : 'acc =
      let _, l = entry in
      List.fold_left helper_inner acc l
    in
    List.fold_left helper_outer acc mem

  let fold_left_full
      (func: 'acc -> MemOffset.t * MemRange.t * 'a -> 'acc)
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc =
    let helper_outer
        (acc: 'acc) (entry: 'a mem_part) : 'acc =
      let _, l = entry in
      List.fold_left func acc l
    in
    List.fold_left helper_outer acc mem

  let fold_left_map
      (func: 'acc -> 'a -> ('acc * 'b))
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc * ('b mem_content) =
    let helper_inner
        (acc: 'acc) (entry: MemOffset.t * MemRange.t * 'a): 
        'acc * (MemOffset.t * MemRange.t * 'b) =
      let off, range, e = entry in
      let acc, e = func acc e in
      acc, (off, range, e)
    in
    let helper_outer
        (acc: 'acc) (entry: 'a mem_part) : 
        'acc * ('b mem_part) =
      let id, l = entry in
      let acc, l = List.fold_left_map helper_inner acc l in
      acc, (id, l)
    in
    List.fold_left_map helper_outer acc mem

  let fold_left_map_full
      (func: 'acc -> MemOffset.t * MemRange.t * 'a -> ('acc * (MemOffset.t * MemRange.t * 'b)))
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc * ('b mem_content) =
    let helper_outer
        (acc: 'acc) (entry: 'a mem_part) :
        'acc * ('b mem_part) =
      let id, l = entry in
      let acc, l = List.fold_left_map func acc l in
      acc, (id, l)
    in
    List.fold_left_map helper_outer acc mem

  let fold_left2
      (func: 'acc -> 'a -> 'b -> 'acc)
      (acc: 'acc)
      (mem1: 'a mem_content)
      (mem2: 'b mem_content) : 'acc =
    let helper_inner
        (acc: 'acc)
        (entry1: MemOffset.t * MemRange.t * 'a)
        (entry2: MemOffset.t * MemRange.t * 'b) : 'acc =
      let off1, _, e1 = entry1 in
      let off2, _, e2 = entry2 in
      if MemOffset.cmp off1 off2 = 0 then func acc e1 e2
      else mem_type_error "[fold_left2] mem offset does not match"
    in
    let helper_outer
        (acc: 'acc) 
        (entry1: 'a mem_part)
        (entry2: 'b mem_part) : 'acc =
      let (v1, _), l1 = entry1 in
      let (v2, _), l2 = entry2 in
      if v1 = v2 then List.fold_left2 helper_inner acc l1 l2
      else mem_type_error "[fold_left2] ptr does not match"
    in
    List.fold_left2 helper_outer acc mem1 mem2

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

  let add_base_to_offset (mem_layout: 'a mem_content) : 'a mem_content =
    List.map (
      fun ((base, base_info), off_list) ->
        ((base, base_info),
        List.map (
          fun (off, range, entry) ->
            MemOffset.add_base (SingleVar base) off,
            (* (SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, l)),
            SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, r))), *)
            MemRange.add_base (SingleVar base) range,
            entry
        ) off_list
        )
    ) mem_layout

  let get_var_type_map (mem_layout: 'a mem_content) : SingleExp.var_type_map_t =
    List.fold_left (
      fun (acc: SingleExp.var_type_map_t) ((ptr, _), off_list) ->
        let acc = SingleExp.set_var_type acc ptr SingleExp.VarPtr in
        List.fold_left (
          fun (acc: SingleExp.var_type_map_t) (entry: MemOffset.t * MemRange.t * 'a) ->
            let (l, r), _, _ = entry in
            let len_var_set = SingleExp.SingleVarSet.diff (SingleExp.get_vars r) (SingleExp.get_vars l) in
            List.fold_left (
              fun acc v -> 
                SingleExp.set_var_type acc v SingleExp.VarLen
            ) acc (SingleExp.SingleVarSet.to_list len_var_set)
        ) acc off_list
    ) [] mem_layout
  
end

module MemType (Entry: EntryType) = struct
  include MemTypeBasic

  type entry_t = Entry.t
  [@@deriving sexp]

  type t = entry_t mem_content
  [@@deriving sexp]

  module MemAnno = FullMemAnno

  let pp_mem_type (lvl: int) (mem: t) = pp_mem_type_generic lvl mem (Some Entry.to_string)

  let pp_ocaml_mem_type (lvl: int) (buf: Buffer.t) (mem: t) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun ((ptr, _), off_list) ->
        PP.bprint_lvl (lvl + 1) buf "(%d, [\n" ptr;
        List.iter (
          fun (off, range, entry) ->
            PP.bprint_lvl (lvl + 2) buf "(%s, %s, %s);\n"
              (MemOffset.to_ocaml_string off)
              (MemRange.to_ocaml_string range)
              (Entry.to_ocaml_string entry)
        ) off_list;
        PP.bprint_lvl (lvl + 1) buf "]);\n"
    ) mem;
    PP.bprint_lvl lvl buf "]\n"

  let init_mem_type_from_layout
      (start_var: entry_t) (mem_layout: 'a mem_content) : entry_t * t =
    let helper (acc: entry_t) (entry: 'a) : entry_t * 'a =
      let _ = entry in
      Entry.next_var acc, acc
    in
    fold_left_map helper start_var mem_layout
    (* let _ = mem_layout in start_var, [] *)

  let add_ret_addr_stack_slot
      (mem: t) : t =
    (* let found, mem =
      List.fold_left_map (
        fun (acc: bool) (ptr, part_mem) ->
          if acc then acc, (ptr, part_mem)
          else if ptr = IsaBasic.rsp_idx then
            true, (ptr, ((SingleExp.SingleConst 0L, SingleExp.SingleConst 0L), MemRange.RangeConst [], Entry.get_top_untaint_type ()) :: part_mem)
          else
            acc, (ptr, part_mem)
      ) false mem
    in *)
    let found =
      List.find_opt (
        fun ((ptr, _), _) -> ptr = IsaBasic.rsp_idx
      ) mem <> None
    in
    if found then mem
    else ((PtrInfo.get_default_info IsaBasic.rsp_idx), [ ((SingleExp.SingleConst 0L, SingleExp.SingleConst 8L), MemRange.RangeConst [], Entry.get_top_untaint_type ()) ]) :: mem

  (* let get_mem_entry_one_ptr_helper
      (smt_ctx: SmtEmitter.t)
      (mem: (MemOffset.t * MemRange.t * 'a) list)
      (addr_offset: MemOffset.t) :
      ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option =
    let helper 
        (acc: ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option)
        (entry: MemOffset.t * MemRange.t * 'a) :
        ((MemOffset.t * MemRange.t * 'a) * bool * MemOffset.ConstraintSet.t) option =
      let entry_offset, _, _ = entry in
      match acc with
      | None ->
        if MemOffset.equal addr_offset entry_offset then begin
          Some (entry, true, MemOffset.ConstraintSet.empty)
        end else begin
          match MemOffset.
        end *)

  (* let get_mem_entry 
      (smt_ctx: SmtEmitter.t) 
      (mem: t) (ptr_list: MemKeySet.t) 
      (addr_offset: MemOffset.t) :
      ((IsaBasic.imm_var_id * MemOffset.t * MemRange.t * entry_t) * MemOffset.ConstraintSet.t) option =
    let addr_l, addr_r = addr_offset in
    let base_opt = MemKeySet.find_base addr_l ptr_list in
    (* The accessed address range [l, r] must satisfy l < r *)
    (* Maybe we do not need this! *)
    let addr_constraint = MemOffset.check_offset smt_ctx addr_offset in
    match base_opt with
    |  *)

  let get_ptr_set (mem: t) : SingleExp.SingleVarSet.t =
    let ptr_list = List.map (fun ((x, _), _) -> x) mem in
    SingleExp.SingleVarSet.of_list ptr_list

  let get_part_mem (mem: 'a mem_content) (ptr: IsaBasic.imm_var_id) : 'a mem_part =
    List.find (fun ((x, _), _) -> x = ptr) mem
    (* let _, part_mem = List.find (fun ((x, _), _) -> x = ptr) mem in part_mem *)

  (* get the entry from memory type using strict comparison *)
  let get_mem_type_strict
      (mem: 'a mem_content)
      (location: IsaBasic.imm_var_id * MemOffset.t)
      : 'a option =
    let base, off = location in
    List.find_map (
      fun mem_part ->
        let (base', _), slots = mem_part in
        if base' <> base then None else
        List.find_map (
          fun (off', _, entry) ->
            if MemOffset.cmp off' off = 0 then Some entry
            else None
        ) slots
    ) mem

  let enable_mult_slot_lookup
      (smt_ctx: SmtEmitter.t)
      (base_ptr: IsaBasic.imm_var_id)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) : bool =
    let simp_l, simp_r = simp_addr_off in
    let simp_l_off = SingleExp.eval (SingleBExp (SingleSub, simp_l, SingleVar base_ptr)) in
    let simp_len = SingleExp.eval (SingleBExp (SingleSub, simp_r, simp_l)) in
    match simp_l_off, simp_len with
    | SingleConst _, SingleConst _ ->
      begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off simp_addr_off CmpEq with
      | Eq -> true
      | _ -> false
      end
    | _ -> false

  let get_mult_slot_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (s_off: MemOffset.t)
      (part_mem: entry_t mem_slot list) :
      (MemOffset.t * MemRange.t * entry_t) option  * int * (Constraint.t list) =
    let s_left, s_right = s_off in
    let rec get_slot_helper (slot_list: entry_t mem_slot list) (left: SingleExp.t) : entry_t mem_slot list =
      match slot_list with
      | [] -> []
      | ((hd_l, hd_r), hd_range, hd_entry) :: tl ->
        (* let hd_entry = if is_full then hd_entry else Entry.mem_partial_read_val hd_entry in *)
        if SingleExp.cmp left hd_l = 0 then
          if SingleExp.cmp s_right hd_r = 0 then
            [ ((hd_l, hd_r), hd_range, hd_entry) ]
          else
            ((hd_l, hd_r), hd_range, hd_entry) :: (get_slot_helper tl hd_r)
        else 
          get_slot_helper tl left
    in
    let check_slots (slot_list: entry_t mem_slot list) : bool =
      match slot_list with
      | [] -> false
      | _ ->
        let (left, _), _, _ = List.hd slot_list in
        let (_, right), _, _ = List.nth slot_list (List.length slot_list - 1) in
        SingleExp.cmp s_left left = 0 && SingleExp.cmp s_right right = 0
    in
    let merge_slot_helper (slot_list: entry_t mem_slot list) : (entry_t mem_slot) * Constraint.t list =
      if List.is_empty slot_list then
        mem_type_error "get_slot_mem_type merge_slot_helper has empty list of slots"
      else
        let _, _, fst_entry = List.hd slot_list in
        let merged_entry = Entry.set_taint_with_other (Entry.get_top_untaint_type ()) fst_entry in
        let merged_range, slot_taint_same_cons =
          List.fold_left (
            fun (acc: MemRange.t * (Constraint.t list)) (slot: entry_t mem_slot) ->
              let acc_range, acc_cons = acc in
              let _, range, entry = slot in
              (* <TODO> The merge may fail here, and may fail the range infer (checker also has the similar issue). *)
              MemRange.merge smt_ctx acc_range range |> fst,
              (Entry.get_eq_taint_constraint merged_entry entry) @ acc_cons
          ) (RangeConst [], []) slot_list
        in
        (s_off, merged_range, merged_entry), slot_taint_same_cons
    in
    let slots = get_slot_helper part_mem s_left in
    if not (check_slots slots) then None, 0, [] (* No slots found, so use 0 for placeholder *)
    else
    let slot, cons = merge_slot_helper slots in
    Some slot, List.length slots, cons

  let get_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: MemOffset.t -> bool)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) :
      (bool * (MemOffset.t * MemRange.t * entry_t)) option =
    List.find_map (
      fun (off, range, entry) ->
        match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off CmpSubset with
        | Subset ->
          (* Note: here we compare read range with s-alloc since s-valid (init range) is not ready in single infer.
             The typing rule actually consider reading full s-valid as full read. *)
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off CmpEqSubset with
          | Eq -> Some (true, (off, range, entry)) (* full read *)
          | Subset -> 
            if is_spill_func off then Some (false, (off, range, entry)) (* although not equal to s-alloc, but spill should be equal to s-valid *)
            else
            (* Printf.printf "get_part_mem_type access off %s and convert off %s type %s to top\n" (MemOffset.to_string addr_offset) (MemOffset.to_string off) (Entry.to_string entry); *)
            Some (false, (off, range, Entry.mem_partial_read_val entry)) (* not full read *)
          | _ -> 
            SmtEmitter.pp_smt_ctx 1 smt_ctx;
            Printf.printf "orig_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t orig_addr_off));
            Printf.printf "simp_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t simp_addr_off));
            Printf.printf "quick cmp success while full cmp failed\n";
            begin match SmtEmitter.check_context smt_ctx with
            | SatYes -> Printf.printf "ctx success\n"
            | _ -> Printf.printf "ctx fail\n"
            end;
            (* let l1, r1 = orig_addr_off in
            let l2, r2 = off in
            Printf.printf "No overflow %s\n" (Z3.Expr.to_string (SmtEmitter.expr_of_single_exp smt_ctx (SingleBExp (SingleMul, SingleVar 2, SingleConst 128L)) true));
            SmtEmitter.pp_smt_ctx 0 smt_ctx;
            begin match SingleCondType.check false smt_ctx [ SingleCondType.Le, l2, l1 ] with
            | SatYes -> Printf.printf "l2<=l1 SatYes\n"
            | SatNo -> Printf.printf "l2<=l1 SatNo\n"
            | _ -> Printf.printf "l2<=l1 SatUnknown\n";
              begin match SmtEmitter.get_model smt_ctx with
              | Some exp -> Printf.printf "Get model %s\n" (Z3.Model.to_string exp)
              | None -> Printf.printf "No model\n"
              end
            end;
            begin match SingleCondType.check false smt_ctx [ SingleCondType.Le, r1, r2 ] with
            | SatYes -> Printf.printf "r1<=r2 SatYes\n"
            | SatNo -> Printf.printf "r1<=r2 SatNo\n"
            | _ -> Printf.printf "r1<=r2\n SatUnknown\n"
            end; *)
            None
          end
        | _ -> None
    ) part_mem

  let get_mem_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (try_mult_slot_lookup: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) :
      (bool * IsaBasic.imm_var_id * (MemOffset.t * MemRange.t * entry_t) * int * (Constraint.t list)) option =
    (* let _ = smt_ctx, mem, addr_offset in
    None *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    (* Printf.printf "orig_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t orig_addr_off));
    Printf.printf "simp_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t simp_addr_off)); *)
    let ptr_set = get_ptr_set mem in
    let simp_l, simp_r = simp_addr_off in
    if SingleExp.cmp simp_l SingleTop = 0 || SingleExp.cmp simp_r SingleTop = 0 then None else
    let result = match SingleExp.find_base_adv sub_sol_list_func simp_l ptr_set, SingleExp.find_base_adv sub_sol_list_func simp_r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
      else 
        let ptr_info, part_mem = get_part_mem mem b_l in
        if not (PtrInfo.can_read ptr_info) then mem_type_error (Printf.sprintf "Cannot read from ptr %d" b_l)
        else
        begin match get_part_mem_type smt_ctx (is_spill_func b_l) part_mem orig_addr_off simp_addr_off with
        | Some (is_full, find_entry) -> Some (is_full, b_l, find_entry, 1, [])
        | None -> (* Decide whether we want to try get_mult_slot_part_mem_type *)
          if try_mult_slot_lookup && enable_mult_slot_lookup smt_ctx b_l orig_addr_off simp_addr_off then begin
            match get_mult_slot_part_mem_type smt_ctx simp_addr_off part_mem with
            | Some slot, find_num_slot, cons_list ->
              Some (true, b_l, slot, find_num_slot, cons_list)
            | _ -> None
          end else None
        end
    | _ ->
      Printf.printf "Cannot find base\n";
      Printf.printf "orig_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t orig_addr_off));
      Printf.printf "simp_addr_off\n%s\n" (Sexplib.Sexp.to_string_hum (MemOffset.sexp_of_t simp_addr_off));
      let related_vars = SingleExp.SingleVarSet.union (SingleExp.get_vars simp_l) (SingleExp.get_vars simp_r) in
      (* heuristic priority: related vars > others; within each group, the offset list sizes are ascending *)
      let reformed_mem = List.map (fun ((x, x_info), x_mem) ->
        ((x, x_info), x_mem, SingleExp.SingleVarSet.exists (fun z -> z = x) related_vars, List.length x_mem)
      ) mem
      in
      let sorted_mem = List.sort (fun ((x, _), _, x_related, x_len) ((y, _), _, y_related, y_len) ->
        if x = y then 0 else
        let len_cmp = Int.compare x_len y_len in
        if x_related && y_related then len_cmp else
        if x_related then -1 else
        if y_related then 1 else
        len_cmp
      ) reformed_mem |> List.map (fun (x, y, _, _) -> (x, y))
      in
      List.find_map (
        fun ((ptr, _), part_mem) -> 
          match get_part_mem_type smt_ctx (is_spill_func ptr) part_mem orig_addr_off simp_addr_off with
          | Some (is_full, find_entry) -> Some (is_full, ptr, find_entry, 1, [])
          | None -> None
      ) sorted_mem
    in
    (* Printf.printf "\ntime elapsed (get_mem_type): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result

  let get_slot_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (check_addr: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t) :
      (MemOffset.t * MemRange.t * entry_t) * (Constraint.t list) * bool =
    if check_addr && (not (MemAnno.check_slot smt_ctx orig_addr_off slot_info)) then
      ((SingleExp.SingleTop, SingleExp.SingleTop), RangeConst [], Entry.get_top_untaint_type ()),
      [],
      false
      (* mem_type_error (Printf.sprintf "get_slot_mem_type: Annotation %s does not match memory slot %s"
        (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_off)) *)
    else
      let s_ptr, s_off, is_full, num_slot = slot_info in
      match List.find_opt (fun ((ptr, _), _) -> ptr = s_ptr) mem with
      | None -> mem_type_error (Printf.sprintf "Cannot get slot at %s" (MemAnno.slot_to_string (Some slot_info)))
      | Some (ptr_info, part_mem) ->
        if not (PtrInfo.can_read ptr_info) then mem_type_error (Printf.sprintf "Cannot read from ptr %d" s_ptr)
        else
        if num_slot = 1 then
          let lookup =
            List.find_map (
              fun (off, range, entry) ->
                if MemOffset.cmp off s_off = 0 then
                  if is_full then Some ((off, range, entry), [])
                  else if is_spill_func s_ptr off then Some ((off, range, entry), [Constraint.RangeEq (orig_addr_off, range)])
                  else Some ((off, range, Entry.mem_partial_read_val entry), [])
                else None
            ) part_mem
          in
          begin match lookup with
          | Some (result, cons) -> result, cons, true
          | None -> mem_type_error (Printf.sprintf "Cannot get slot at %s" (MemAnno.slot_to_string (Some slot_info)))
          end
        else if not is_full then mem_type_error "get_slot_mem_type: get multiple slot is not full" 
        else
          match get_mult_slot_part_mem_type smt_ctx s_off part_mem with
          | Some slot, find_num_slot, cons_list ->
            if num_slot = find_num_slot then slot, cons_list, true
            else mem_type_error "get_slot_mem_type: get multiple slot has invalid number of slots"
          | None, _, _ -> mem_type_error "get_slot_mem_type: get multiple slot cannot find match slots"

  let get_heuristic_mem_slot_helper
      (smt_ctx: SmtEmitter.t)
      (var_type_map: SingleExp.var_type_map_t)
      (mem: t)
      (addr_off: MemOffset.t) :
      MemOffset.t option =
    let get_heuristic_part_mem_with_addr
        (part_mem: (MemOffset.t * 'a * 'b) list) 
        (addr: SingleExp.t) (is_addr_left: bool) : MemOffset.t option =
      let cmp_op = if is_addr_left then SingleCondType.Lt else SingleCondType.Le in
      List.find_map (
        fun (entry: MemOffset.t * 'a * 'b) ->
          let off, _, _ = entry in
          let _, off_r = off in
          if SingleCondType.check true smt_ctx [ cmp_op, addr, off_r ] = SatYes then
            Some off
          else None
      ) part_mem
    in
    let choose_left_right (off: MemOffset.t) : bool * SingleExp.SingleVarSet.t =
      let l, r = off in
      let l_var_set = SingleExp.get_vars l in
      let r_var_set = SingleExp.get_vars r in
      if SingleExp.SingleVarSet.cardinal l_var_set <= SingleExp.SingleVarSet.cardinal r_var_set then
        true, l_var_set
      else
        false, r_var_set
    in
    (* let get_other_zero_addr 
        (ptr: int) (addr: SingleExp.t) (addr_var_set: SingleExp.SingleVarSet.t) :
        SingleExp.t =
      let other_var_set = SingleExp.SingleVarSet.remove ptr addr_var_set in
      let other_var_to_zero_map = List.map (fun x -> (x, SingleExp.SingleConst 0L)) (SingleExp.SingleVarSet.to_list other_var_set) in
      SingleExp.repl_local_var other_var_to_zero_map addr
    in *)
    let get_heuristic_addr
        (ptr: int) (off: MemOffset.t) (choose_left: bool) (addr_var_set: SingleExp.SingleVarSet.t) :
        SingleExp.t =
      let l, r = off in
      let addr = if choose_left then l else r in
      let len =
        match SingleExp.eval (SingleBExp (SingleSub, r, l)) with
        | SingleConst c -> c
        | _ -> 0L
      in
      let other_var_set = SingleExp.SingleVarSet.remove ptr addr_var_set in
      let other_var_repl_map =
        List.map (
          fun x ->
            match SingleExp.get_var_type var_type_map x with
            | VarLen -> (x, SingleExp.SingleConst len)
            | _ -> (x, SingleExp.SingleConst 0L)
        ) (SingleExp.SingleVarSet.to_list other_var_set)
      in
      SingleExp.repl_local_var other_var_repl_map addr
    in
    let ptr_set = get_ptr_set mem in
    let l, r = addr_off in
    if SingleExp.cmp l SingleTop = 0 || SingleExp.cmp r SingleTop = 0 then None else
    match SingleExp.find_base l ptr_set, SingleExp.find_base r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r (* || b_l = IsaBasic.rsp_idx *) then None
      else
        let ptr_info, part_mem = get_part_mem mem b_l in
        if not (PtrInfo.can_read ptr_info) then mem_type_error (Printf.sprintf "Cannot read from ptr %d" b_l)
        else
        begin match part_mem with
        | [ target_offset, _, _] -> Some target_offset
        | _ -> 
          let is_left, chosen_addr_var_set = choose_left_right addr_off in
          let other_zero_chosen_addr = get_heuristic_addr b_l addr_off is_left chosen_addr_var_set in
          get_heuristic_part_mem_with_addr part_mem other_zero_chosen_addr is_left
        end
    | _ -> None

  let get_heuristic_mem_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_to_list_func: MemOffset.t * int -> (MemOffset.t list) option)
      (input_var_set: SingleExp.SingleVarSet.t)
      (var_type_map: SingleExp.var_type_map_t)
      (mem: t)
      (addr_off_pc: MemOffset.t * int) :
      (MemOffset.t * int, (SingleContext.t list)) Either.t =
    let orig_off, _ = addr_off_pc in
    let try_simp_off_opt =
      if MemOffset.is_val input_var_set orig_off then Some (orig_off, orig_off)
      else
        match sub_sol_to_list_func addr_off_pc with
        | Some [ simp_off ] -> Some (simp_off, simp_off)
        | Some simp_off_list -> Some (List.hd simp_off_list, orig_off)
        | None -> None
    in
    match try_simp_off_opt with
    | None -> Left addr_off_pc
    | Some (simp_off, (assert_l, assert_r)) ->
      match get_heuristic_mem_slot_helper smt_ctx var_type_map mem simp_off with
      | Some (off_l, off_r) ->
        let cmp_op: SingleCondType.cond = if SingleExp.cmp (fst orig_off) (snd orig_off) = 0 then Le else Lt in
        let cond_list: SingleContext.t list = [
          (* Cond (Le, SingleExp.eval (SingleBExp (SingleSub, off_l, assert_l)), SingleConst 0L); *)
          Cond (Le, off_l, assert_l);
          (* TODO: Think about whether this should be Lt or Le!!! *)
          Cond (cmp_op, assert_l, assert_r);
          Cond (cmp_op, SingleExp.eval (SingleBExp (SingleSub, assert_l, assert_r)), SingleConst 0L);
          (* Cond (Le, SingleExp.eval (SingleBExp (SingleSub, assert_r, off_r)), SingleConst 0L); *)
          Cond (Le, assert_r, off_r);
        ] in
        begin match SingleContext.check_or_assert smt_ctx cond_list with
        | None -> Left addr_off_pc (* addr_off does not belongs to this entry *)
        | Some cond_list -> 
          (* Printf.printf "lookup %s\nslot %s" (MemOffset.to_string (assert_l, assert_r)) (MemOffset.to_string (off_l, off_r));
          SmtEmitter.pp_smt_ctx 0 smt_ctx; *)
          Right cond_list
        end
      | None -> Left addr_off_pc

  (* let get_heuristic_mem_type
      (smt_ctx: SmtEmitter.t)
      (mem: t)
      (addr_off: MemOffset.t) :
      (MemOffset.t, SingleContext.t list) Either.t =
    (* Note: this function might add additional constraints to smt_ctx, please do push/pop outside! *)
    let check_or_assert_helper
        (lookup_offset: MemOffset.t) (target_offset: MemOffset.t) :
        (MemOffset.t, SingleContext.t list) Either.t =
      let l, r = lookup_offset in
      let off_l, off_r = target_offset in
      (* let cond_list: SingleContext.t list = 
        if SingleExp.cmp l r = 0 then
          [
            Cond (Le, SingleExp.eval (SingleBExp (SingleSub, off_l, l)), SingleConst 0L);
            Cond (Le, SingleExp.eval (SingleBExp (SingleSub, r, off_r)), SingleConst 0L);
          ]
        else  
          [
            Cond (Le, SingleExp.eval (SingleBExp (SingleSub, off_l, l)), SingleConst 0L);
            (* TODO: Think about whether this should be Lt or Le!!! *)
            Cond (Lt, SingleExp.eval (SingleBExp (SingleSub, l, r)), SingleConst 0L);
            Cond (Le, SingleExp.eval (SingleBExp (SingleSub, r, off_r)), SingleConst 0L);
          ] 
      in *)
      let cond_list: SingleContext.t list = [
        Cond (Le, SingleExp.eval (SingleBExp (SingleSub, off_l, l)), SingleConst 0L);
        (* TODO: Think about whether this should be Lt or Le!!! *)
        Cond (Le, SingleExp.eval (SingleBExp (SingleSub, l, r)), SingleConst 0L);
        Cond (Le, SingleExp.eval (SingleBExp (SingleSub, r, off_r)), SingleConst 0L);
      ] in
      (* TODO: Check, sat or add, one by one *)
      begin match SingleContext.check_or_assert smt_ctx cond_list with
      | None -> Left addr_off (* addr_off does not belongs to this entry *)
      | Some cond_list -> Right cond_list
      end
    in
    let ptr_set = get_ptr_set mem in
    let l, r = addr_off in
    match SingleExp.find_base l ptr_set, SingleExp.find_base r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r || b_l = IsaBasic.rsp_idx then Left addr_off
      else
        let part_mem = get_part_mem mem b_l in
        begin match part_mem with
        | [ target_offset, _, _ ] ->
          check_or_assert_helper addr_off target_offset
          (* let cond_list = [
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, off_l, l)), SingleExp.SingleConst 0L;
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, l, r)), SingleExp.SingleConst 0L;
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, r, off_r)), SingleExp.SingleConst 0L;
          ] in
          (* TODO: Check, sat or add, one by one *)
          begin match SingleCondType.check_or_assert smt_ctx cond_list with
          | None -> Left addr_off (* addr_off does not belongs to this entry *)
          | Some cond_list -> Right cond_list
          end *)
          (* Left addr_off *)
        | _ -> 
          let other_var_set = SingleExp.SingleVarSet.remove b_l (MemOffset.get_vars addr_off) in
          let other_var_to_zero_map = List.map (fun x -> (x, SingleExp.SingleConst 0L)) (SingleExp.SingleVarSet.to_list other_var_set) in
          let other_zero_addr_off = MemOffset.repl_local_var other_var_to_zero_map addr_off in
          Printf.printf "get_heuristic_mem_type orig off %s heuristic off %s\n" (MemOffset.to_string addr_off) (MemOffset.to_string other_zero_addr_off);
          let lookup_part_mem = get_part_mem_type smt_ctx part_mem other_zero_addr_off other_zero_addr_off in
          begin match lookup_part_mem with
          | Some (_, (target_offset, _, _)) ->
            check_or_assert_helper addr_off target_offset
          | None -> Left addr_off
          end
          (* Left addr_off *)
        end
    | _ -> Left addr_off *)
        
  let is_shared_mem_helper
      (is_quick: bool)
      (smt_ctx: SmtEmitter.t)
      (ptr: IsaBasic.imm_var_id)
      (addr_offset: MemOffset.t) : bool =
    if ptr = IsaBasic.rsp_idx then (* NOTE: this requires on function input, rsp holds ImmVar rsp_idx*)
      let local_stack_start: SingleExp.t = SingleBExp (SingleAdd, SingleVar ptr, SingleConst 8L) in
      match MemOffset.offset_cmp_helper is_quick smt_ctx addr_offset (local_stack_start, local_stack_start) CmpLeGe with
      | Le -> false
      | Ge -> true
      | _ -> 
        SmtEmitter.pp_smt_ctx 0 smt_ctx;
        mem_type_error 
        (Printf.sprintf "is_shared_mem cannot tell whether ptr %d and mem slot %s is shared or not" 
          ptr (MemOffset.to_string addr_offset))
    else true

  let is_shared_mem_quick_cmp = is_shared_mem_helper true
  let is_shared_mem_full_cmp = is_shared_mem_helper false

  let set_mult_slot_part_mem_type_helper
      (* (smt_ctx: SmtEmitter.t) *)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (update_init_range: bool)
      (s_ptr: IsaBasic.imm_var_id)
      (part_mem: entry_t mem_slot list)
      (s_off: MemOffset.t)
      (* (orig_addr_off: MemOffset.t) *) (* We check the slot correctness outside, so orig_addr_off is useless here *)
      (* (slot_info: MemAnno.slot_t) *)
      (new_val: entry_t) :
      (entry_t mem_slot list * (Constraint.t list) * FullMemAnno.slot_t) option =
    let s_left, s_right = s_off in
    (* let s_ptr, (s_left, s_right), is_full, num_slot = slot_info in *)
    (* let is_shared_mem = is_shared_mem_full_cmp smt_ctx s_ptr (s_left, s_right) in *)
    (* if num_slot <= 1 then mem_type_error "set_mult_slot_part_mem_type get incorrect num slot"
    else if not is_full then mem_type_error "set_mult_slot_part_mem_type: set multiple slot is not full"
    else *)
    let helper 
        (acc: MemOffset.t list * (SingleExp.t option)) (entry: entry_t mem_slot) :
        (MemOffset.t list * (SingleExp.t option)) * (int option) =
      let update_rev_off_list, left_opt = acc in
      match left_opt with
      | None -> acc, None (* Do not update *)
      | Some left ->
        let (entry_l, entry_r), _, _ = entry in
        if SingleExp.cmp left entry_l = 0 then
          ((entry_l, entry_r) :: update_rev_off_list,
          if SingleExp.cmp s_right entry_r = 0 then None else Some entry_r),
          Some (List.length update_rev_off_list)
        else
          acc, None
    in
    let (update_rev_off_list, left_opt), update_idx_list = 
      List.fold_left_map helper ([], Some s_left) part_mem
    in
    let num_slot = List.length update_rev_off_list in
    match left_opt with
    | Some _ -> None
    | None -> 
      let split_val_list = Entry.split_val new_val (List.rev update_rev_off_list) in
      (* Printf.printf "set_mult_slot_part_mem_type_helper: set slots\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list MemOffset.sexp_of_t update_rev_off_list));
      Printf.printf "orig val:\n%s\n" (Sexplib.Sexp.to_string_hum (Entry.sexp_of_t new_val));
      Printf.printf "split val list:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list Entry.sexp_of_t split_val_list)); *)
      let update_helper
          (entry: entry_t mem_slot) (update_idx: int option) : 
          entry_t mem_slot * (Constraint.t list) =
        match update_idx with
        | None -> entry, []
        | Some idx ->
          let off, range, entry_val = entry in
          let range: MemRange.t = if update_init_range then RangeConst [ off ] else range in
          let idx_new_val = List.nth split_val_list idx in
          if not (is_spill_func s_ptr off) then
            (* I do not add range must known here, since it is not spill, and is overwritten *)
            (* All slots taint are equal to the same new taint (required by st), so they are the same *)
            (off, range, Entry.set_taint_with_other idx_new_val entry_val), 
            (Entry.get_must_known_taint_constraint entry_val) @
            (Entry.get_eq_taint_constraint entry_val idx_new_val)
          else
            (* All slots taint are updated to the new taint, so they are the same after update *)
            (off, range, idx_new_val), 
            Constraint.RangeOverwritten range :: (Entry.get_overwritten_taint_constraint entry_val)
      in
      let part_mem, constraint_list_list =
        List.map2 update_helper part_mem update_idx_list |> List.split
      in
      Some (part_mem, List.concat constraint_list_list, (s_ptr, s_off, true, num_slot))

  let set_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (update_init_range: bool)
      (ptr: IsaBasic.imm_var_id)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t)
      (new_val: entry_t) :
      ((MemOffset.t * MemRange.t * entry_t) list * (Constraint.t list) * FullMemAnno.slot_t) option =
    let helper
        (acc: (FullMemAnno.slot_t option) * (Constraint.t list)) 
        (entry: MemOffset.t * MemRange.t * entry_t) :
        ((FullMemAnno.slot_t option) * (Constraint.t list)) * (MemOffset.t * MemRange.t * entry_t) =
      match acc with
      | Some _, _ -> acc, entry
      | None, cons ->
        let off, range, entry_val = entry in
        begin match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off CmpSubset with
        | Subset ->
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off CmpEqSubset with
          | Eq -> 
            let new_range: MemRange.t = if update_init_range then RangeConst [ off ] else range in
            if not (is_spill_func ptr off) then
              (Some (ptr, off, true, 1), (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, new_range, new_val)
            else
              (Some (ptr, off, true, 1), cons), (off, new_range, new_val)
          | Subset -> 
            (* TODO: Think about whether we need to subsitute off when adding it to init_mem_range *)
            if is_spill_func ptr off then begin
              let new_range: MemRange.t = RangeConst [orig_addr_off] in
              (Some (ptr, off, false, 1), cons), (off, new_range, new_val)
            end else begin
              (* <TODO> The merge may fail here, and may fail the range infer (checker also has the similar issue). *)
              let new_range: MemRange.t = if update_init_range then MemRange.merge smt_ctx (RangeConst [orig_addr_off]) range |> fst else range in
              (Some (ptr, off, false, 1), (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, new_range, Entry.mem_partial_write_val entry_val new_val)
            end
          | _ -> acc, entry
          end
        | _ -> acc, entry
        end
    in
    let (slot_anno, cons), part_mem = List.fold_left_map helper (None, []) part_mem in
    match slot_anno with
    | Some anno -> Some (part_mem, cons, anno)
    | None -> None
    (* if found then Some (part_mem, cons)
    else None  *)

  let set_mem_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (simp_entry_func: Entry.t -> Entry.t) (* Only used for set mult entries *)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (try_mult_slot_lookup: bool)
      (update_init_range: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t)
      (new_type: entry_t) :
      (t * (Constraint.t list) * FullMemAnno.slot_t) option =
    (* let _ = smt_ctx, mem, addr_offset, new_type in
    None *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    (* let _, _, _, _ = smt_ctx, mem, addr_offset, new_type in *)
    let ptr_set = get_ptr_set mem in
    let simp_l, simp_r = simp_addr_off in
    let result: (t * (Constraint.t list) * FullMemAnno.slot_t) option = 
      match SingleExp.find_base_adv sub_sol_list_func simp_l ptr_set, SingleExp.find_base_adv sub_sol_list_func simp_r ptr_set with
      | Some b_l, Some b_r ->
        if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
        else let ptr_info, part_mem = get_part_mem mem b_l in
        if not (PtrInfo.can_write ptr_info) then mem_type_error (Printf.sprintf "Cannot write to ptr %d" b_l)
        else
        begin match set_part_mem_type smt_ctx is_spill_func update_init_range b_l part_mem orig_addr_off simp_addr_off new_type with
        | Some (new_part_mem, new_cons, slot_anno) ->
          Some (
            List.map (fun ((p, p_info), entry) -> 
              if p = b_l then ((p, p_info), new_part_mem) 
              else (PtrInfo.invalidate_on_write b_l (p, p_info), entry)) mem,
            new_cons, slot_anno
          )
        | None ->
          if try_mult_slot_lookup && enable_mult_slot_lookup smt_ctx b_l orig_addr_off simp_addr_off then begin
            match set_mult_slot_part_mem_type_helper is_spill_func update_init_range b_l part_mem simp_addr_off (simp_entry_func new_type) with
            | Some (new_part_mem, new_cons, slot_anno) ->
              Some (
                List.map (
                  fun ((p, p_info), entry) -> 
                    if p = b_l then ((p, p_info), new_part_mem) 
                    else (PtrInfo.invalidate_on_write b_l (p, p_info), entry)
                ) mem,
                new_cons, slot_anno
              )
            | None -> None
          end else None
        end
      | _ ->
        let related_vars = SingleExp.SingleVarSet.union (SingleExp.get_vars simp_l) (SingleExp.get_vars simp_r) in
        (* heuristic priority: related vars > others; within each group, the offset list sizes are ascending *)
        let reformed_mem = List.mapi (fun order ((x, x_info), x_mem) ->
          ((x, x_info), x_mem, SingleExp.SingleVarSet.exists (fun z -> z = x) related_vars, List.length x_mem, order)
        ) mem
        in
        let sorted_mem = 
          List.sort (fun ((x, _), _, x_related, x_len, _) ((y, _), _, y_related, y_len, _) ->
            if x = y then 0 else
            let len_cmp = Int.compare x_len y_len in
            if x_related && y_related then len_cmp else
            if x_related then -1 else
            if y_related then 1 else
            len_cmp
          ) reformed_mem |> 
          List.map (fun (x, y, _, _, order) -> (x, y, order))
        in
        let helper
            (acc: (FullMemAnno.slot_t option) * (Constraint.t list))
            (entry: PtrInfo.t * ((MemOffset.t * MemRange.t * entry_t) list) * int) :
            ((FullMemAnno.slot_t option) * (Constraint.t list)) * (PtrInfo.t * ((MemOffset.t * MemRange.t * entry_t) list) * int) =
          begin match acc with
          | Some _, _ -> acc, entry
          | None, cons ->
            let (ptr, ptr_info), part_mem, orig_order = entry in 
            begin match set_part_mem_type smt_ctx is_spill_func update_init_range ptr part_mem orig_addr_off simp_addr_off new_type with
            | None -> acc, entry
            | Some (new_part_mem, new_cons, slot_anno) -> (Some slot_anno, cons @ new_cons), ((ptr, ptr_info), new_part_mem, orig_order)
            end
          end
        in
        let (slot_anno, cons), new_mem = List.fold_left_map helper (None, []) sorted_mem in
        match slot_anno with
        | Some anno ->
        (* if found then begin *)
          (* recover the original order *)
          let recovered_mem = 
            List.sort (fun (_, _, x) (_, _, y) -> Int.compare x y) new_mem |> 
            List.map (fun (x, y, _) -> (x, y)) 
          in
          Some (recovered_mem, cons, anno)
        (* end *)
        | None -> None
        (* else None *)
    in
    (* Printf.printf "\ntime elapsed (set_mem_type): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result
    (* TODO!!! *)

  let set_slot_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (update_init_range: bool)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_val: entry_t) :
      (MemOffset.t * MemRange.t * entry_t) list * (Constraint.t list) =
    let s_ptr, s_off, is_full, num_slot = slot_info in
    if num_slot <> 1 then mem_type_error "set_slot_part_mem_type get incorrect num slot"
    else
    let helper
        (acc: (Constraint.t list) option)
        (entry: MemOffset.t * MemRange.t * entry_t) :
        (Constraint.t list) option * (MemOffset.t * MemRange.t * entry_t) =
      match acc with
      | Some _ -> acc, entry
      | None ->
        let off, range, entry_val = entry in
        if MemOffset.cmp off s_off = 0 then
          if is_full then
            let new_range: MemRange.t = if update_init_range then RangeConst [ off ] else range in
            if not (is_spill_func s_ptr s_off) then
              Some (
                (* I do not add range must known here, since it is not spill (will not be overwritten in current version) *)
                (Entry.get_must_known_taint_constraint entry_val) @ 
                (Entry.get_eq_taint_constraint entry_val new_val)), 
              (off, new_range, Entry.set_taint_with_other new_val entry_val)
            else
              Some (Constraint.RangeOverwritten range :: (Entry.get_overwritten_taint_constraint entry_val)), 
              (off, new_range, new_val)
          else if is_spill_func s_ptr s_off then
            let new_range: MemRange.t = RangeConst [orig_addr_off] in
            Some (Constraint.RangeOverwritten range :: (Entry.get_overwritten_taint_constraint entry_val)), 
            (off, new_range, new_val) 
          else
            (* TODO: Think about whether we need to subsitute off when adding it to init_mem_range *)
            (* <TODO> The merge may fail here, and may fail the range infer (checker also has the similar issue). *)
            let new_range: MemRange.t = if update_init_range then MemRange.merge smt_ctx (RangeConst [orig_addr_off]) range |> fst else range in
            Some (
              (* I do not add range must known here, since it is not spill (will not be overwritten in current version) *)
              (Entry.get_must_known_taint_constraint entry_val) @ 
              (Entry.get_eq_taint_constraint entry_val new_val)), 
            (off, new_range, Entry.set_taint_with_other (Entry.mem_partial_write_val entry_val new_val) entry_val)
        else
          None, entry
    in
    match List.fold_left_map helper None part_mem with
    | None, _ -> mem_type_error (Printf.sprintf "set_slot_part_mem_type cannot find slot %s" (MemAnno.slot_to_string (Some slot_info)))
    | Some constaints, part_mem -> part_mem, constaints

  let set_mult_slot_part_mem_type
      (* (smt_ctx: SmtEmitter.t) *)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (update_init_range: bool)
      (part_mem: entry_t mem_slot list)
      (* (orig_addr_off: MemOffset.t) *) (* We check the slot correctness outside, so orig_addr_off is useless here *)
      (slot_info: MemAnno.slot_t)
      (new_val: entry_t) :
      entry_t mem_slot list * (Constraint.t list) =
    let s_ptr, s_off, is_full, num_slot = slot_info in
    if num_slot <= 1 then mem_type_error "set_mult_slot_part_mem_type get incorrect num slot"
    else if not is_full then mem_type_error "set_mult_slot_part_mem_type: set multiple slot is not full"
    else
      match set_mult_slot_part_mem_type_helper is_spill_func update_init_range s_ptr part_mem s_off new_val with
      | Some (new_part_mem, new_cons_list, (new_s_ptr, new_s_off, new_is_full, new_num_slot)) ->
        if s_ptr = new_s_ptr && MemOffset.cmp new_s_off s_off = 0 && new_is_full = is_full && new_num_slot = num_slot then
          new_part_mem, new_cons_list
        else
          mem_type_error "set_mult_slot_part_mem_type: get incorrect mem anno"
      | None -> mem_type_error "set_mult_slot_part_mem_type: cannot find match slots"
  
  let set_slot_mem_type
      (smt_ctx: SmtEmitter.t)
      (simp_entry_func: Entry.t -> Entry.t) (* Only used for set mult entries *)
      (is_spill_func: IsaBasic.imm_var_id -> MemOffset.t -> bool)
      (check_addr: bool)
      (update_init_range: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: entry_t) :
      t * (Constraint.t list) * bool =
    if check_addr && (not (MemAnno.check_slot smt_ctx orig_addr_off slot_info)) then
      mem, [], false
      (* mem_type_error (Printf.sprintf "set_slot_mem_type: Annotation %s does not match memory slot %s"
        (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_off)) *)
    else
      let s_ptr, _, _, num_slot = slot_info in
      match List.find_opt (fun ((ptr, _), _) -> ptr = s_ptr) mem with
      | None -> mem_type_error (Printf.sprintf "set_slot_mem_type cannot find slot %s" (MemAnno.slot_to_string (Some slot_info)))
      | Some (ptr_info, part_mem) ->
        if not (PtrInfo.can_write ptr_info) then mem_type_error (Printf.sprintf "Cannot read from ptr %d" s_ptr)
        else
        let part_mem, constraints = 
          if num_slot = 1 then
            set_slot_part_mem_type smt_ctx is_spill_func update_init_range part_mem orig_addr_off slot_info new_type
          else
            set_mult_slot_part_mem_type is_spill_func update_init_range part_mem slot_info (simp_entry_func new_type)
        in
        List.map (
          fun ((ptr, ptr_info), p_mem) -> 
            if ptr = s_ptr then ((ptr, ptr_info), part_mem) 
            else (PtrInfo.invalidate_on_write s_ptr (ptr, ptr_info), p_mem)
        ) mem,
        constraints,
        true

  let init_stack_update_list (mem: t) : (MemOffset.t * bool) list =
    let find_stack = 
      List.find_map (
        fun ((ptr, _), part_mem) ->
          if ptr = IsaBasic.rsp_idx then Some (List.map (fun (off, _, _) -> off, false) part_mem)
          else None
      ) mem
    in
    match find_stack with
    | Some update_list -> update_list
    | None -> mem_type_error "Cannot find stack memory"

  let update_stack_mem
      (start_var: entry_t)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (update_list: (MemOffset.t * bool) list) :
      entry_t * ((MemOffset.t * MemRange.t * entry_t) list) =
    let helper (acc: entry_t) (off_update: MemOffset.t * bool) : entry_t * (MemOffset.t * MemRange.t * entry_t) =
      let off, update = off_update in
      if update then Entry.next_var acc, (off, MemRange.get_uninit_range (), acc)
      else
        match List.find_opt (fun (o, _, _) -> MemOffset.cmp o off = 0) part_mem with
        | Some entry -> acc, entry
        | None -> mem_type_error (Printf.sprintf "update_stack_mem cannot find unchanged entry %s" (MemOffset.to_string off))
    in
    List.fold_left_map helper start_var update_list

  let update_mem
      (start_var: entry_t)
      (mem: t)
      (update_list: (MemOffset.t * bool) list) :
      entry_t * t =
    List.fold_left_map (
      fun acc ((ptr, ptr_info), entry) ->
        if ptr = IsaBasic.rsp_idx then
          let acc, entry = update_stack_mem acc entry update_list in
          acc, ((ptr, ptr_info), entry)
        else acc, ((ptr, ptr_info), entry)
    ) start_var mem

  (* let remove_local_mem_quick_cmp
      (smt_ctx: SmtEmitter.t)
      (mem: t) : t =
    (* pp_mem_type 0 mem; *)
    (* NOTE: this could also be optimized *)
    List.map (
      fun (ptr, part_mem) ->
        ptr, List.filter (fun (off, _, _) -> is_shared_mem_quick_cmp smt_ctx ptr off) part_mem
    ) mem *)

  let merge_local_mem_quick_cmp
      (smt_ctx: SmtEmitter.t)
      (mem: t) : t =
    List.map (
      fun ((ptr, ptr_info), part_mem) ->
        if ptr = IsaBasic.rsp_idx then
          (* ptr, part_mem *)
          let shared_part_mem, local_part_mem = List.partition (fun (off, _, _) -> is_shared_mem_full_cmp smt_ctx ptr off) part_mem in
          let local_entry : MemOffset.t * MemRange.t * entry_t = (
            match local_part_mem with
            | [] -> 
              pp_mem_type 0 mem;
              mem_type_error "Empty local mem"
            | ((l, r), _, _) :: [] ->
              (l, r), MemRange.RangeConst [], Entry.get_top_taint_type ()
            | ((l, _), _, _) :: tl ->
              let (_, r), _, _ = List.nth tl ((List.length tl) - 1) in
              (l, r), MemRange.RangeConst [], Entry.get_top_taint_type ()
          ) in
          (ptr, ptr_info), (local_entry :: shared_part_mem)
        else (ptr, ptr_info), part_mem
    ) mem

  let get_shared_useful_var_quick_cmp (smt_ctx: SmtEmitter.t) (mem_type: t) : SingleExp.SingleVarSet.t =
    (* An ugly version that avoids copying mem_type *)
    List.fold_left (
      fun (acc: SingleExp.SingleVarSet.t) ((ptr, _), part_mem) ->
        if ptr <> IsaBasic.rsp_idx then
          List.fold_left (
            fun (acc: SingleExp.SingleVarSet.t) (_, _, entry) -> 
              SingleExp.SingleVarSet.union acc (SingleExp.get_vars (Entry.get_single_exp entry))
          ) acc part_mem
        else
          List.fold_left (
            fun (acc: SingleExp.SingleVarSet.t) (off, _, entry) ->
              if is_shared_mem_quick_cmp smt_ctx ptr off then
                SingleExp.SingleVarSet.union acc (SingleExp.get_vars (Entry.get_single_exp entry))
              else acc
          ) acc part_mem
    ) SingleExp.SingleVarSet.empty mem_type
    (* let shared_mem = remove_local_mem_quick_cmp smt_ctx mem_type in
    fold_left (
      fun (acc: SingleExp.SingleVarSet.t) (entry: entry_t) ->
        SingleExp.SingleVarSet.union acc (SingleExp.get_vars (Entry.get_single_exp entry))
    ) SingleExp.SingleVarSet.empty shared_mem *)

  (* let gen_implicit_mem_constraints (smt_ctx: SmtEmitter.t) (mem_type: t) : unit =
    (* let helper (offset_list: (MemOffset.t * 'a * 'b) list) : SmtEmitter.exp_t list =
      match offset_list with
      | [] -> []
      | ((l, r), _, _) :: [] ->
        if SingleExp.cmp l r = 0 then [] 
        else [ SingleCondType.to_smt_expr smt_ctx (SingleCondType.Lt, l, r) ]
      | ((l, _), _, _) :: tl ->
        let( _, r), _, _ = List.nth tl ((List.length tl) - 1) in
        [ SingleCondType.to_smt_expr smt_ctx (SingleCondType.Lt, l, r) ]
    in
    let exps =
      List.fold_left (
        fun (acc: SmtEmitter.exp_t list) (_, part_mem) ->
          (helper part_mem) @ acc
      ) [] mem_type
    in *)
    let helper (acc: SmtEmitter.exp_t list) (part_mem: IsaBasic.imm_var_id * ((MemOffset.t * 'a * 'b) list)) : SmtEmitter.exp_t list =
      let _, offset_list = part_mem in
      match offset_list with
      | [] -> acc
      | ((l, r), _, _) :: [] ->
        if SingleExp.cmp l r = 0 then acc 
        else (SingleCondType.to_smt_expr smt_ctx (SingleCondType.Lt, l, r)) :: acc
      | ((l, _), _, _) :: tl ->
        let( _, r), _, _ = List.nth tl ((List.length tl) - 1) in
        (SingleCondType.to_smt_expr smt_ctx (SingleCondType.Lt, l, r)) :: acc
    in
    let exps = List.fold_left helper [] mem_type in
    SmtEmitter.add_assertions smt_ctx exps *)

  let get_mem_align_constraint_helper (mem_type: t) : (int * int64) list =
    let helper_inner (acc: int64) (entry: 'a mem_slot) : int64 =
      if acc = 16L then acc
      else
        let (l, r), _, _ = entry in
        let len = SingleExp.eval (SingleBExp (SingleSub, r, l)) in
        (* Printf.printf "len %s\n" (SingleExp.to_string len); *)
        SingleExp.get_align [] len
    in
    let helper_outer (part_mem: 'a mem_part) : (int * int64) option =
      let (ptr, _), entry_list = part_mem in
      (* let align = List.fold_left helper_inner 1L entry_list in *)
      let align =
        if ptr = IsaBasic.rsp_idx then 8L
        else if List.length entry_list = 1 then 1L
        else List.fold_left helper_inner 1L entry_list
      in
      (* Printf.printf "align %Ld\n" align; *)
      if align > 1L then Some (ptr, align)
      else None
    in
    List.filter_map helper_outer mem_type

  let get_mem_align_constraint (mem_type: t) : SingleContext.t list =
    List.map (
      fun (ptr, align) -> 
        SingleContext.Cond (Eq, SingleBExp (SingleAnd, SingleVar ptr, SingleConst (Int64.sub align 1L)), SingleConst 0L)
    ) (get_mem_align_constraint_helper mem_type)

  let get_mem_boundary_list (mem_type: t) : (PtrInfo.t * MemOffset.t) list =
    let helper 
        (part_mem: 'a mem_part) :
        (PtrInfo.t * MemOffset.t) option =
      let ptr_info, offset_list = part_mem in
      match offset_list with
      | [] -> None
      | ((l, r), _, _) :: [] ->
        if SingleExp.cmp l r = 0 then None
        else Some (ptr_info, (l, r))
      | ((l, _), _, _) :: tl ->
        let (_, r), _, _ = List.nth tl ((List.length tl) - 1) in
        Some (ptr_info, (l, r))
    in
    List.filter_map helper mem_type

  let get_mem_boundary_constraint_helper (boundary_list: (PtrInfo.t * MemOffset.t) list) : SingleContext.t list =
    List.concat_map (
        fun (_, (l, r)) -> 
          (* get_num_entry_constraint (SingleExp.eval (SingleBExp (SingleSub, r, l))) @  *)
          let len = SingleExp.eval (SingleBExp (SingleSub, r, l)) in
          let len_no_overflow_constraint =
            if SingleExp.is_val SingleExp.SingleVarSet.empty len then []
            else [(SingleContext.NoOverflow len)]
          in
          len_no_overflow_constraint @ [
            SingleContext.Cond (Lt, SingleExp.SingleConst 0L, l);
            SingleContext.Cond (Le, l, r); 
            SingleContext.Cond (Le, SingleExp.SingleConst 0L, len);
            SingleContext.Cond (Le, len, SingleExp.SingleConst (Int64.shift_left 1L 32));
          ]
      ) boundary_list

  let rec get_mem_non_overlap_constraint_helper
      (acc: SingleContext.t list)
      (boundary_list: (PtrInfo.t * MemOffset.t) list) :
      SingleContext.t list =
    match boundary_list with
    | [] -> acc
    | ((_, (hd_overlap_set, _, _)), (hd_l, hd_r)) :: tl ->
      let acc = get_mem_non_overlap_constraint_helper acc tl in
      List.fold_left (
          fun (acc: SingleContext.t list) ((ptr, _), (l, r)) ->
            if IntSet.mem ptr hd_overlap_set then acc
            else 
              (SingleContext.Or [
                Cond (Le, hd_r, l);
                Cond (Le, r, hd_l);
              ]) :: acc
        ) acc tl

  let get_mem_boundary_constraint (mem_type: t) : SingleContext.t list =
    (get_mem_align_constraint mem_type) @
    (get_mem_boundary_constraint_helper (get_mem_boundary_list mem_type))

  let get_mem_non_overlap_constraint (mem_type: t) : SingleContext.t list =
    get_mem_non_overlap_constraint_helper [] (get_mem_boundary_list mem_type)

  let get_all_mem_constraint (mem_type: t) : SingleContext.t list =
    let boundary_list = get_mem_boundary_list mem_type in
    let align_constraint = get_mem_align_constraint mem_type in
    let boundary_constraint = get_mem_boundary_constraint_helper boundary_list in
    let result = get_mem_non_overlap_constraint_helper (align_constraint @ boundary_constraint) boundary_list in
    (* Printf.printf "get_all_mem_constraint\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t result)); *)
    result

end

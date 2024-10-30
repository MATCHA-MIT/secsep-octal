open Pretty_print
open Smt_emitter
open Isa_basic
open Single_exp
open Entry_type
open Mem_offset_new
open Full_mem_anno
open Cond_type_new
open Constraint
(* open Arch_type *)
open Sexplib.Std

module MemKeySet = struct
include Set.Make(Int)
  
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
  
  let find_base (e: SingleExp.t) (ptr_list: t) : IsaBasic.imm_var_id option =
    let p_list = filter_single_var e in
    match to_list (inter p_list ptr_list) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> mem_key_set_error "find_base find more than one base"

end

module MemTypeBasic = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type 'a mem_slot = MemOffset.t * MemRange.t * 'a
  [@@deriving sexp]

  type 'a mem_part = IsaBasic.imm_var_id * (('a mem_slot) list)
  [@@deriving sexp]

  type 'a mem_content = ('a mem_part) list
  [@@deriving sexp]


  let map (func: 'a -> 'b) (mem: 'a mem_content) : 'b mem_content =
    let helper_inner 
        (entry: MemOffset.t * MemRange.t * 'a) : 
        MemOffset.t * MemRange.t * 'b =
      let off, range, e = entry in off, range, func e
    in
    let helper_outer 
        (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) :
        IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list) =
      let ptr, part_mem = entry in
      ptr, List.map helper_inner part_mem
    in
    List.map helper_outer mem

  let map_full
      (func: MemOffset.t * MemRange.t * 'a -> MemOffset.t * MemRange.t * 'b)
      (mem: 'a mem_content) : 'b mem_content =
    let helper_outer
        (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) :
        IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list) =
      let ptr, part_mem = entry in
      ptr, List.map func part_mem
    in
    List.map helper_outer mem

  let map2 
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
        (entry1: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) :
        IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'c) list) =
      let ptr1, part_mem1 = entry1 in
      let ptr2, part_mem2 = entry2 in
      if ptr1 = ptr2 then
        ptr1, List.map2 helper_inner part_mem1 part_mem2
      else
        mem_type_error "[map2] ptr does not match"
    in
    List.map2 helper_outer mem1 mem2

  let map2_full
      (func: MemOffset.t * MemRange.t * 'a -> MemOffset.t * MemRange.t * 'b ->
              MemOffset.t * MemRange.t * 'c)
      (mem1: 'a mem_content) (mem2: 'b mem_content) : 'c mem_content =
    let helper_outer
        (entry1: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) :
        IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'c) list) =
      let ptr1, part_mem1 = entry1 in
      let ptr2, part_mem2 = entry2 in
      if ptr1 = ptr2 then
        ptr1, List.map2 func part_mem1 part_mem2
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
        (acc: 'acc) (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 'acc =
      let _, l = entry in
      List.fold_left helper_inner acc l
    in
    List.fold_left helper_outer acc mem

  let fold_left_full
      (func: 'acc -> MemOffset.t * MemRange.t * 'a -> 'acc)
      (acc: 'acc)
      (mem: 'a mem_content) : 'acc =
    let helper_outer
        (acc: 'acc) (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 'acc =
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
        (acc: 'acc) (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 
        'acc * (IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) =
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
        (acc: 'acc) (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) :
        'acc * (IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) =
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
        (entry1: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) : 'acc =
      let v1, l1 = entry1 in
      let v2, l2 = entry2 in
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
        (entry1: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) : 'acc =
      let v1, l1 = entry1 in
      let v2, l2 = entry2 in
      if v1 = v2 then List.fold_left2 func acc l1 l2
      else mem_type_error "[fold_left2_full] ptr does not match"
    in
    List.fold_left2 helper_outer acc mem1 mem2

  let add_base_to_offset (mem_layout: 'a mem_content) : 'a mem_content =
    List.map (
      fun (base, off_list) ->
        (base,
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

end

module MemType (Entry: EntryType) = struct
  include MemTypeBasic

  type entry_t = Entry.t
  [@@deriving sexp]

  type t = entry_t mem_content
  [@@deriving sexp]

  module MemAnno = FullMemAnno

  let pp_mem_type (lvl: int) (mem: t) =
    PP.print_lvl lvl "<MemType>\n";
    List.iter (
      fun (ptr, off_list) ->
        PP.print_lvl (lvl + 1) "<Ptr %d>\n" ptr;
        List.iter (
          fun (off, r, entry) ->
            PP.print_lvl (lvl + 2) "%s\t%s\t%s\n" (MemOffset.to_string off) (MemRange.to_string r) (Entry.to_string entry)
        ) off_list
    ) mem

  let pp_ocaml_mem_type (lvl: int) (buf: Buffer.t) (mem: t) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun (ptr, off_list) ->
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
    let ptr_list = List.map (fun (x, _) -> x) mem in
    SingleExp.SingleVarSet.of_list ptr_list

  let get_part_mem (mem: 'a mem_content) (ptr: IsaBasic.imm_var_id) : (MemOffset.t * MemRange.t * 'a) list =
    let _, part_mem = List.find (fun (x, _) -> x = ptr) mem in part_mem

  (* get the entry from memory type using strict comparison *)
  let get_mem_type_strict
      (mem: 'a mem_content)
      (location: IsaBasic.imm_var_id * MemOffset.t)
      : 'a option =
    let base, off = location in
    List.find_map (
      fun mem_part ->
        let base', slots = mem_part in
        if base' != base then None else
        List.find_map (
          fun (off', _, entry) ->
            if MemOffset.cmp off' off = 0 then Some entry
            else None
        ) slots
    ) mem

  let get_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) :
      (bool * (MemOffset.t * MemRange.t * entry_t)) option =
    List.find_map (
      fun (off, range, entry) ->
        match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off CmpEqSubset with
        | Eq | Subset ->
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off CmpEqSubset with
          | Eq -> Some (true, (off, range, entry)) (* full read *)
          | Subset -> 
            (* Printf.printf "get_part_mem_type access off %s and convert off %s type %s to top\n" (MemOffset.to_string addr_offset) (MemOffset.to_string off) (Entry.to_string entry); *)
            Some (false, (off, range, Entry.mem_partial_read_val entry)) (* not full read *)
          | _ -> None
          end
        | _ -> None
    ) part_mem

  let get_mem_type
      (smt_ctx: SmtEmitter.t)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) :
      (bool * IsaBasic.imm_var_id * (MemOffset.t * MemRange.t * entry_t)) option =
    (* let _ = smt_ctx, mem, addr_offset in
    None *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    let ptr_set = get_ptr_set mem in
    let simp_l, simp_r = simp_addr_off in
    let result = match SingleExp.find_base simp_l ptr_set, SingleExp.find_base simp_r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
      else 
        let part_mem = get_part_mem mem b_l in
        begin match get_part_mem_type smt_ctx part_mem orig_addr_off simp_addr_off with
        | Some (is_full, find_entry) -> Some (is_full, b_l, find_entry)
        | None -> None
        end
    | _ ->
      let related_vars = SingleExp.SingleVarSet.union (SingleExp.get_vars simp_l) (SingleExp.get_vars simp_r) in
      (* heuristic priority: related vars > others; within each group, the offset list sizes are ascending *)
      let reformed_mem = List.map (fun (x, x_mem) ->
        (x, x_mem, SingleExp.SingleVarSet.exists (fun z -> z = x) related_vars, List.length x_mem)
      ) mem
      in
      let sorted_mem = List.sort (fun (x, _, x_related, x_len) (y, _, y_related, y_len) ->
        if x = y then 0 else
        let len_cmp = Int.compare x_len y_len in
        if x_related && y_related then len_cmp else
        if x_related then -1 else
        if y_related then 1 else
        len_cmp
      ) reformed_mem |> List.map (fun (x, y, _, _) -> (x, y))
      in
      List.find_map (
        fun (ptr, part_mem) -> 
          match get_part_mem_type smt_ctx part_mem orig_addr_off simp_addr_off with
          | Some (is_full, find_entry) -> Some (is_full, ptr, find_entry)
          | None -> None
      ) sorted_mem
    in
    (* Printf.printf "\ntime elapsed (get_mem_type): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result

  let get_slot_mem_type
      (smt_ctx: SmtEmitter.t)
      (check_addr: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t) :
      MemOffset.t * MemRange.t * entry_t =
    if check_addr && (not (MemAnno.check_slot smt_ctx orig_addr_off slot_info)) then
      mem_type_error (Printf.sprintf "get_slot_mem_type: Annotation %s does not match memory slot %s"
        (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_off))
    else
      let s_ptr, s_off, is_full = slot_info in
      match List.find_opt (fun (ptr, _) -> ptr = s_ptr) mem with
      | None -> mem_type_error (Printf.sprintf "Cannot get slot at %s" (MemAnno.slot_to_string (Some slot_info)))
      | Some (_, part_mem) ->
        let lookup =
          List.find_map (
            fun (off, range, entry) ->
              if MemOffset.cmp off s_off = 0 then
                if is_full then Some (off, range, entry)
                else Some (off, range, Entry.mem_partial_read_val entry)
              else None
          ) part_mem
        in
        begin match lookup with
        | Some result -> result
        | None -> mem_type_error (Printf.sprintf "Cannot get slot at %s" (MemAnno.slot_to_string (Some slot_info)))
        end

  let get_heuristic_mem_type
      (smt_ctx: SmtEmitter.t)
      (mem: t)
      (addr_off: MemOffset.t) :
      (MemOffset.t, SingleCondType.t list) Either.t =
    (* Note: this function might add additional constraints to smt_ctx, please do push/pop outside! *)
    let ptr_set = get_ptr_set mem in
    let l, r = addr_off in
    match SingleExp.find_base l ptr_set, SingleExp.find_base r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r then Left addr_off
      else
        let part_mem = get_part_mem mem b_l in
        begin match part_mem with
        | [ (off_l, off_r), _, _ ] ->
          let cond_list = [
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, off_l, l)), SingleExp.SingleConst 0L;
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, l, r)), SingleExp.SingleConst 0L;
            SingleCondType.Le, SingleExp.eval (SingleExp.SingleBExp (SingleSub, r, off_r)), SingleExp.SingleConst 0L;
          ] in
          (* TODO: Check, sat or add, one by one *)
          begin match SingleCondType.check_trivial_or_assert smt_ctx cond_list with
          | None -> Left addr_off (* addr_off does not belongs to this entry *)
          | Some cond_list -> Right cond_list
          end
          (* Left addr_off *)
        | _ -> Left addr_off
        end
    | _ -> Left addr_off
        
  let is_shared_mem_helper
      (is_quick: bool)
      (smt_ctx: SmtEmitter.t)
      (ptr: IsaBasic.imm_var_id)
      (addr_offset: MemOffset.t) : bool =
    if ptr = IsaBasic.rsp_idx then (* NOTE: this requires on function input, rsp holds ImmVar rsp_idx*)
      match MemOffset.offset_cmp_helper is_quick smt_ctx addr_offset (SingleVar ptr, SingleVar ptr) CmpLeGe with
      | Le -> false
      | Ge -> true
      | _ -> 
        mem_type_error 
        (Printf.sprintf "is_shared_mem cannot tell whether ptr %d and mem slot %s is shared or not" 
          ptr (MemOffset.to_string addr_offset))
    else true

  let is_shared_mem_quick_cmp = is_shared_mem_helper true
  let is_shared_mem_full_cmp = is_shared_mem_helper false

  let set_part_mem_type
      (smt_ctx: SmtEmitter.t)
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
        begin match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off CmpEqSubset with
        | Eq | Subset ->
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off CmpEqSubset with
          | Eq -> 
            let range: MemRange.t = if update_init_range then RangeConst [ off ] else range in
            if is_shared_mem_full_cmp smt_ctx ptr orig_addr_off then
              (Some (ptr, off, true), (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, range, new_val)
            else
              (Some (ptr, off, true), cons), (off, range, new_val)
          | Subset -> 
            (* TODO: Think about whether we need to subsitute off when adding it to init_mem_range *)
            let range: MemRange.t = if update_init_range then MemRange.merge smt_ctx (RangeConst [orig_addr_off]) range else range in
            (Some (ptr, off, false), (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, range, Entry.mem_partial_write_val entry_val new_val)
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
      match SingleExp.find_base simp_l ptr_set, SingleExp.find_base simp_r ptr_set with
      | Some b_l, Some b_r ->
        if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
        else let part_mem = get_part_mem mem b_l in
        begin match set_part_mem_type smt_ctx update_init_range b_l part_mem orig_addr_off simp_addr_off new_type with
        | None -> None
        | Some (new_part_mem, new_cons, slot_anno) ->
          Some (
            List.map (fun (p, entry) -> if p = b_l then (p, new_part_mem) else (p, entry)) mem,
            new_cons, slot_anno
          )
        end
      | _ ->
        let related_vars = SingleExp.SingleVarSet.union (SingleExp.get_vars simp_l) (SingleExp.get_vars simp_r) in
        (* heuristic priority: related vars > others; within each group, the offset list sizes are ascending *)
        let reformed_mem = List.mapi (fun order (x, x_mem) ->
          (x, x_mem, SingleExp.SingleVarSet.exists (fun z -> z = x) related_vars, List.length x_mem, order)
        ) mem
        in
        let sorted_mem = List.sort (fun (x, _, x_related, x_len, _) (y, _, y_related, y_len, _) ->
          if x = y then 0 else
          let len_cmp = Int.compare x_len y_len in
          if x_related && y_related then len_cmp else
          if x_related then -1 else
          if y_related then 1 else
          len_cmp
        ) reformed_mem |> List.map (fun (x, y, _, _, order) -> (x, y, order))
        in
        let helper
            (acc: (FullMemAnno.slot_t option) * (Constraint.t list))
            (entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list) * int) :
            ((FullMemAnno.slot_t option) * (Constraint.t list)) * (IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list) * int) =
          begin match acc with
          | Some _, _ -> acc, entry
          | None, cons ->
            let ptr, part_mem, orig_order = entry in 
            begin match set_part_mem_type smt_ctx update_init_range ptr part_mem orig_addr_off simp_addr_off new_type with
            | None -> acc, entry
            | Some (new_part_mem, new_cons, slot_anno) -> (Some slot_anno, cons @ new_cons), (ptr, new_part_mem, orig_order)
            end
          end
        in
        let (slot_anno, cons), new_mem = List.fold_left_map helper (None, []) sorted_mem in
        match slot_anno with
        | Some anno ->
        (* if found then begin *)
          (* recover the original order *)
          let recovered_mem = List.sort (fun (_, _, x) (_, _, y) -> Int.compare x y) new_mem |> List.map (fun (x, y, _) -> (x, y)) in
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
      (update_init_range: bool)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_val: entry_t) :
      (MemOffset.t * MemRange.t * entry_t) list * (Constraint.t list) =
    let helper
        (acc: (Constraint.t list) option)
        (entry: MemOffset.t * MemRange.t * entry_t) :
        (Constraint.t list) option * (MemOffset.t * MemRange.t * entry_t) =
      match acc with
      | Some _ -> acc, entry
      | None ->
        let s_ptr, s_off, is_full = slot_info in
        let off, range, entry_val = entry in
        if MemOffset.cmp off s_off = 0 then
          if is_full then
            let range: MemRange.t = if update_init_range then RangeConst [ off ] else range in
            if is_shared_mem_full_cmp smt_ctx s_ptr s_off then
              Some (Entry.get_eq_taint_constraint entry_val new_val), (off, range, Entry.set_taint_with_other new_val entry_val)
            else
              Some [], (off, range, new_val)
          else
            (* TODO: Think about whether we need to subsitute off when adding it to init_mem_range *)
            let range: MemRange.t = if update_init_range then MemRange.merge smt_ctx (RangeConst [orig_addr_off]) range else range in
            Some (Entry.get_eq_taint_constraint entry_val new_val), (off, range, Entry.set_taint_with_other (Entry.mem_partial_write_val entry_val new_val) entry_val)
        else
          None, entry
    in
    match List.fold_left_map helper None part_mem with
    | None, _ -> mem_type_error (Printf.sprintf "set_slot_part_mem_type cannot find slot %s" (MemAnno.slot_to_string (Some slot_info)))
    | Some constaints, part_mem -> part_mem, constaints

  let set_slot_mem_type
      (smt_ctx: SmtEmitter.t)
      (check_addr: bool)
      (update_init_range: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (slot_info: MemAnno.slot_t)
      (new_type: entry_t) :
      t * (Constraint.t list) =
    if check_addr && (not (MemAnno.check_slot smt_ctx orig_addr_off slot_info)) then
      mem_type_error (Printf.sprintf "set_slot_mem_type: Annotation %s does not match memory slot %s"
        (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_off))
    else
      let s_ptr, _, _ = slot_info in
      match List.find_opt (fun (ptr, _) -> ptr = s_ptr) mem with
      | None -> mem_type_error (Printf.sprintf "set_slot_mem_type cannot find slot %s" (MemAnno.slot_to_string (Some slot_info)))
      | Some (_, part_mem) ->
        let part_mem, constraints = set_slot_part_mem_type smt_ctx update_init_range part_mem orig_addr_off slot_info new_type in
        List.map (fun (ptr, p_mem) -> if ptr = s_ptr then (ptr, part_mem) else (ptr, p_mem)) mem,
        constraints

  let init_stack_update_list (mem: t) : (MemOffset.t * bool) list =
    let find_stack = 
      List.find_map (
        fun (ptr, part_mem) ->
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
      fun acc (ptr, entry) ->
        if ptr = IsaBasic.rsp_idx then
          let acc, entry = update_stack_mem acc entry update_list in
          acc, (ptr, entry)
        else acc, (ptr, entry)
    ) start_var mem

  let remove_local_mem_quick_cmp
      (smt_ctx: SmtEmitter.t)
      (mem: t) : t =
    (* pp_mem_type 0 mem; *)
    (* NOTE: this could also be optimized *)
    List.map (
      fun (ptr, part_mem) ->
        ptr, List.filter (fun (off, _, _) -> is_shared_mem_quick_cmp smt_ctx ptr off) part_mem
    ) mem

  let get_shared_useful_var_quick_cmp (smt_ctx: SmtEmitter.t) (mem_type: t) : SingleExp.SingleVarSet.t =
    (* An ugly version that avoids copying mem_type *)
    List.fold_left (
      fun (acc: SingleExp.SingleVarSet.t) (ptr, part_mem) ->
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

  let gen_implicit_mem_constraints (smt_ctx: SmtEmitter.t) (mem_type: t) : unit =
    (* let helper (offset_list: (MemOffset.t * 'a * 'b) list) : SmtEmitter.exp_t list =
      match offset_list with
      | [] -> []
      | ((l, r), _, _) :: [] ->
        if SingleExp.cmp l r = 0 then [] 
        else [ SingleCondType.get_z3_mk smt_ctx (SingleCondType.Lt, l, r) ]
      | ((l, _), _, _) :: tl ->
        let( _, r), _, _ = List.nth tl ((List.length tl) - 1) in
        [ SingleCondType.get_z3_mk smt_ctx (SingleCondType.Lt, l, r) ]
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
        else (SingleCondType.get_z3_mk smt_ctx (SingleCondType.Lt, l, r)) :: acc
      | ((l, _), _, _) :: tl ->
        let( _, r), _, _ = List.nth tl ((List.length tl) - 1) in
        (SingleCondType.get_z3_mk smt_ctx (SingleCondType.Lt, l, r)) :: acc
    in
    let exps = List.fold_left helper [] mem_type in
    SmtEmitter.add_assertions smt_ctx exps

end

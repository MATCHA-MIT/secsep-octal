open Pretty_print
open Smt_emitter
open Isa
open Single_exp
open Entry_type
open Mem_offset_new
open Constraint
(* open Arch_type *)

module MemKeySet = struct
include Set.Make(Int)
  
  exception MemKeySetError of string
  let mem_key_set_error msg = raise (MemKeySetError ("[Mem Key Set Error] " ^ msg))

  let pp_list (lvl: int) (ptr_list: Isa.imm_var_id list) =
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
  
  let find_base (e: SingleExp.t) (ptr_list: t) : Isa.imm_var_id option =
    let p_list = filter_single_var e in
    match to_list (inter p_list ptr_list) with
    | [] -> None
    | hd :: [] -> Some hd
    | _ -> mem_key_set_error "find_base find more than one base"

end

module MemType (Entry: EntryType) = struct
  exception MemTypeError of string
  let mem_type_error msg = raise (MemTypeError ("[Mem Type Error] " ^ msg))

  type entry_t = Entry.t
  type 'a mem_content = (Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) list
  type t = entry_t mem_content

  let pp_mem_type (lvl: int) (mem: t) =
    PP.print_lvl lvl "<MemType>\n";
    List.iter (
      fun (ptr, off_list) ->
        PP.print_lvl (lvl + 1) "<Ptr %d>\n" ptr;
        List.iter (
          fun (off, _, entry) ->
            PP.print_lvl (lvl + 2) "%s\t%s\n" (MemOffset.to_string off) (Entry.to_string entry)
        ) off_list
    ) mem

  let map (func: 'a -> 'b) (mem: 'a mem_content) : 'b mem_content =
    let helper_inner 
        (entry: MemOffset.t * MemRange.t * 'a) : 
        MemOffset.t * MemRange.t * 'b =
      let off, range, e = entry in off, range, func e
    in
    let helper_outer 
        (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) :
        Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list) =
      let ptr, part_mem = entry in
      ptr, List.map helper_inner part_mem
    in
    List.map helper_outer mem

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
        (acc: 'acc) (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 'acc =
      let _, l = entry in
      List.fold_left helper_inner acc l
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
        (acc: 'acc) (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list)) : 
        'acc * (Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) =
      let id, l = entry in
      let acc, l = List.fold_left_map helper_inner acc l in
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
        (entry1: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'a) list))
        (entry2: Isa.imm_var_id * ((MemOffset.t * MemRange.t * 'b) list)) : 'acc =
      let v1, l1 = entry1 in
      let v2, l2 = entry2 in
      if v1 = v2 then List.fold_left2 helper_inner acc l1 l2
      else mem_type_error "[fold_left2] ptr does not match"
    in
    List.fold_left2 helper_outer acc mem1 mem2

  let add_base_to_offset (mem_layout: 'a mem_content) : 'a mem_content =
    List.map (
      fun (base, off_list) ->
        (base,
        List.map (
          fun ((l, r), range, entry) ->
            (SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, l)), 
            SingleExp.eval (SingleBExp (SingleAdd, SingleVar base, r))),
            range, entry
        ) off_list
        )
    ) mem_layout

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
      ((Isa.imm_var_id * MemOffset.t * MemRange.t * entry_t) * MemOffset.ConstraintSet.t) option =
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

  let get_part_mem (mem: 'a mem_content) (ptr: Isa.imm_var_id) : (MemOffset.t * MemRange.t * 'a) list =
    let _, part_mem = List.find (fun (x, _) -> x = ptr) mem in part_mem

  let get_part_mem_type
      (smt_ctx: SmtEmitter.t)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t) :
      (bool * (MemOffset.t * MemRange.t * entry_t)) option =
    List.find_map (
      fun (off, range, entry) ->
        match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off 1 with
        | Eq | Subset ->
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off 1 with
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
      (bool * (MemOffset.t * MemRange.t * entry_t)) option =
    (* let _ = smt_ctx, mem, addr_offset in
    None *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    let ptr_set = get_ptr_set mem in
    let simp_l, simp_r = simp_addr_off in
    let result = match SingleExp.find_base simp_l ptr_set, SingleExp.find_base simp_r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
      else let part_mem = get_part_mem mem b_l in
      get_part_mem_type smt_ctx part_mem orig_addr_off simp_addr_off
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
        fun (_, part_mem) -> get_part_mem_type smt_ctx part_mem orig_addr_off simp_addr_off
      ) sorted_mem
    in
    (* Printf.printf "\ntime elapsed (get_mem_type): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result

  let is_shared_mem_helper
      (is_quick: bool)
      (smt_ctx: SmtEmitter.t)
      (ptr: Isa.imm_var_id)
      (addr_offset: MemOffset.t) : bool =
    if ptr = Isa.rsp_idx then (* NOTE: this requires on function input, rsp holds ImmVar rsp_idx*)
      match MemOffset.offset_cmp_helper is_quick smt_ctx addr_offset (SingleVar ptr, SingleVar ptr) 2 with
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
      (ptr: Isa.imm_var_id)
      (part_mem: (MemOffset.t * MemRange.t * entry_t) list)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t)
      (new_val: entry_t) :
      ((MemOffset.t * MemRange.t * entry_t) list * (Constraint.t list)) option =
    let helper
        (acc: bool * (Constraint.t list)) 
        (entry: MemOffset.t * MemRange.t * entry_t) :
        (bool * (Constraint.t list)) * (MemOffset.t * MemRange.t * entry_t) =
      match acc with
      | true, _ -> acc, entry
      | false, cons ->
        let off, range, entry_val = entry in
        begin match MemOffset.offset_quick_cmp smt_ctx simp_addr_off off 1 with
        | Eq | Subset ->
          begin match MemOffset.offset_full_cmp smt_ctx orig_addr_off off 1 with
          | Eq -> 
            let range = if update_init_range then [ off ] else range in
            if is_shared_mem_full_cmp smt_ctx ptr orig_addr_off then
              (true, (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, range, new_val)
            else
              (true, cons), (off, range, new_val)
          | Subset -> 
            (* TODO: Think about whether we need to subsitute off when adding it to init_mem_range *)
            let range = if update_init_range then MemRange.merge smt_ctx [ off ] range else range in
            (true, (Entry.get_eq_taint_constraint entry_val new_val) @ cons), (off, range, Entry.mem_partial_write_val entry_val new_val)
          | _ -> acc, entry
          end
        | _ -> acc, entry
        end
    in
    let (found, cons), part_mem = List.fold_left_map helper (false, []) part_mem in
    if found then Some (part_mem, cons)
    else None 

  let set_mem_type
      (smt_ctx: SmtEmitter.t)
      (update_init_range: bool)
      (mem: t)
      (orig_addr_off: MemOffset.t)
      (simp_addr_off: MemOffset.t)
      (new_type: entry_t) :
      (t * (Constraint.t list)) option =
    (* let _ = smt_ctx, mem, addr_offset, new_type in
    None *)
    (* let stamp_beg = Unix.gettimeofday () in *)
    (* let _, _, _, _ = smt_ctx, mem, addr_offset, new_type in *)
    let ptr_set = get_ptr_set mem in
    let simp_l, simp_r = simp_addr_off in
    let result = match SingleExp.find_base simp_l ptr_set, SingleExp.find_base simp_r ptr_set with
    | Some b_l, Some b_r ->
      if b_l <> b_r then mem_type_error (Printf.sprintf "get_mem_type offset base does not match %s" (MemOffset.to_string simp_addr_off))
      else let part_mem = get_part_mem mem b_l in
      begin match set_part_mem_type smt_ctx update_init_range b_l part_mem orig_addr_off simp_addr_off new_type with
      | None -> None
      | Some (new_part_mem, new_cons) ->
        Some (
          List.map (fun (p, entry) -> if p = b_l then (p, new_part_mem) else (p, entry)) mem,
          new_cons
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
          (acc: bool * (Constraint.t list))
          (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list) * int) :
          (bool * (Constraint.t list)) * (Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list) * int) =
        begin match acc with
        | true, _ -> acc, entry
        | false, cons ->
          let ptr, part_mem, orig_order = entry in 
          begin match set_part_mem_type smt_ctx update_init_range ptr part_mem orig_addr_off simp_addr_off new_type with
          | None -> acc, entry
          | Some (new_part_mem, new_cons) -> (true, cons @ new_cons), (ptr, new_part_mem, orig_order)
          end
        end
      in
      let (found, cons), new_mem = List.fold_left_map helper (false, []) sorted_mem in
      if found then begin
        (* recover the original order *)
        let recovered_mem = List.sort (fun (_, _, x) (_, _, y) -> Int.compare x y) new_mem |> List.map (fun (x, y, _) -> (x, y)) in
        Some (recovered_mem, cons)
      end
      else None
    in
    (* Printf.printf "\ntime elapsed (set_mem_type): %f\n" (Unix.gettimeofday () -. stamp_beg); *)
    result
    (* TODO!!! *)

  let init_stack_update_list (mem: t) : (MemOffset.t * bool) list =
    let find_stack = 
      List.find_map (
        fun (ptr, part_mem) ->
          if ptr = Isa.rsp_idx then Some (List.map (fun (off, _, _) -> off, false) part_mem)
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
      if update then Entry.next_var acc, (off, [], acc)
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
        if ptr = Isa.rsp_idx then
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
        if ptr <> Isa.rsp_idx then
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

end

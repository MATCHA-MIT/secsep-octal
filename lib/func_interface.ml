open Isa
open Single_exp
open Entry_type
open Mem_offset_new
open Cond_type_new
open Constraint
open Reg_type_new
open Mem_type_new
open Smt_emitter

module FuncInterface (Entry: EntryType) = struct
  exception FuncInterfaceError of string
  let func_interface_error msg = raise (FuncInterfaceError ("[Func Interface Error] " ^ msg))

  type entry_t = Entry.t
  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  module CondType = CondType (Entry)

  type t = {
    func_name: Isa.label;
    in_reg: RegType.t;
    in_mem: MemType.t;
    context: CondType.t list;
    out_reg: RegType.t;
    out_mem: MemType.t
  }

  let add_reg_var_map 
      (child_reg: RegType.t) (parent_reg: RegType.t) : 
      SingleExp.local_var_map_t * Entry.local_var_map_t =
    let child_single_reg = List.map Entry.get_single_exp child_reg in
    let parent_single_reg = List.map Entry.get_single_exp parent_reg in
    List.fold_left2 SingleExp.add_local_var [] child_single_reg parent_single_reg,
    List.fold_left2 Entry.add_local_var Entry.get_empty_var_map child_reg parent_reg

  type var_map_set_t = SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * Entry.local_var_map_t
  type read_hint_t =
    | Untrans of (MemOffset.t * MemRange.t * entry_t)
    | Unmapped of MemOffset.t
    | Mapped of (MemOffset.t * bool * MemRange.t * entry_t)
  
  let add_mem_var_map
      (smt_ctx: SmtEmitter.t)
      (local_var_map: SingleExp.local_var_map_t) 
      (* local var map of the parent context, not changed, so keep it as a separate parameter *)
      (var_map_set: var_map_set_t)
      (* single_var_map * single_var_set * var_map : 
         single_var_map: child single var -> parent single exp
         single_var_set: child global single var and mapped single var
         var_map: child var -> parent exp (not only for single var/exp) *)
      (child_mem: MemType.t) (parent_mem: MemType.t) :
      (var_map_set_t * SingleExp.SingleVarSet.t) * ((Isa.imm_var_id * (read_hint_t list)) list) =
    let helper_inner
        (acc: var_map_set_t * SingleExp.SingleVarSet.t)
        (entry: read_hint_t) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * read_hint_t =
      let (single_var_map, single_var_set, var_map), acc_useful = acc in
      match entry with
      | Untrans (c_off, c_range, c_entry) ->
        if MemOffset.is_val single_var_set c_off && MemRange.is_val single_var_set c_range then
          let m_off = MemOffset.repl_local_var local_var_map (MemOffset.repl_local_var single_var_map c_off) in
          let m_range = MemRange.repl_local_var local_var_map (MemRange.repl_local_var single_var_map c_range) in
          let useful_vars = SingleExp.SingleVarSet.union (MemOffset.get_vars m_off) (MemRange.get_vars m_range) in
          begin match MemType.get_mem_type smt_ctx parent_mem m_off with
          | Some (is_full, (p_off, _, p_entry)) ->
            let c_exp = Entry.get_single_exp c_entry in
            let p_exp = Entry.get_single_exp p_entry in
            ( (* acc *)
              (SingleExp.add_local_var single_var_map c_exp p_exp,
              SingleExp.SingleVarSet.union single_var_set (SingleExp.get_vars c_exp),
              Entry.add_local_var var_map c_entry p_entry),
              SingleExp.SingleVarSet.union acc_useful useful_vars
            ),
            Mapped (p_off, is_full, m_range, c_entry)
          | None -> acc, Unmapped m_off (* TODO: Maybe only keep real unmapped addresses*)
          end
        else acc, entry
      | _ -> acc, entry
    in
    let helper_outer
        (acc: var_map_set_t * SingleExp.SingleVarSet.t)
        (entry: Isa.imm_var_id * (read_hint_t list)) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * (Isa.imm_var_id * (read_hint_t list)) =
      let (_, single_var_set, _), _ = acc in
      let c_ptr, c_part_mem = entry in
      if SingleExp.SingleVarSet.mem c_ptr single_var_set then
        let acc, c_part_mem = List.fold_left_map helper_inner acc c_part_mem in
        acc, (c_ptr, c_part_mem)
      else
        acc, (c_ptr, c_part_mem)
    in
    let rec helper
        (acc: var_map_set_t * SingleExp.SingleVarSet.t) (child_mem: (Isa.imm_var_id * (read_hint_t list)) list) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * ((Isa.imm_var_id * (read_hint_t list)) list) =
      let (single_var_map, _, _), _ = acc in
      let l = List.length single_var_map in
      let ((single_var_map, single_var_set, var_map), useful_vars), child_mem =
        List.fold_left_map helper_outer acc child_mem
      in
      if List.length single_var_map > l then helper ((single_var_map, single_var_set, var_map), useful_vars) child_mem
      else ((single_var_map, single_var_set, var_map), useful_vars), child_mem
    in
    let child_mem_helper = 
      List.map (
        fun (ptr, part_mem) ->
          ptr, List.map (
            fun (x, y, z) -> Untrans (x, y, z)
          ) part_mem
      ) child_mem
    in
    helper (var_map_set, SingleExp.SingleVarSet.empty) child_mem_helper

  let set_reg_type
      (global_var: SingleExp.SingleVarSet.t)
      (var_map: Entry.local_var_map_t)
      (child_reg: RegType.t) : RegType.t =
    let helper (reg_out: entry_t) : entry_t =
      if Entry.is_val2 global_var var_map reg_out then
        Entry.repl_local_var var_map reg_out
      else Entry.get_top_type
    in
    List.map helper child_reg

  let set_one_entry
      (smt_ctx: SmtEmitter.t)
      (local_var_map: SingleExp.local_var_map_t)
      (var_map_set: var_map_set_t)
      (parent_entry: MemOffset.t * MemRange.t * entry_t)
      (read_hint: bool * MemRange.t * entry_t)
      (write_mem: MemOffset.t * MemRange.t * entry_t) :
      (MemOffset.t * MemRange.t * entry_t) * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let single_var_map, single_var_set, var_map = var_map_set in
    let p_off, p_range, p_entry = parent_entry in
    let is_full, m_in_range, _ = read_hint in
    let _, c_out_range, c_out_entry = write_mem in 
    let read_range_constraint = List.map (fun x -> Constraint.Subset (x, p_range, p_off)) m_in_range in
    let p_range, out_range_constraint, out_range_useful_var =
      if MemRange.is_val single_var_set c_out_range then
        let m_out_range = MemRange.repl_local_var local_var_map (MemRange.repl_local_var single_var_map c_out_range) in
        MemRange.merge smt_ctx p_range m_out_range, [], MemRange.get_vars m_out_range
      else p_range, [ Constraint.Unknown (SingleTop, SingleTop) ], SingleExp.SingleVarSet.empty
    in
    (* Question: do we need this optimization? *)
    (* if Entry.cmp c_in_entry c_out_entry = 0 && Entry.cmp c_in_entry Entry.get_top_type != 0 then 
      (* NOTE: if not top and unchanged, then is not overwritten *)
      (p_off, p_range, p_entry), read_range_constraint @ out_range_constraint
    else  *)
      begin
      let m_out_entry = 
        if Entry.is_val2 single_var_set var_map c_out_entry then Entry.repl_local_var var_map c_out_entry
        else Entry.get_top_type
      in
      let write_val_constraint = Entry.get_write_constraint p_entry m_out_entry in
      if is_full then 
        (p_off, p_range, m_out_entry), 
        read_range_constraint @ out_range_constraint @ write_val_constraint,
        out_range_useful_var
      else 
        (p_off, p_range, Entry.mem_partial_write_val p_entry m_out_entry), 
        read_range_constraint @ out_range_constraint @ write_val_constraint,
        out_range_useful_var
    end

  let set_part_mem
      (smt_ctx: SmtEmitter.t)
      (local_var_map: SingleExp.local_var_map_t)
      (var_map_set: var_map_set_t)
      (parent_entry: (MemOffset.t * MemRange.t * entry_t) list)
      (read_hint: read_hint_t list)
      (write_mem: (MemOffset.t * MemRange.t * entry_t) list) :
      ((MemOffset.t * MemRange.t * entry_t) list) * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let rec helper
        (finished_parent_entry: (MemOffset.t * MemRange.t * entry_t) list)
        (constraint_list: Constraint.t list)
        (useful_vars: SingleExp.SingleVarSet.t)
        (parent_entry: (MemOffset.t * MemRange.t * entry_t) list)
        (read_hint: read_hint_t list)
        (write_mem: (MemOffset.t * MemRange.t * entry_t) list) :
        ((MemOffset.t * MemRange.t * entry_t) list) * (Constraint.t list) * SingleExp.SingleVarSet.t =
      match parent_entry, read_hint, write_mem with
      | _, [], [] -> finished_parent_entry @ parent_entry, constraint_list, useful_vars
      | _, (Untrans _) :: read_hint, _ :: write_mem ->
        helper finished_parent_entry ((Unknown (SingleTop, SingleTop)) :: constraint_list) useful_vars parent_entry read_hint write_mem
      | _, (Unmapped off) :: read_hint, _ :: write_mem ->
        helper finished_parent_entry ((Unknown off) :: constraint_list) useful_vars parent_entry read_hint write_mem
      | (p_off, p_range, p_entry) :: parent_entry_tl,
        (Mapped (mp_off, is_full, m_in_range, c_in_entry)) :: read_hint_tl,
        write_entry :: write_mem_tl ->
        if MemOffset.cmp p_off mp_off = 0 then
          let new_entry, new_constraints, new_useful_vars = 
            set_one_entry smt_ctx local_var_map var_map_set (p_off, p_range, p_entry) (is_full, m_in_range, c_in_entry) write_entry 
          in
          helper (new_entry :: finished_parent_entry) (new_constraints @ constraint_list) (SingleExp.SingleVarSet.union useful_vars new_useful_vars) parent_entry_tl read_hint_tl write_mem_tl
        else
          helper ((p_off, p_range, p_entry) :: finished_parent_entry) constraint_list useful_vars parent_entry_tl read_hint write_mem
      | _ -> func_interface_error "write_one_ptr_entries: unexpected case"
    in
    let rev_parent_entry, constraints, useful_vars = helper [] [] SingleExp.SingleVarSet.empty parent_entry read_hint write_mem in
    List.rev rev_parent_entry, constraints, useful_vars

  let set_mem_type
      (smt_ctx: SmtEmitter.t)
      (local_var_map: SingleExp.local_var_map_t)
      (var_map_set: var_map_set_t)
      (parent_mem: MemType.t)
      (read_hint: (Isa.imm_var_id * (read_hint_t list)) list)
      (write_mem: MemType.t) :
      MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let single_var_map, single_var_set, _ = var_map_set in
    let helper
        (acc: MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t)
        (read_hint_entry: Isa.imm_var_id * (read_hint_t list))
        (write_mem_entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list)) :
        MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t =
      let p_mem, constraint_list, acc_useful_vars = acc in
      let ptr, read_hint = read_hint_entry in
      let ptr2, write_mem = write_mem_entry in
      if ptr = ptr2 then
        if SingleExp.SingleVarSet.mem ptr single_var_set then
          let p_base = SingleExp.repl_local_var local_var_map (SingleExp.repl_local_var single_var_map (SingleVar ptr)) in
          match SingleExp.find_base p_base (MemType.get_ptr_set parent_mem) with
          | Some p_ptr ->
            let find_result =
              List.find_map (
                fun (x, part_mem) ->
                  if x = p_ptr then Some (set_part_mem smt_ctx local_var_map var_map_set part_mem read_hint write_mem)
                  else None
              ) p_mem
            in
            begin match find_result with
            | Some (new_part_mem, new_constraints, new_useful_vars) ->
              List.map (
                fun (x, part_mem) -> 
                  if x = p_ptr then x, new_part_mem else x, part_mem
              ) p_mem, 
              new_constraints @ constraint_list,
              SingleExp.SingleVarSet.union acc_useful_vars new_useful_vars
            | None -> p_mem, (Unknown (SingleTop, SingleTop)) :: constraint_list, acc_useful_vars
            end
          | None -> p_mem, (Unknown (p_base, p_base)) :: constraint_list, acc_useful_vars
        else
          p_mem, (Unknown (SingleTop, SingleTop)) :: constraint_list, acc_useful_vars
      else
        func_interface_error (Printf.sprintf "set_mem_type: read_hint and write_mem ptr does not match %d %d\n" ptr ptr2)
    in
    List.fold_left2 helper (parent_mem, [], SingleExp.SingleVarSet.empty) read_hint write_mem

  let func_call_helper
      (smt_ctx: SmtEmitter.t)
      (global_var_set: SingleExp.SingleVarSet.t)
      (local_var_map: SingleExp.local_var_map_t)
      (child_reg: RegType.t) (child_mem: MemType.t)
      (child_out_reg: RegType.t) (child_out_mem: MemType.t)
      (parent_reg: RegType.t) (parent_mem: MemType.t) :
      RegType.t * MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let single_var_map, var_map = add_reg_var_map child_reg parent_reg in
    let single_var_set = 
      SingleExp.SingleVarSet.union 
        global_var_set
        (SingleExp.SingleVarSet.of_list (List.map (fun (x, _) -> x) single_var_map)) 
    in
    let ((single_var_map, single_var_set, var_map), read_useful_vars), mem_read_hint =
      add_mem_var_map smt_ctx local_var_map (single_var_map, single_var_set, var_map) child_mem parent_mem
    in
    let reg_type = set_reg_type global_var_set var_map child_out_reg in
    let mem_type, constraint_list, write_useful_vars = 
    (* TODO: 1) use local_var_map 2) gen write_useful_vars *)
      set_mem_type smt_ctx local_var_map (single_var_map, single_var_set, var_map) parent_mem mem_read_hint child_out_mem in
    reg_type, mem_type, constraint_list, SingleExp.SingleVarSet.union read_useful_vars write_useful_vars

  (* let add_mem_single_var_map
      (smt_ctx: SmtEmitter.t)
      (var_map_set: SingleExp.local_var_map_t * SingleExp.SingleVarSet.t)
      (child_mem: MemType.t) (parent_mem: MemType.t) :
      SingleExp.local_var_map_t * SingleExp.SingleVarSet.t =
    let helper_inner 
        (acc: SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * bool)
        (entry: MemOffset.t * MemRange.t * entry_t) :
        SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * bool =
      let var_map, var_set, has_unknown = acc in
      let c_off, _, c_entry = entry in
      if MemOffset.is_val var_set c_off then
        let p_off = MemOffset.repl_local_var var_map c_off in
        match MemType.get_mem_type smt_ctx parent_mem p_off with
        | Some (_, _, p_entry) -> 
          let c_exp = Entry.get_single_exp c_entry in
          let p_exp = Entry.get_single_exp p_entry in
          SingleExp.add_local_var var_map c_exp p_exp, 
          SingleExp.SingleVarSet.union var_set (SingleExp.get_vars c_exp),
          has_unknown
        | None -> var_map, var_set, true
      else var_map, var_set, true
    in
    let helper_outer
        (acc: SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * bool)
        (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list)) :
        SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * bool =
      let var_map, var_set, _ = acc in
      let ptr, c_part_mem = entry in
      if SingleExp.SingleVarSet.mem ptr var_set then
        List.fold_left helper_inner acc c_part_mem
      else var_map, var_set, true
      (* match List.find_opt (fun (idx, _) -> idx = ptr) var_map with
      | None -> var_map, true
      | Some _ -> List.fold_left helper_inner acc c_part_mem *)
    in
    let rec helper 
        (var_map_set: SingleExp.local_var_map_t * SingleExp.SingleVarSet.t) : 
        SingleExp.local_var_map_t * SingleExp.SingleVarSet.t =
      let var_map, var_set = var_map_set in
      let l = List.length var_map in
      match List.fold_left helper_outer (var_map, var_set, false) child_mem with
      | var_map, var_set, true -> 
        if List.length var_map > l then helper (var_map, var_set) else (var_map, var_set)
      | var_map, var_set, false -> var_map, var_set
    in
    helper var_map_set

  let add_reg_var_map
      (child_reg: RegType.t) (parent_reg: RegType.t) : 
      Entry.local_var_map_t =
      List.fold_left2 Entry.add_local_var Entry.get_empty_var_map child_reg parent_reg

  let add_mem_var_map
      (smt_ctx: SmtEmitter.t)
      (single_var_map_set: SingleExp.local_var_map_t * SingleExp.SingleVarSet.t)
      (var_map: Entry.local_var_map_t)
      (child_mem: MemType.t) (parent_mem: MemType.t) :
      (Entry.local_var_map_t * (Constraint.t list)) * (bool MemType.mem_content) =
    let single_var_map, single_var_set = single_var_map_set in
    let helper_inner
        (acc: Entry.local_var_map_t * (Constraint.t list)) 
        (entry: MemOffset.t * MemRange.t * entry_t) :
        (Entry.local_var_map_t * (Constraint.t list)) * (MemOffset.t * MemRange.t * bool) =
      let var_map, constraints = acc in
      let c_off, c_range, c_type = entry in
      let m_off = 
        if MemOffset.is_val single_var_set c_off then 
          MemOffset.repl_local_var single_var_map c_off 
        else (SingleTop, SingleTop) 
      in
      let m_range =
        if MemRange.is_val single_var_set c_range then
          MemRange.repl_local_var single_var_map c_range
        else [(SingleTop, SingleTop)]
      in
      match MemType.get_mem_type smt_ctx parent_mem m_off with
      | Some (p_off, p_range, p_type) ->
        let range_constraints = List.map (fun o -> Constraint.Subset (o, p_range, p_off)) m_range in
        let var_map = Entry.add_local_var var_map c_type p_type in
        (var_map, range_constraints @ constraints), (m_off, m_range, true)
      | None ->
        (var_map, (Constraint.Unknown m_off) :: constraints), ((SingleTop, SingleTop), [(SingleTop, SingleTop)], false)
    in
    let helper_outer
        (acc: Entry.local_var_map_t * (Constraint.t list))
        (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list)) :
        (Entry.local_var_map_t * (Constraint.t list)) * (Isa.imm_var_id * (MemOffset.t * MemRange.t * bool) list) =
      let ptr, part_p_mem = entry in
      if SingleExp.SingleVarSet.mem ptr single_var_set then
        let acc, part_m_mem = List.fold_left_map helper_inner acc part_p_mem in
        acc, (ptr, part_m_mem)
      else
        let var_map, constraints = acc in
        (var_map, (Constraint.Unknown (SingleTop, SingleTop)) :: constraints),
        (ptr, List.map (fun _ -> ((SingleExp.SingleTop, SingleExp.SingleTop), [(SingleExp.SingleTop, SingleExp.SingleTop)], false)) part_p_mem)
    in
    List.fold_left_map helper_outer (var_map, []) child_mem

  let set_reg_type
      (global_var: SingleExp.SingleVarSet.t)
      (var_map: Entry.local_var_map_t)
      (child_reg: RegType.t) : RegType.t =
    let helper (reg_out: entry_t) : entry_t =
      if Entry.is_val2 global_var var_map reg_out then
        Entry.repl_local_var var_map reg_out
      else Entry.get_top_type
    in
    List.map helper child_reg

  let set_mem_type
      (global_var: SingleExp.SingleVarSet.t)
      (var_map: Entry.local_var_map_t)
      (write_hint: bool MemType.mem_content)
      (child_mem: MemType.t) (parent_mem: MemType.t) : MemType.t =
    let helper_inner
        (acc: MemType.t * (Constraint.t list))
        (write_hint: MemOffset.t * MemRange.t * bool)
        (child_mem: MemOffset.t * MemRange.t * entry_t) :
        MemType.t * (Constraint.t list) =
      let write_addr, _, write_ready = write_hint in
      let _, child_range, child_type = child_mem in
      let 

  let get_single_var_map 
      (smt_ctx: SmtEmitter.t)
      (global_var_set: SingleExp.SingleVarSet.t)
      (child_reg: RegType.t) (child_mem: MemType.t) 
      (parent_reg: RegType.t) (parent_mem: MemType.t) :
      SingleExp.local_var_map_t =
    let single_var_map = add_reg_single_var_map child_reg parent_reg in
    let single_var_set = 
      SingleExp.SingleVarSet.union 
        global_var_set
        (SingleExp.SingleVarSet.of_list (List.map (fun (x, _) -> x) single_var_map)) 
    in
    let single_var_map_set = 
      add_mem_single_var_map smt_ctx (single_var_map, single_var_set) child_mem parent_mem 
    in
    let var_map = add_reg_var_map child_reg parent_reg in
    let (var_map, constriants), m_mem = add_mem_var_map smt_ctx single_var_map_set var_map child_mem parent_mem in *)





end

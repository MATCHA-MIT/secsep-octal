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

  let add_reg_single_var_map 
      (child_reg: RegType.t) (parent_reg: RegType.t) : 
      SingleExp.local_var_map_t =
    let child_single_reg = List.map Entry.get_single_exp child_reg in
    let parent_single_reg = List.map Entry.get_single_exp parent_reg in
    List.fold_left2 SingleExp.add_local_var [] child_single_reg parent_single_reg

  let add_mem_single_var_map
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

  (* let set_mem_type
      (global_var: SingleExp.SingleVarSet.t)
      (var_map: Entry.local_var_map_t)
      (write_hint: bool MemType.mem_content)
      (child_mem: MemType.t) (parent_mem: MemType.t) : MemType.t =
    let helper_inner
        (acc: paren)

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

open Isa
open Single_exp
open Entry_type
open Mem_offset
open Cond_type_new
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
      (var_map: SingleExp.local_var_map_t)
      (child_mem: MemType.t) (parent_mem: MemType.t) :
      SingleExp.local_var_map_t =
    let helper_inner 
        (acc: SingleExp.local_var_map_t * bool)
        (entry: MemOffset.t * MemRange.t * entry_t) :
        SingleExp.local_var_map_t * bool =
      let var_map, has_unknown = acc in
      let (l, r), _, c_entry = entry in
      let p_off = (SingleExp.repl_local_var var_map l, SingleExp.repl_local_var var_map r) in
      match MemType.get_mem_type smt_ctx parent_mem p_off with
      | Some (_, _, p_entry) -> SingleExp.add_local_var var_map (Entry.get_single_exp c_entry) (Entry.get_single_exp p_entry), has_unknown
      | None -> var_map, true
    in
    let helper_outer
        (acc: SingleExp.local_var_map_t * bool)
        (entry: Isa.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list)) :
        SingleExp.local_var_map_t * bool =
      let var_map, _ = acc in
      let ptr, c_part_mem = entry in
      match List.find_opt (fun (idx, _) -> idx = ptr) var_map with
      | None -> var_map, true
      | Some _ -> List.fold_left helper_inner acc c_part_mem
    in
    let rec helper (var_map: SingleExp.local_var_map_t) : SingleExp.local_var_map_t =
      let l = List.len var_map in
      match List.fold_left helper_outer (var_map, false) child_mem with
      | var_map, true -> 
        if List.len var_map > l then helper var_map else var_map
      | var_map, false -> var_map
    in
    helper var_map

  (* let get_single_var_map 
      (child_reg: RegType.t) (child_mem: MemType.t) 
      (parent_reg: RegType.t) (parent_mem: MemType.t) :
      SingleExp.local_var_map_t =
    let child_single_reg = List.map Entry.get_single_exp child_reg in
    let child_single_mem = MemType.map Entry.get_single_exp child_mem in
    let parent_single_reg = List.map Entry.get_single_exp parent_reg in
    let parent_single_mem = MemType.map Entry.get_single_exp parent_mem in
    let var_map = add_reg_single_var_map child_single_reg parent_single_reg in *)



end

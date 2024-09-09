open Isa
open Single_exp
open Entry_type
open Taint_exp
open Constraint
open Smt_emitter

module TaintEntryType (Entry: EntryType) : EntryType = struct
  exception TaintEntryTypeError of string

  let taint_entry_type_error msg = raise (TaintEntryTypeError ("[Taint Single Type Error] " ^ msg))

  type t = Entry.t * TaintExp.t

  type ext_t = 
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)

  type local_var_map_t = Entry.local_var_map_t * TaintExp.local_var_map_t

  let get_empty_var_map = Entry.get_empty_var_map, []

  let partial_read_val (e: t) : t =
    let single, taint = e in
    Entry.partial_read_val single, TaintExp.partial_read_val taint

  let partial_write_val (e: t) (new_e: t) : t =
    let single, taint = e in
    let new_single, new_taint = new_e in
    Entry.partial_write_val single new_single,
    TaintExp.partial_write_val taint new_taint

  let next_var (e: t) : t =
    let single, taint = e in
    Entry.next_var single, TaintExp.next_var taint

  let to_string (e: t) : string =
    let single, taint = e in
    Printf.sprintf "(%s, %s)"
      (Entry.to_string single)
      (TaintExp.to_string taint)

  let cmp (e1: t) (e2: t) : int =
     let s1, t1 = e1 in
     let s2, t2 = e2 in
     let cmp_s = Entry.cmp s1 s2 in
     if cmp_s = 0 then TaintExp.cmp t1 t2
     else cmp_s

  let read_val (off: int64) (sz: int64) (e: t) : t =
    let single, taint = e in
    Entry.read_val off sz single, taint

  let mem_partial_read_val (e: t) : t =
    let single, taint = e in
    Entry.mem_partial_read_val single, taint

  let mem_partial_write_val (e: t) (new_e: t) : t =
    let single, taint = e in
    let new_single, new_taint = new_e in
    Entry.mem_partial_write_val single new_single, TaintExp.partial_write_val taint new_taint

  let ext_val (ext: ext_t) (off: int64) (sz: int64) (e: t) : t =
    let single, taint = e in
    let s_ext : Entry.ext_t = 
      match ext with
      | SignExt -> SignExt
      | ZeroExt -> ZeroExt
      | OldExt (s, _) -> OldExt s
    in
    Entry.ext_val s_ext off sz single, taint

  let get_eq_taint_constraint (e: t) (new_e: t) : Constraint.t list =
    let single, taint = e in
    let new_single, new_taint = new_e in
    (Entry.get_eq_taint_constraint single new_single) @
    [ Constraint.TaintSub (taint, new_taint); Constraint.TaintSub (new_taint, taint) ]

  let get_untaint_constraint (e: t) : Constraint.t list =
    let _, taint = e in
    [ Constraint.TaintSub (taint, TaintConst false)]

  let update_st_taint_constraint (e: t) (st_taint: TaintExp.t) : t * (Constraint.t list) =
    let single, taint = e in
    (single, st_taint), [ TaintSub (taint, st_taint) ]
  
  let exe_bop_inst (isa_bop: Isa.bop) (e1: t) (e2: t) : t =
    let s1, t1 = e1 in
    let s2, t2 = e2 in
    Entry.exe_bop_inst isa_bop s1 s2,
    TaintExp.merge t1 t2

  let exe_uop_inst (isa_uop: Isa.uop) (e: t) : t =
    let single, taint = e in
    Entry.exe_uop_inst isa_uop single, taint

  let get_single_exp (e: t) : SingleExp.t =
    let single, _ = e in Entry.get_single_exp single

  let get_single_local_var_map (map: local_var_map_t) : SingleExp.local_var_map_t =
    let single_map, _ = map in Entry.get_single_local_var_map single_map

  let get_const_type (imm: Isa.immediate) : t =
    Entry.get_const_type imm, TaintConst false
  
  let get_top_type : t = taint_entry_type_error "Maybe we should not get_top_type for taint"

  let split_option (e: ('a * 'b) option) : ('a option) * ('b option) =
    match e with
    | Some (a, b) -> Some a, Some b
    | None -> None, None

  let get_mem_op_type
      (disp: Isa.immediate option) (base: t option)
      (index: t option) (scale: int64) : t =
    let s_base, t_base = split_option base in
    let s_index, t_index = split_option index in
    Entry.get_mem_op_type disp s_base s_index scale,
    TaintExp.merge_opt t_base t_index
    
  let update_local_var (map: local_var_map_t) (e: t) (pc: int) : (local_var_map_t * t) =
    let single_map, taint_map = map in
    let single, taint = e in
    let single_map, single = Entry.update_local_var single_map single pc in
    let taint_map, taint = TaintExp.update_local_var taint_map taint pc in
    (single_map, taint_map), (single, taint)

  let add_local_var (map: local_var_map_t) (e1: t) (e2: t) : local_var_map_t =
    let single_map, taint_map = map in
    let s1, t1 = e1 in
    let s2, t2 = e2 in
    Entry.add_local_var single_map s1 s2, TaintExp.add_local_var taint_map t1 t2
  
  let add_local_global_var (map: local_var_map_t) (global_var: SingleExp.SingleVarSet.t) : local_var_map_t =
    let single_map, taint_map = map in
    Entry.add_local_global_var single_map global_var, taint_map

  let pp_local_var (lvl: int) (map: local_var_map_t) : unit =
    let single_map, taint_map = map in
    Entry.pp_local_var lvl single_map;
    TaintExp.pp_local_var lvl taint_map

  let repl_local_var (map: local_var_map_t) (e: t) : t =
    let single_map, taint_map = map in
    let single, taint = e in
    Entry.repl_local_var single_map single,
    TaintExp.repl_local_var taint_map taint

  let repl_context_var (map: local_var_map_t) (e: t) : t =
    let single_map, taint_map = map in
    let single, taint = e in
    Entry.repl_context_var single_map single,
    TaintExp.repl_context_var taint_map taint

  let is_val2 (map: local_var_map_t) (e: t) : bool =
    let single_map, taint_map = map in
    let single, taint = e in
    (Entry.is_val2 single_map single) && (TaintExp.is_val2 taint_map taint)

  let to_smt_expr (smg_ctx: SmtEmitter.t) (e: t) : SmtEmitter.exp_t =
    let single, _ = e in
    Entry.to_smt_expr smg_ctx single

end

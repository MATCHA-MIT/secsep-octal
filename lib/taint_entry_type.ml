open Isa
open Single_exp
open Entry_type
open Taint_exp
open Constraint

module TaintEntryType (Entry: EntryType) = struct
  exception TaintEntryTypeError of string

  let taint_entry_type_error msg = raise (TaintEntryTypeError ("[Taint Single Type Error] " ^ msg))

  type t = Entry.t * TaintExp.t

  type ext_t = Entry.ext_t

  type local_var_map_t = Entry.local_var_map_t * TaintExp.local_var_map_t

  let get_empty_var_map = [], []

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
    Entry.ext_val ext off sz single, taint

  let get_write_constraint (e: t) (new_e: t) : Constraint.t list =
    let single, taint = e in
    let new_single, new_taint = new_e in
    (Entry.get_write_constraint single new_single) @
    [ Constraint.TaintSub (taint, new_taint); Constraint.TaintSub (new_taint, taint) ]
  
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
    

end

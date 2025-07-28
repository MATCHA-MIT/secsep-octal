open Isa_basic
open Single_exp
open Entry_type
open Single_exp_basic
open Single_entry_type
open Taint_exp
open Constraint
open Smt_emitter
open Sexplib.Std

module TaintBaseEntryType (Entry: EntryType) = struct
  exception TaintEntryTypeError of string

  let taint_entry_type_error msg = raise (TaintEntryTypeError ("[Taint Single Type Error] " ^ msg))

  type t = Entry.t * TaintExp.t
  [@@deriving sexp]

  type flag_src_t =
  | FlagCmp of t * t
  | FlagBInst of IsaBasic.bop * t * t
  | FlagUInst of IsaBasic.uop * t
  | FlagTInst of IsaBasic.top * t list
  [@@deriving sexp]

  type flag_t = {
    legacy: (t * t) option;   (* legacy left and right *)
    finstr: flag_src_t option; (* extra info recording instr modifying the flag *)
  }
  [@@deriving sexp]

  type ext_t =
    | SignExt
    | ZeroExt
    | OldExt of t (* Used for memory slot partial update *)
  [@@deriving sexp]

  type local_var_map_t = Entry.local_var_map_t * TaintExp.local_var_map_t
  [@@deriving sexp]

  let get_empty_var_map = Entry.get_empty_var_map, []

  let get_empty_var_map_from_init_single_var_map (single_map: SingleExp.local_var_map_t) =
    Entry.get_empty_var_map_from_init_single_var_map single_map, []

  let get_single_var_map (map: local_var_map_t) : SingleExp.local_var_map_t =
    let single_map, _ = map in Entry.get_single_var_map single_map

  let get_taint_var_map (map: local_var_map_t) : TaintExp.local_var_map_t option =
    let _, taint_var_map = map in Some taint_var_map

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

  let to_ocaml_string (e: t) : string =
    let single, taint = e in
    Printf.sprintf "(%s, %s)" (Entry.to_string single) (TaintExp.to_string taint)

  let empty_var_map_to_ocaml_string = "([], [])"

  let cmp (e1: t) (e2: t) : int =
     let s1, t1 = e1 in
     let s2, t2 = e2 in
     let cmp_s = Entry.cmp s1 s2 in
     if cmp_s = 0 then TaintExp.cmp t1 t2
     else cmp_s

  let read_val (orig_sz: int64 option) (off: int64) (sz: int64) (e: t) : t =
    let single, taint = e in
    Entry.read_val orig_sz off sz single, taint

  let write_gpr_partial (off: int64) (sz: int64) (orig_e: t) (write_e: t) : t =
    let orig_single, orig_taint = orig_e in
    let write_single, write_taint = write_e in
    match off, sz with
    | 0L, 1L | 0L, 2L | 1L, 1L -> Entry.write_gpr_partial off sz orig_single write_single, TaintExp.merge orig_taint write_taint
    | _ -> taint_entry_type_error "write_gpr_partial: expecting off,sz=0,1/0,2/1,1"

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
    (
      if TaintExp.cmp new_taint taint = 0 then []
      else [ Constraint.TaintSub (taint, new_taint); Constraint.TaintSub (new_taint, taint) ]
    )

  let get_sub_taint_constraint (sub: t) (sup: t) : Constraint.t list =
    let sub_s, sub_t = sub in
    let sup_s, sup_t = sup in
    (Entry.get_sub_taint_constraint sub_s sup_s) @
    (
      if TaintExp.cmp sub_t sup_t = 0 then []
      else [ Constraint.TaintSub (sub_t, sup_t) ]
    )

  let get_untaint_constraint (e: t) : Constraint.t list =
    let _, taint = e in
    [ Constraint.TaintSub (taint, TaintConst false)]

  let get_overwritten_taint_constraint (e: t) : Constraint.t list =
    let _, taint = e in
    [ Constraint.TaintOverwritten taint ]

  let get_must_known_taint_constraint (e: t) : Constraint.t list =
    let _, taint = e in
    [ Constraint.TaintMustKnown taint ]

  let update_ld_taint_constraint (e: t) (ld_taint: TaintExp.t option) : (Constraint.t list) =
    (* e represents the data loaded from memory *)
    match ld_taint with
    | Some ld_taint ->
      let _, taint = e in
      (* t_mem == t_ld *)
      [ TaintSub (taint, ld_taint); TaintSub (ld_taint, taint); TaintMustKnown taint ]
    | None -> []

  let update_st_taint_constraint (e: t) (st_taint: TaintExp.t option) : t * (Constraint.t list) =
    (* e represents the data about to go into memory *)
    match st_taint with
    | Some st_taint ->
      let single, taint = e in
      (single, st_taint) (* override data's taint with store's taint *), [ TaintSub (taint, st_taint) ] (* t_data => t_st *)
    | None -> e, []

  let get_empty_flag () : flag_t =
    { legacy = None; finstr = None }

  let get_flag_taint (flag: flag_t) : TaintExp.t option =
    match flag.legacy, flag.finstr with
    | Some (l, r), _ -> Some (TaintExp.merge (snd l) (snd r))
    | None, Some (FlagCmp (l, r)) -> Some (TaintExp.merge (snd l) (snd r))
    | None, Some (FlagBInst (_, l, r)) -> Some (TaintExp.merge (snd l) (snd r))
    | None, Some (FlagUInst (_, e)) -> Some (snd e)
    | None, Some (FlagTInst (_, e_list)) -> Some (List.fold_left TaintExp.merge (TaintConst false) (List.map snd e_list))
    | _ -> None

  let make_flag ~(legacy: (t * t) option) ~(finstr: flag_src_t option) : flag_t =
    { legacy; finstr }

  let get_entry_flag (flag: flag_t) : Entry.flag_t =
    let legacy' = Option.map (
      fun (fl, fr) -> (fst fl, fst fr)
    ) flag.legacy
    in
    let finstr' = Option.map (function
      | FlagCmp (l, r) -> Entry.FlagCmp (fst l, fst r)
      | FlagBInst (bop, l, r) -> Entry.FlagBInst (bop, fst l, fst r)
      | FlagUInst (uop, e) -> Entry.FlagUInst (uop, fst e)
      | FlagTInst (top, e_list) -> Entry.FlagTInst (top, List.map fst e_list)
    ) flag.finstr
    in
    { Entry.legacy = legacy'; Entry.finstr = finstr'}
  
  let set_flag_taint (flag: Entry.flag_t) (t: TaintExp.t) : flag_t =
    (* the same taint is assigned to every entry *)
    let set_finstr (finstr: Entry.flag_src_t) : flag_src_t =
      match finstr with
      | Entry.FlagCmp (l, r) -> FlagCmp ((l, t), (r, t))
      | Entry.FlagBInst (bop, l, r) -> FlagBInst (bop, (l, t), (r, t))
      | Entry.FlagUInst (uop, e) -> FlagUInst (uop, (e, t))
      | Entry.FlagTInst (top, e_list) -> FlagTInst (top, List.map (fun e -> (e, t)) e_list)
    in
    match flag.legacy, flag.finstr with
    | Some (l, r), _ -> {
        legacy = Some ((l, t), (r, t));
        finstr = Option.map set_finstr flag.finstr
      }
    | None, _ -> {
        legacy = None;
        finstr = Option.map set_finstr flag.finstr
      }

  let compile_cond_j (cc: IsaBasic.branch_cond) (flag: flag_t) : CondTypeBase.t * t * t =
    let error_msg =
        Printf.sprintf "compile_cond_j: unsupported combination, flag: %s, cc: %s\n"
          (flag |> sexp_of_flag_t |> Sexplib.Sexp.to_string_hum)
          (cc |> IsaBasic.sexp_of_branch_cond |> Sexplib.Sexp.to_string_hum)
    in
    let entry_flag = get_entry_flag flag in
    match cc, flag.finstr with
    | _, Some FlagCmp (_, _)
    | _, Some FlagBInst (IsaBasic.Sub, _, _)
    | _, Some FlagUInst (IsaBasic.Dec, _) -> (
      let (_, t1), (_, t2) = Option.get flag.legacy in
      let cond, e1, e2 = Entry.compile_cond_j cc entry_flag in
      (* assign combined taint to both sides *)
      (* t1 and t2 may differ, e.g. Cmp assigns two operands that may have different taint *)
      (* therefore, merge is necessary *)
      let taint = TaintExp.merge t1 t2 in
      (cond, (e1, taint), (e2, taint))
    )
    | _, Some (FlagBInst (_, (_, t1), (_, t2))) ->
      let cond, e1, e2 = Entry.compile_cond_j cc entry_flag in
      let taint = TaintExp.merge t1 t2 in
      (* assign combined taint to both sides *)
      (* actually t1 and t2 are the same, so merge is not necessary *)
      (* see how we set flag's taint when executing BInst other than Sub *)
      (cond, (e1, taint), (e2, taint))
    | _, Some (FlagUInst (_, (_, taint))) ->
      let cond, e1, e2 = Entry.compile_cond_j cc entry_flag in
      (* assign taint to both sides *)
      (cond, (e1, taint), (e2, taint))
    | _ -> taint_entry_type_error error_msg

  let compile_cond_cmov (cc: IsaBasic.bop) (flag: flag_t) : CondTypeBase.t * t * t =
    let dummy_j: IsaBasic.branch_cond = match cc with
    | CmovNe -> JNe
    | CmovE -> JE
    | CmovL -> JL
    | CmovLe -> JLe
    | CmovG -> JG
    | CmovGe -> JGe
    | CmovB -> JB
    | CmovBe -> JBe
    | CmovA -> JA
    | CmovAe -> JAe
    | CmovOther -> JOther
    | _ -> taint_entry_type_error "compile_cond_cmov: expecting cmovxx"
    in
    compile_cond_j dummy_j flag

  let compile_cond_set (cc: IsaBasic.uop) (flag: flag_t) : CondTypeBase.t * t * t =
    let dummy_j: IsaBasic.branch_cond = match cc with
    | SetNe -> JNe
    | SetE -> JE
    | SetL -> JL
    | SetLe -> JLe
    | SetG -> JG
    | SetGe -> JGe
    | SetB -> JB
    | SetBe -> JBe
    | SetA -> JA
    | SetAe -> JAe
    | SetOther -> JOther
    | _ -> taint_entry_type_error "compile_cond_set: expecting setxx"
    in
    compile_cond_j dummy_j flag

  let exe_bop_inst (isa_bop: IsaBasic.bop) (e1: t) (e2: t) (flag: flag_t) (same_op: bool): t * flag_t =
    let s1, t1 = e1 in
    let s2, t2 = e2 in
    let entry_flag = get_entry_flag flag in
    let dest_entry_type, entry_flag = Entry.exe_bop_inst isa_bop s1 s2 entry_flag same_op in
    let (dest_taint_type: TaintExp.t) =
      if (isa_bop = Xor || isa_bop = Xorp || isa_bop = Pxor) && same_op then
        TaintConst false
      else begin
        let new_t = TaintExp.merge t1 t2 in
        if IsaBasic.bop_result_depends_on_flag isa_bop then
          TaintExp.merge new_t (get_flag_taint flag |> Option.get)
        else
          new_t
      end
    in
    let new_flag = match isa_bop with
    | Add | Adc | Sub | Sbb | Mul | Imul | Sal | Shl | Sar | Shr | Rol | Ror | Xor | And | Or | Bt ->
      set_flag_taint entry_flag dest_taint_type
    | CmovNe | CmovE | CmovL | CmovLe | CmovG | CmovGe
    | CmovB | CmovBe | CmovA | CmovAe | CmovOther
    | Punpck | Packxs
    | Pshuf
    | Padd | Psub | Pxor | Pand | Pandn | Por
    | Psll | Psrl
    | Xorp ->
      (* flag is untouched *)
      flag
    in
    (dest_entry_type, dest_taint_type), new_flag


  let exe_uop_inst (isa_uop: IsaBasic.uop) (e: t) (flag: flag_t) : t * flag_t =
    let single, taint = e in
    let entry_flag = get_entry_flag flag in
    let dest_entry_type, entry_flag = Entry.exe_uop_inst isa_uop single entry_flag in
    let dest_taint_type = if IsaBasic.uop_result_depends_on_flag isa_uop then
      TaintExp.merge taint (get_flag_taint flag |> Option.get)
    else
      taint
    in
    let new_flag = match isa_uop with
      | Mov | MovZ | MovS | Lea | Not | Bswap
      (* set does not affect any flags *)
      | SetNe | SetE | SetL | SetLe | SetG | SetGe
      | SetB | SetBe | SetA | SetAe | SetOther ->
        (* flag is untouched *)
        flag
      | Neg | Inc | Dec ->
        set_flag_taint entry_flag dest_taint_type
    in
    (dest_entry_type, dest_taint_type), new_flag

  let exe_top_inst (isa_top: IsaBasic.top) (e_list: t list) (flag: flag_t) : t * flag_t =
    let single_list, t_list = List.split e_list in
    let entry_flag = get_entry_flag flag in
    let dest_entry_type, entry_flag = Entry.exe_top_inst isa_top single_list entry_flag in
    let dest_taint_type = List.fold_left TaintExp.merge (TaintConst false) t_list in
    let dest_taint_type = if IsaBasic.top_result_depends_on_flag isa_top then
      TaintExp.merge dest_taint_type (get_flag_taint flag |> Option.get)
    else
      dest_taint_type
    in
    let new_flag = match isa_top with
    | Shld | Shrd ->
      set_flag_taint entry_flag dest_taint_type
    | Shufp ->
      (* flag is untouched *)
      flag
    in
    (dest_entry_type, dest_taint_type), new_flag

  let get_single_exp (e: t) : SingleExp.t =
    let single, _ = e in Entry.get_single_exp single

  let get_taint_exp (e: t) : TaintExp.t option =
    let _, taint = e in Some taint

  let get_single_taint_exp (e: t) : SingleExp.t * TaintExp.t =
    let single, taint = e in Entry.get_single_exp single, taint

  let set_taint_with_other (x: t) (y: t) : t =
    (* Set x's taint with y's taint*)
    let s, _ = x in
    let _, t = y in
    s, t

  let get_const_type (imm: IsaBasic.immediate) : t =
    Entry.get_const_type imm, TaintConst false

  let get_top_type () : t = taint_entry_type_error "Maybe we should not get_top_type for taint"
  let get_top_untaint_type () : t = (Entry.get_top_untaint_type (), TaintConst false)

  let get_top_taint_type () : t = (Entry.get_top_taint_type (), TaintConst true)

  let get_unknown_taint_type () : t = (Entry.get_unknown_taint_type (), TaintUnknown)

  let split_option (e: ('a * 'b) option) : ('a option) * ('b option) =
    match e with
    | Some (a, b) -> Some a, Some b
    | None -> None, None

  let get_mem_op_type
      (disp: IsaBasic.immediate option) (base: t option)
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

  let add_context_map
      (is_mem: bool)
      (simp_func: SingleExp.t -> SingleExp.t)
      (map: local_var_map_t) (e1: t) (e2: t) :
      local_var_map_t =
    let single_map, taint_map = map in
    let s1, t1 = e1 in
    let s2, t2 = e2 in
    Entry.add_context_map is_mem simp_func single_map s1 s2,
    TaintExp.add_context_map is_mem taint_map t1 t2
  
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

  let flag_repl_local_var (map: local_var_map_t) (flag: flag_t) : flag_t =
    let repl = repl_local_var map in
    let legacy' = Option.map (fun (l, r) -> (repl l, repl r)) flag.legacy in
    let finstr' = Option.map (function
      | FlagCmp (l, r) -> FlagCmp (repl l, repl r)
      | FlagBInst (bop, l, r) -> FlagBInst (bop, repl l, repl r)
      | FlagUInst (uop, e) -> FlagUInst (uop, repl e)
      | FlagTInst (top, e_list) -> FlagTInst (top, List.map repl e_list)
    ) flag.finstr
    in
    { legacy = legacy'; finstr = finstr' }

  let flag_get_useful_vars (flag: flag_t) : SingleExp.SingleVarSet.t =
    let entry_flag = get_entry_flag flag in
    Entry.flag_get_useful_vars entry_flag

  let is_val2 (map: local_var_map_t) (e: t) : bool =
    let single_map, taint_map = map in
    let single, taint = e in
    (Entry.is_val2 single_map single) && (TaintExp.is_val2 taint_map taint)

  let to_smt_expr ?(get_var_size: (int -> int option) option = None) (smg_ctx: SmtEmitter.t) (e: t) : SmtEmitter.exp_t =
    let single, _ = e in
    Entry.to_smt_expr ~get_var_size:get_var_size smg_ctx single

  let split_val (e: t) (off_list: (SingleExp.t * SingleExp.t) list) : t list =
    let single, taint = e in
    let single_list = Entry.split_val single off_list in
    List.map (
      fun x -> x, taint
    ) single_list

end

module TaintEntryType = TaintBaseEntryType (SingleEntryType)

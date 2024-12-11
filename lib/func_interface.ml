open Isa_basic
open Single_exp
open Entry_type
open Mem_offset_new
open Single_context
open Constraint
open Single_entry_type
open Taint_entry_type
open Reg_type_new
open Mem_type_new
open Call_anno_type
open Smt_emitter
open Pretty_print
open Sexplib.Std

module FuncInterface (Entry: EntryType) = struct
  exception FuncInterfaceError of string
  let func_interface_error msg = raise (FuncInterfaceError ("[Func Interface Error] " ^ msg))

  type entry_t = Entry.t
  [@@deriving sexp]

  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  (* module CondType = CondType (Entry) *)

  type t = {
    func_name: IsaBasic.label;
    in_reg: RegType.t;
    in_mem: MemType.t;
    context: SingleContext.t list;
    out_reg: RegType.t;
    out_mem: MemType.t;
    base_info: CallAnno.base_info MemType.mem_content;
  }
  [@@deriving sexp]

  let interface_list_to_file (filename: string) (interface_list: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t interface_list)

  let interface_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp

  let pp_func_interface (lvl: int) (interface: t) =
    Printf.printf "\n";
    PP.print_lvl lvl "Func interface of func %s\n" interface.func_name;
    PP.print_lvl lvl "Input\n";
    RegType.pp_reg_type 0 interface.in_reg;
    MemType.pp_mem_type 0 interface.in_mem;
    PP.print_lvl lvl "Context\n";
    Sexplib.Sexp.output_hum stdout (sexp_of_list SingleContext.sexp_of_t interface.context);
    (* SingleContext.pp_cond_list 0 interface.context; *)
    PP.print_lvl lvl "Output\n";
    RegType.pp_reg_type 0 interface.out_reg;
    MemType.pp_mem_type 0 interface.out_mem;
    Printf.printf "\n"

  let pp_ocaml_func_interface (lvl: int) (buf: Buffer.t) (interface: t) =
    PP.bprint_lvl lvl buf "{\n";
    PP.bprint_lvl (lvl + 1) buf "func_name = \"%s\";\n" interface.func_name;
    PP.bprint_lvl (lvl + 1) buf "in_reg =\n";
    RegType.pp_ocaml_reg_type (lvl + 2) buf interface.in_reg;
    PP.bprint_lvl (lvl + 1) buf "in_mem =\n";
    MemType.pp_ocaml_mem_type (lvl + 2) buf interface.in_mem;
    PP.bprint_lvl (lvl + 1) buf "context =\n";
    (* Note that this function is deprecated, so I didn't support print the correct context here. *)
    Sexplib.Sexp.output_hum stdout (sexp_of_list SingleContext.sexp_of_t interface.context);
    (* SingleCondType.pp_ocaml_cond_list (lvl + 2) buf interface.context; *)
    PP.bprint_lvl (lvl + 1) buf "out_reg =\n";
    RegType.pp_ocaml_reg_type (lvl + 2) buf interface.out_reg;
    PP.bprint_lvl (lvl + 1) buf "out_mem =\n";
    MemType.pp_ocaml_mem_type (lvl + 2) buf interface.out_mem

  let add_reg_var_map
      (simp_local_var: SingleExp.t -> SingleExp.t)
      (child_reg: RegType.t) (parent_reg: RegType.t) : 
      SingleExp.local_var_map_t * Entry.local_var_map_t =
    let child_single_reg = List.map Entry.get_single_exp child_reg in
    let parent_single_reg = List.map Entry.get_single_exp parent_reg in
    List.fold_left2 (SingleExp.add_local_var_simp simp_local_var) [] child_single_reg parent_single_reg,
    List.fold_left2 (Entry.add_context_map false simp_local_var) Entry.get_empty_var_map child_reg parent_reg

  type var_map_set_t = SingleExp.local_var_map_t * SingleExp.SingleVarSet.t * Entry.local_var_map_t
  [@@deriving sexp]

  type read_hint_t =
    | Untrans of (MemOffset.t * MemRange.t * entry_t)
    | Unmapped of MemOffset.t
    | Mapped of (IsaBasic.imm_var_id * MemOffset.t * bool * MemRange.t * entry_t)
  [@@deriving sexp]

  let add_mem_var_map
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (simp_local_var: SingleExp.t -> SingleExp.t)
      (* Simp local var of the parent context, not changed, so keep it as a separate parameter *)
      (* (local_var_map: SingleExp.local_var_map_t)  *)
      (* local var map of the parent context, not changed, so keep it as a separate parameter *)
      (var_map_set: var_map_set_t)
      (* single_var_map * single_var_set * var_map : 
         single_var_map: child single var -> parent single exp
         single_var_set: child global single var and mapped single var
         var_map: child var -> parent exp (not only for single var/exp) *)
      (child_mem: MemType.t) (parent_mem: MemType.t) :
      (var_map_set_t * SingleExp.SingleVarSet.t) * ((IsaBasic.imm_var_id * (read_hint_t list)) list) =
    let helper_inner
        (acc: var_map_set_t * SingleExp.SingleVarSet.t)
        (entry: read_hint_t) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * read_hint_t =
      let (single_var_map, single_var_set, var_map), acc_useful = acc in
      match entry with
      | Untrans (c_off, c_range, c_entry) ->
        if MemOffset.is_val single_var_set c_off && MemRange.is_val single_var_set c_range then
          let orig_m_off = MemOffset.repl_context_var single_var_map c_off in
          let m_range = MemRange.repl_context_var single_var_map c_range in
          (* Update useful vars as long as addr can be represented by parent vars *)
          let useful_vars = SingleExp.SingleVarSet.union (MemOffset.get_vars orig_m_off) (MemRange.get_vars m_range) in
          let acc_useful = SingleExp.SingleVarSet.union acc_useful useful_vars in
          let m_off_l, m_off_r = orig_m_off in
          if SingleExp.cmp m_off_l SingleTop = 0 || SingleExp.cmp m_off_r SingleTop = 0 then 
            (* NOTE: This is the case where var in addr has sol while the sol is SingleTop 
              sub_sol_func will still get Some xx, but we should treat the offset in the same way as no sol case!!! *)
            ( (* acc *)
              (single_var_map, single_var_set, var_map),
              acc_useful), 
            Unmapped (SingleTop, SingleTop) (* Use Top to avoid lookup addr with Top sol in update_mem *)
          else
          begin match sub_sol_func m_off_l, sub_sol_func m_off_r with
          | Some (m_off_l, _), Some (_, m_off_r) ->
            let simp_m_off = m_off_l, m_off_r in
            begin match MemType.get_mem_type smt_ctx sub_sol_list_func parent_mem orig_m_off simp_m_off with
            | Some (is_full, p_ptr, (p_off, _, p_entry)) ->
              let c_exp = Entry.get_single_exp c_entry in
              let p_exp = Entry.get_single_exp p_entry in
              ( (* acc *)
                (SingleExp.add_local_var_simp simp_local_var single_var_map c_exp p_exp,
                SingleExp.SingleVarSet.union single_var_set (SingleExp.get_vars c_exp),
                (Entry.add_context_map true simp_local_var) var_map c_entry p_entry),
                acc_useful
              ),
              Mapped (p_ptr, p_off, is_full, m_range, c_entry)
            | None -> 
              (* Printf.printf "Unmapped m_off %s c_off %s\n" (MemOffset.to_string m_off) (MemOffset.to_string c_off); *)
              (* SingleExp.pp_local_var 0 single_var_map; *)
              ( (* acc *)
                (single_var_map, single_var_set, var_map),
                acc_useful), 
              (* Use simp_m_off since we do not need to distinguish between eq and subset for resolving unknown address *)
              Unmapped simp_m_off (* TODO: Maybe only keep real unmapped addresses*)
            end
          | _ -> 
            (* Printf.printf "Unresolved addr (parent ctx) %s\n" (MemOffset.to_string m_off); *)
            ( (* acc *)
              (single_var_map, single_var_set, var_map),
              acc_useful), 
            Unmapped (SingleTop, SingleTop) (* Use Top to avoid lookup unresolved addr in update_mem *)
          end
        else acc, entry
      | _ -> acc, entry
    in
    let helper_outer
        (acc: var_map_set_t * SingleExp.SingleVarSet.t)
        (entry: IsaBasic.imm_var_id * (read_hint_t list)) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * (IsaBasic.imm_var_id * (read_hint_t list)) =
      let (_, single_var_set, _), _ = acc in
      let c_ptr, c_part_mem = entry in
      if SingleExp.SingleVarSet.mem c_ptr single_var_set then
        let acc, c_part_mem = List.fold_left_map helper_inner acc c_part_mem in
        acc, (c_ptr, c_part_mem)
      else
        acc, (c_ptr, c_part_mem)
    in
    let rec helper
        (acc: var_map_set_t * SingleExp.SingleVarSet.t) (child_mem: (IsaBasic.imm_var_id * (read_hint_t list)) list) :
        (var_map_set_t * SingleExp.SingleVarSet.t) * ((IsaBasic.imm_var_id * (read_hint_t list)) list) =
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
      (var_map: Entry.local_var_map_t)
      (child_reg: RegType.t) : RegType.t =
    let helper (reg_out: entry_t) : entry_t =
      if Entry.is_val2 var_map reg_out then
        Entry.repl_context_var var_map reg_out
      else begin
        Printf.printf "set_reg_type get_top_type for %s\n" (Entry.to_string reg_out);
        Entry.get_top_type ()
      end
    in
    List.map helper child_reg

  let set_one_entry
      (smt_ctx: SmtEmitter.t)
      (* (local_var_map: SingleExp.local_var_map_t) *)
      (var_map_set: var_map_set_t)
      (parent_entry: MemOffset.t * MemRange.t * entry_t)
      (read_hint: bool * MemRange.t * entry_t)
      (write_mem: MemOffset.t * MemRange.t * entry_t) :
      (MemOffset.t * MemRange.t * entry_t) * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let single_var_map, single_var_set, var_map = var_map_set in
    let p_off, p_range, p_entry = parent_entry in
    let is_full, m_in_range, _ = read_hint in
    let _, c_out_range, c_out_entry = write_mem in 
    let read_range_constraint = Constraint.gen_range_subset m_in_range p_range p_off in
    let p_range, out_range_constraint, out_range_useful_var =
      if MemRange.is_val single_var_set c_out_range then
        let m_out_range = MemRange.repl_context_var single_var_map c_out_range in
        MemRange.merge smt_ctx p_range m_out_range, [], MemRange.get_vars m_out_range
      else p_range, [ Constraint.Unknown (SingleTop, SingleTop) ], SingleExp.SingleVarSet.empty
    in
    (* Question: do we need this optimization? *)
    (* if Entry.cmp c_in_entry c_out_entry = 0 && Entry.cmp c_in_entry Entry.get_top_type <> 0 then 
      (* NOTE: if not top and unchanged, then is not overwritten *)
      (p_off, p_range, p_entry), read_range_constraint @ out_range_constraint
    else  *)
      begin
      let m_out_entry = 
        if Entry.is_val2 var_map c_out_entry then Entry.repl_context_var var_map c_out_entry
        else Entry.get_top_type ()
      in
      (* We don't need this here since it is ensured by taint type check/inference of the child function!!! *)
      let write_val_constraint = Entry.get_eq_taint_constraint p_entry m_out_entry in
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
      (* (local_var_map: SingleExp.local_var_map_t) *)
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
      | _, [], [] -> (List.rev parent_entry) @ finished_parent_entry, constraint_list, useful_vars
      | _, (Untrans _) :: read_hint, _ :: write_mem ->
        helper finished_parent_entry ((Unknown (SingleTop, SingleTop)) :: constraint_list) useful_vars parent_entry read_hint write_mem
      | _, (Unmapped off) :: read_hint, _ :: write_mem ->
        helper finished_parent_entry ((Unknown off) :: constraint_list) useful_vars parent_entry read_hint write_mem
      | (p_off, p_range, p_entry) :: parent_entry_tl,
        (Mapped (_, mp_off, is_full, m_in_range, c_in_entry)) :: read_hint_tl,
        write_entry :: write_mem_tl ->
        if MemOffset.cmp p_off mp_off = 0 then
          let new_entry, new_constraints, new_useful_vars = 
            set_one_entry smt_ctx var_map_set (p_off, p_range, p_entry) (is_full, m_in_range, c_in_entry) write_entry 
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
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (* (local_var_map: SingleExp.local_var_map_t) *)
      (var_map_set: var_map_set_t)
      (parent_mem: MemType.t)
      (read_hint: (IsaBasic.imm_var_id * (read_hint_t list)) list)
      (write_mem: MemType.t) :
      MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    let single_var_map, single_var_set, _ = var_map_set in
    let helper
        (acc: MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t)
        (read_hint_entry: IsaBasic.imm_var_id * (read_hint_t list))
        (write_mem_entry: IsaBasic.imm_var_id * ((MemOffset.t * MemRange.t * entry_t) list)) :
        MemType.t * (Constraint.t list) * SingleExp.SingleVarSet.t =
      let p_mem, constraint_list, acc_useful_vars = acc in
      let ptr, read_hint = read_hint_entry in
      let ptr2, write_mem = write_mem_entry in
      if ptr = ptr2 then
        if SingleExp.SingleVarSet.mem ptr single_var_set then
          let p_base = SingleExp.repl_context_var single_var_map (SingleVar ptr) in
          match sub_sol_func p_base with
          | Some (p_base, _) -> 
            begin match SingleExp.find_base_adv sub_sol_list_func p_base (MemType.get_ptr_set parent_mem) with
            | Some p_ptr ->
              let find_result =
                List.find_map (
                  fun (x, part_mem) ->
                    if x = p_ptr then Some (set_part_mem smt_ctx var_map_set part_mem read_hint write_mem)
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
            end
          | None -> p_mem, (Unknown (SingleTop, SingleTop)) :: constraint_list, acc_useful_vars
        else
          p_mem, (Unknown (SingleTop, SingleTop)) :: constraint_list, acc_useful_vars
      else
        func_interface_error (Printf.sprintf "set_mem_type: read_hint and write_mem ptr does not match %d %d\n" ptr ptr2)
    in
    List.fold_left2 helper (parent_mem, [], SingleExp.SingleVarSet.empty) read_hint write_mem

  let get_reg_taint_constraint
      (var_map: Entry.local_var_map_t)
      (parent_reg: RegType.t) (child_reg: RegType.t) : Constraint.t list =
    List.map2 (
      fun (p_type: entry_t) (c_type: entry_t) ->
        Entry.get_sub_taint_constraint p_type (Entry.repl_context_var var_map c_type)
    ) parent_reg child_reg |> List.flatten

  let get_mem_taint_constraint
      (var_map: Entry.local_var_map_t)
      (parent_mem: MemType.t) (child_mem: MemType.t) : Constraint.t list =
    MemType.fold_left2 (
      fun (acc: Constraint.t list) (p_entry: entry_t) (c_entry: entry_t) ->
        (Entry.get_eq_taint_constraint p_entry (Entry.repl_context_var var_map c_entry)) @ acc
    ) [] parent_mem child_mem

  let get_slot_map_info
      (read_hint: (IsaBasic.imm_var_id * (read_hint_t list)) list)
      (child_mem: MemType.t) : CallAnno.slot_info MemType.mem_content option =
    let find_unresolved =
      List.find_map (
        fun (_, part_hint) ->
          List.find_map (
            fun (hint: read_hint_t) ->
              match hint with
              | Mapped _ -> None
              | _ -> Some true
          ) part_hint
      ) read_hint
    in
    match find_unresolved with
    | Some _ -> None
    | None ->
      Some (List.map2 (
        fun (ptr1, part_hint) (ptr2, part_mem) ->
          if ptr1 = ptr2 then
            ptr1,
            List.map2 (
              fun (hint: read_hint_t) (entry: MemOffset.t * MemRange.t * entry_t) ->
                match hint with
                | Mapped (p_ptr, p_off, p_is_full, _, _) ->
                  let c_off, c_range, _ = entry in
                  c_off, c_range, (p_ptr, p_off, p_is_full)
                | _ -> func_interface_error "get_slot_map_info read hint is not fully resolved"
            ) part_hint part_mem
          else 
            func_interface_error "get_slot_map_info ptr does not match"
      ) read_hint child_mem)

  let func_call_helper
      (smt_ctx: SmtEmitter.t)
      (check_context: bool)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (global_var_set: SingleExp.SingleVarSet.t)
      (simp_local_var: SingleExp.t -> SingleExp.t)
      (* (local_var_map: SingleExp.local_var_map_t) *)
      (child_reg: RegType.t) (child_mem: MemType.t)
      (child_context: SingleContext.t list)
      (child_out_reg: RegType.t) (child_out_mem: MemType.t)
      (parent_reg: RegType.t) (parent_mem: MemType.t) :
      RegType.t * MemType.t * 
      (Constraint.t list) * SingleExp.SingleVarSet.t * 
      CallAnno.slot_info MemType.mem_content option *
      Entry.local_var_map_t =
    let single_var_map, var_map = add_reg_var_map simp_local_var child_reg parent_reg in
    let single_var_set = 
      SingleExp.SingleVarSet.union 
        global_var_set
        (SingleExp.SingleVarSet.of_list (List.map (fun (x, _) -> x) single_var_map)) 
    in
    let single_var_map = SingleExp.add_local_global_var single_var_map global_var_set in
    let var_map = Entry.add_local_global_var var_map global_var_set in
    let ((single_var_map, single_var_set, var_map), read_useful_vars), mem_read_hint =
      add_mem_var_map smt_ctx sub_sol_func sub_sol_list_func simp_local_var (single_var_map, single_var_set, var_map) child_mem parent_mem
    in
    (* Printf.printf "!!! set_reg_type\n"; *)
    RegType.pp_reg_type 0 child_out_reg;
    Entry.pp_local_var 0 var_map;
    let reg_type = set_reg_type var_map child_out_reg in
    (* Printf.printf "!!! set_mem_type\n"; *)
    let mem_type, constraint_list, write_useful_vars = 
      set_mem_type smt_ctx sub_sol_func sub_sol_list_func (single_var_map, single_var_set, var_map) parent_mem mem_read_hint child_out_mem in

    let constraint_list = 
      get_reg_taint_constraint var_map parent_reg child_reg @
      (* get_mem_taint_constraint var_map parent_mem child_mem @ *)
      constraint_list
    in

    (* How to handle taint in func call:
       1. Build taint var map (add_reg_var_map, add_mem_var_map)
       2. Add constraints:
          1. Reg: p_taint => cin_taint (get_reg_taint_constraint)
          2. Mem: p_taint = cin_taint = cout_taint
              (Child function ensures cin_taint = cout_taint
              and set_mem_type ensures that p_taint = cout_taint)
      *)

    (* TODO: Check context!!! *)

    let check_context_helper () : Constraint.t list =
      (* TODO: Fix this!!! *)
      let p_context, unknown_context =
        List.partition_map (
          fun (x: SingleContext.t) ->
            if SingleContext.is_val (SingleExp.is_val single_var_set) x then
              let orig_cond = SingleContext.repl (fun x -> SingleExp.repl_context_var single_var_map x) x in
              match SingleContext.try_sub_sol sub_sol_func orig_cond with
              | Some simp_cond -> Left (orig_cond, simp_cond)
              | None -> Right None (* Sol for single var in this cond is not resolved *)
            else
              (* Context map to parent var is not resolved *)
              Right None
        ) child_context
      in
      if List.is_empty unknown_context then begin
        (* We do not do quick check to avoid missing overflow constraints, but this might be slow!!! *)
        SmtEmitter.push smt_ctx;
        let check_result = SingleContext.sub_check_or_filter false smt_ctx p_context in
        SmtEmitter.pop smt_ctx 1;
        match check_result with
        (* Note simp_context is simplified with solution, 
          but to be used for check_or_assert, we need to filter out cond that is_val. *)
        | Left simp_context ->
          if List.is_empty simp_context then []
          else [Constraint.CalleeContext simp_context]
        | Right unsat_cond ->
          let unsat_cond_expr = SingleContext.to_smt_expr smt_ctx unsat_cond in
          Printf.printf "p_context\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t (List.map fst p_context)));
          Printf.printf "%s\n%s\n" (Sexplib.Sexp.to_string_hum (SingleContext.sexp_of_t unsat_cond)) (Z3.Expr.to_string unsat_cond_expr);
          (* (
            match SmtEmitter.check_compliance smt_ctx [unsat_cond_expr] with
            | SatYes -> Printf.printf "Should be sat\n"
            | SatNo -> Printf.printf "Not sat\n"
            | SatUnknown -> Printf.printf "Should be unknown\n"
          ); *)
          SmtEmitter.pp_smt_ctx 0 smt_ctx;
          func_interface_error "func_call_helper: get unstat constraint"
        end else
        [Constraint.CalleeUnknownContext]
    in

    let constraint_list = if check_context then (check_context_helper ()) @ constraint_list else constraint_list in

    reg_type, mem_type, 
    constraint_list, SingleExp.SingleVarSet.union read_useful_vars write_useful_vars,
    get_slot_map_info mem_read_hint child_mem,
    var_map

  let func_call
      (smt_ctx: SmtEmitter.t)
      (check_context: bool)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (sub_sol_list_func: SingleExp.t -> (MemOffset.t list) option)
      (func_interface: t)
      (global_var_set: SingleExp.SingleVarSet.t)
      (local_var_map: Entry.local_var_map_t)
      (reg_type: RegType.t) (mem_type: MemType.t) :
      RegType.t * MemType.t * 
      (Constraint.t list) * SingleExp.SingleVarSet.t *
      CallAnno.slot_info MemType.mem_content option *
      Entry.local_var_map_t =
    (* match List.find_opt (fun (x: t) -> x.func_name = func_name) func_inferface_list with
    | None -> func_interface_error (Printf.sprintf "Func %s interface not resolved yet" func_name)
    | Some func_interface -> *)
      (* TODO: Check context!!! *)
    func_call_helper smt_ctx check_context sub_sol_func sub_sol_list_func global_var_set (SingleExp.repl_local_var (Entry.get_single_var_map local_var_map)) 
      func_interface.in_reg func_interface.in_mem
      func_interface.context
      func_interface.out_reg func_interface.out_mem
      reg_type mem_type

  let fi_list_to_file (filename: string) (fi_list: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t fi_list)

  let fi_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp

  let find_fi (fi_list: t list) (l: IsaBasic.label) : t option =
    List.find_opt (fun fi -> fi.func_name = l) fi_list

end

module FuncInterfaceConverter = struct
  module SingleFuncInterface = FuncInterface (SingleEntryType)
  module TaintFuncInterface = FuncInterface (TaintEntryType)

  let get_one_single_func_interface
      (interface: TaintFuncInterface.t) : SingleFuncInterface.t =
    let mem_map = TaintFuncInterface.MemType.map in
    {
      func_name = interface.func_name;
      in_reg = List.map TaintEntryType.get_single_exp interface.in_reg;
      in_mem = mem_map TaintEntryType.get_single_exp interface.in_mem;
      context = interface.context;
      out_reg = List.map TaintEntryType.get_single_exp interface.out_reg;
      out_mem = mem_map TaintEntryType.get_single_exp interface.out_mem;
      base_info = interface.base_info;
    }

  let get_single_func_interface
      (interface_list: TaintFuncInterface.t list) : SingleFuncInterface.t list =
    List.map get_one_single_func_interface interface_list

end

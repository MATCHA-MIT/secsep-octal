open Pretty_print
open Entry_type
open Cond_type_new
open Single_context
open Reg_type_new
open Mem_offset_new
open Mem_type_new
open Smt_emitter
open Isa
open Single_exp
open Range_exp
open Constraint
open Func_interface
open Full_mem_anno
open Call_anno_type
open Sexplib.Std

module ArchType (Entry: EntryType) = struct
  exception ArchTypeError of string
  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  type entry_t = Entry.t
  [@@deriving sexp]

  module MemAnno = FullMemAnno

  module Isa = Isa (MemAnno)
  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  module CondType = CondType (Entry)

  module FuncInterface = FuncInterface (Entry)

  type prop_mode_t =
    | TypeInferDep
    | TypeInferTaint
    | TypeInferInit
    | TypeCheck
  [@@deriving sexp]

  type t = {
    label: Isa.label;
    pc: int;
    reg_type: RegType.t;
    mem_type: MemType.t;
    context: SingleContext.t list;
    flag: entry_t * entry_t;
    branch_hist: (CondType.t * int) list;
    full_not_taken_hist: (CondType.t * int) list;
    constraint_list: (Constraint.t * int) list;
    (* smt_ctx: SmtEmitter.t; *)
    local_var_map: Entry.local_var_map_t;
    useful_var: SingleExp.SingleVarSet.t;
    global_var: SingleExp.SingleVarSet.t;
    prop_mode: prop_mode_t;
    (* Maybe add constraint set here!!! *)
  }
  [@@deriving sexp]

  type block_subtype_t = t * (t list)
  [@@deriving sexp]

  let prop_mode_to_ocaml_string (prop_mode: prop_mode_t) : string =
    match prop_mode with
    | TypeInferDep -> "TypeInferDep"
    | TypeInferTaint -> "TypeInferTaint"
    | TypeInferInit -> "TypeInferInit"
    | TypeCheck -> "TypeCheck"

  let pp_arch_type (lvl: int) (curr_type: t) =
    PP.print_lvl lvl "<ArchType %s %d>\n" curr_type.label curr_type.pc;
    RegType.pp_reg_type lvl curr_type.reg_type;
    MemType.pp_mem_type lvl curr_type.mem_type

  let pp_ocaml_arch_type (lvl: int) (buf: Buffer.t) (curr_type: t) =
    PP.bprint_lvl lvl buf "{\n";
    PP.bprint_lvl (lvl + 1) buf "label = \"%s\";\n" curr_type.label;
    PP.bprint_lvl (lvl + 1) buf "pc = %d;\n" curr_type.pc;
    PP.bprint_lvl (lvl + 1) buf "reg_type = \n"; RegType.pp_ocaml_reg_type (lvl + 2) buf curr_type.reg_type; PP.bprint_lvl (lvl + 1) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "mem_type = \n"; MemType.pp_ocaml_mem_type (lvl + 2) buf curr_type.mem_type; PP.bprint_lvl (lvl + 1) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "flag = (%s, %s);\n" (Entry.to_ocaml_string (fst curr_type.flag)) (Entry.to_ocaml_string (snd curr_type.flag));
    PP.bprint_lvl (lvl + 1) buf "branch_hist = [];\n"; (* We do not need branch hist, so I omit it *)
    PP.bprint_lvl (lvl + 1) buf "full_not_taken_hist = [];\n";
    PP.bprint_lvl (lvl + 1) buf "constraint_list = [];\n";
    PP.bprint_lvl (lvl + 1) buf "local_var_map = %s;\n" Entry.empty_var_map_to_ocaml_string;
    PP.bprint_lvl (lvl + 1) buf "useful_var = %s;\n" (SingleExp.var_set_to_ocaml_string curr_type.useful_var);
    PP.bprint_lvl (lvl + 1) buf "global_var = %s;\n" (SingleExp.var_set_to_ocaml_string curr_type.global_var);
    PP.bprint_lvl (lvl + 1) buf "prop_mode = %s;\n" (prop_mode_to_ocaml_string curr_type.prop_mode);
    PP.bprint_lvl lvl buf "}\n"

  let pp_arch_type_list (lvl: int) (type_list: t list) =
    List.iter (pp_arch_type lvl) type_list

  let pp_ocaml_arch_type_list (lvl: int) (buf: Buffer.t) (type_list: t list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun arch_type ->
        pp_ocaml_arch_type (lvl + 1) buf arch_type;
        PP.bprint_lvl (lvl + 1) buf ";\n"
    ) type_list;
    PP.bprint_lvl lvl buf "]\n"

  let pp_arch_type_useful_var (lvl: int) (curr_type: t) =
    let var_s = String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list curr_type.useful_var)) in
    PP.print_lvl lvl "%s\t%s\n" curr_type.label var_s

  let pp_block_subtype_useful_var_list (lvl: int) (block_subtype_list: block_subtype_t list) =
    List.iter (
      fun (x, _) -> pp_arch_type_useful_var lvl x
    ) block_subtype_list

  let init_func_input_from_layout
      (label: Isa.label) (start_var: entry_t) 
      (start_pc: int) (mem_type: MemType.t)
      (global_var: SingleExp.SingleVarSet.t)
      (prop_mode: prop_mode_t)
      : t =
    let _, reg_type = RegType.init_reg_type start_var in
    (* Printf.printf "BB %s vars for reg: [%s, %s)\n" label (Entry.to_string start_var) (Entry.to_string idx0); *)
    {
      label = label;
      pc = start_pc;
      reg_type = reg_type;
      mem_type = mem_type;
      context = [];
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      branch_hist = [];
      full_not_taken_hist = [];
      constraint_list = [];
      local_var_map = Entry.get_empty_var_map;
      useful_var = SingleExp.SingleVarSet.empty;
      global_var = global_var;
      prop_mode = prop_mode;
    }

  let init_from_layout 
      (label: Isa.label)
      (start_var: entry_t) (start_pc: int)
      (mem_layout: 'a MemType.mem_content)
      (global_var: SingleExp.SingleVarSet.t)
      (prop_mode: prop_mode_t) : entry_t * t =
    let idx0, reg_type = RegType.init_reg_type start_var in
    (* Printf.printf "BB %s vars for reg: [%s, %s)\n" label (Entry.to_string start_var) (Entry.to_string idx0); *)
    let idx1, mem_type = MemType.init_mem_type_from_layout idx0 mem_layout in
    (* Printf.printf "BB %s vars for mem: [%s, %s)\n" label (Entry.to_string idx0) (Entry.to_string idx1); *)
    idx1, {
      label = label;
      pc = start_pc;
      reg_type = reg_type;
      mem_type = mem_type;
      context = [];
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      branch_hist = [];
      full_not_taken_hist = [];
      constraint_list = [];
      local_var_map = Entry.get_empty_var_map;
      useful_var = SingleExp.SingleVarSet.empty;
      global_var = global_var;
      prop_mode = prop_mode;
    }

  let clean_up (arch_type: t) : t =
    { arch_type with
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      branch_hist = [];
      full_not_taken_hist = [];
      constraint_list = [];
      local_var_map = Entry.get_empty_var_map
      (* useful_var = SingleExp.SingleVarSet.empty *)
    }

  let init_block_subtype_from_layout
      (func: Isa.func) (start_var: entry_t) (start_pc: int)
      (mem_layout: 'a MemType.mem_content)
      (global_var: SingleExp.SingleVarSet.t)
      (prop_mode: prop_mode_t) : entry_t * (block_subtype_t list) =
    let helper (acc_var: entry_t) (bb: Isa.basic_block) : entry_t * (t * (t list)) =
      let label = bb.label in
      let acc_var, block_type = (init_from_layout label acc_var start_pc mem_layout global_var prop_mode) in
      acc_var, (block_type, [])
    in
    List.fold_left_map helper start_var func.body

  let init_block_subtype_list_from_block_type_list
      (func_type: t list) : block_subtype_t list =
    List.map (fun x -> x, []) func_type

  let update_one_with_another_helper (orig: t) (update: t) : t =
    { orig with
      local_var_map = update.local_var_map;
      useful_var = update.useful_var;
      full_not_taken_hist = update.full_not_taken_hist;
      constraint_list = update.constraint_list
    }

  let update_with_block_subtype
      (block_subtype: block_subtype_t list) (func_type: t list) : t list =
    List.map2 (
      fun (x: block_subtype_t) (y: t) ->
        let x, _ = x in
        if x.label = y.label then 
          update_one_with_another_helper y x
          (* { y with 
            useful_var = x.useful_var;
            full_not_taken_hist = x.full_not_taken_hist;
            constraint_list = x.constraint_list;
          } *)
        else arch_type_error "update_useful_var label does not match"
    ) block_subtype func_type

  let get_reg_type (curr_type: t) (r: Isa.register) : entry_t =
    RegType.get_reg_type curr_type.reg_type r

  let set_reg_type (curr_type: t) (r: Isa.register) (new_type: entry_t) : t =
    { curr_type with reg_type = (RegType.set_reg_type curr_type.reg_type r new_type) }

  let get_mem_op_type
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option) : entry_t =
    let base_r = if base = None then None else Some (get_reg_type curr_type (Option.get base)) in
    let index_r = if index = None then None else Some (get_reg_type curr_type (Option.get index)) in
    let scale_v = if scale = None then 1L else Isa.scale_val (Option.get scale) in
    Entry.get_mem_op_type disp base_r index_r scale_v

  let get_ld_op_type_no_slot
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) : *)
      (anno_opt: MemAnno.slot_t option) :
      entry_t * (Constraint.t list) * SingleExp.SingleVarSet.t * (MemAnno.slot_t option) =
    (* TODO: Add constriant: addr_type is untainted *)
    (* TODO: ld/st op taint is not handled!!! *)
    (* let addr_type = get_mem_op_type curr_type disp base index scale in *)
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let size_type = Entry.repl_local_var curr_type.local_var_map size_type in
    let addr_untaint_cons = (Entry.get_untaint_constraint addr_type) @ (Entry.get_untaint_constraint size_type) in
    let addr_exp = Entry.get_single_exp addr_type in
    let size_exp = Entry.get_single_exp size_type in
    if addr_exp = SingleTop || size_exp = SingleTop then begin
      (* Printf.printf "ld addr or size is top where addr = %s and size = %s\n" (SingleExp.to_string addr_exp) (SingleExp.to_string size_exp); *)
      Entry.get_top_type (), 
      Unknown (SingleTop, SingleTop) :: addr_untaint_cons, 
      SingleExp.SingleVarSet.empty,
      None
    end else
      let useful_vars = MemOffset.get_vars (addr_exp, size_exp) in
      let orig_addr_offset =
        addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, size_exp))
      in
      begin match sub_sol_func addr_exp, sub_sol_func size_exp with
      | Some (simp_l, simp_r), Some (_, simp_size) ->
        let simp_addr_offset = 
          simp_l,
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, simp_r, simp_size))
        in
        let try_get_slot_type =
          match anno_opt with
          | None -> None
          | Some slot_info -> Some (MemType.get_slot_mem_type smt_ctx true curr_type.mem_type simp_addr_offset slot_info)
        in
        begin match try_get_slot_type with
        | Some ((off_w, off_r, e_t), true) ->
          e_t, 
          Subset (orig_addr_offset, off_r, off_w) :: addr_untaint_cons, 
          useful_vars,
          anno_opt
          (* NOTE: Possible problem: If previous is_full is false while it should be true, the current code cannot correct this over-conservative annotation!!! *)
          (* Some (ptr, off_w, is_full) *)
        | _ -> (* Annotation not pass check or no annotation *)
        (* TODO: Still need to check with SMT solver after resolving range with opt_offset!!! *)
          begin match MemType.get_mem_type smt_ctx curr_type.mem_type orig_addr_offset simp_addr_offset with
          | Some (is_full, ptr, (off_w, off_r, e_t)) -> 
            e_t, 
            Subset (orig_addr_offset, off_r, off_w) :: addr_untaint_cons, 
            useful_vars,
            Some (ptr, off_w, is_full)
          | None -> 
            Printf.printf "get_ld_op_type unknown addr orig %s simp %s\n" (MemOffset.to_string orig_addr_offset) (MemOffset.to_string simp_addr_offset);
            (* Use simp_addr_offset since we do not need to distinguish between eq and subset for resolving unknown address *)
            (* Printf.printf "Curr_type:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t curr_type));
            SmtEmitter.pp_smt_ctx 0 smt_ctx; *)
            Entry.get_top_type (), 
            Unknown simp_addr_offset :: addr_untaint_cons, 
            useful_vars,
            None
          end
        end
      | _ -> 
        Printf.printf "get_ld_op_type cannot simplify for addr_exp %s\n" (SingleExp.to_string addr_exp);
        Entry.get_top_type (), 
        Unknown (SingleTop, SingleTop) :: addr_untaint_cons, 
        useful_vars,
        None
      end

  let get_ld_op_type_slot
      (smt_ctx: SmtEmitter.t)
      (check_addr: bool)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) *)
      (anno_opt: MemAnno.slot_t option) :
      entry_t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    match anno_opt with
    | None -> arch_type_error "get_ld_op_type_slot slot annotation is None"
    | Some slot_info ->
      (* let addr_type = get_mem_op_type curr_type disp base index scale in *)
      let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
      let size_type = Entry.repl_local_var curr_type.local_var_map size_type in
      let addr_untaint_cons = (Entry.get_untaint_constraint addr_type) @ (Entry.get_untaint_constraint size_type) in
      let addr_exp = Entry.get_single_exp addr_type in
      let size_exp = Entry.get_single_exp size_type in
      let useful_vars = MemOffset.get_vars (addr_exp, size_exp) in
      let orig_addr_offset = 
        (addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, size_exp))) 
      in
      let (off, range, entry), pass_check = MemType.get_slot_mem_type smt_ctx check_addr curr_type.mem_type orig_addr_offset slot_info in
      if pass_check then
        entry, Subset (orig_addr_offset, range, off) :: addr_untaint_cons, useful_vars
      else
        arch_type_error (Printf.sprintf "get_ld_op_type_slot: Annotation %s does not match memory slot %s"
          (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_offset))

  let get_ld_op_type
      (smt_ctx: SmtEmitter.t)
      (prop_mode: prop_mode_t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) *)
      (anno_opt: MemAnno.slot_t option) :
      entry_t * (Constraint.t list) * SingleExp.SingleVarSet.t * (MemAnno.slot_t option) =
    match prop_mode with
    | TypeInferDep -> get_ld_op_type_no_slot smt_ctx sub_sol_func curr_type addr_type size_type anno_opt
    | TypeInferTaint | TypeInferInit -> 
      let e, cons, useful_var = get_ld_op_type_slot smt_ctx false curr_type addr_type size_type anno_opt in
      e, cons, useful_var, anno_opt
    | TypeCheck -> arch_type_error "get_ld_op_type TypeCheck not implemented"

  let set_st_op_type_no_slot
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64)  *)
      (anno_opt: MemAnno.slot_t option)
      (new_type: entry_t) :
      t * (Constraint.t list) * SingleExp.SingleVarSet.t * (MemAnno.slot_t option) =
    (* TODO: Add constriant: addr_type is untainted *)
    (* TODO: ld/st op taint is not handled!!! *)
    (* let addr_type = get_mem_op_type curr_type disp base index scale in *)
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let size_type = Entry.repl_local_var curr_type.local_var_map size_type in
    let addr_untaint_cons = (Entry.get_untaint_constraint addr_type) @ (Entry.get_untaint_constraint size_type) in
    let addr_exp = Entry.get_single_exp addr_type in
    let size_exp = Entry.get_single_exp size_type in
    if addr_exp = SingleTop || size_exp = SingleTop then begin
      (* Printf.printf "St op %s\n" (Isa.string_of_operand (StOp (disp, base, index, scale, size, (None, None))));
      RegType.pp_reg_type 0 curr_type.reg_type;
      Printf.printf "Top addr_exp for orign addr_type %s pc %d\n" (Entry.to_string addr_type) curr_type.pc; *)
      (* Printf.printf "st addr or size is top where addr = %s and size = %s\n" (SingleExp.to_string addr_exp) (SingleExp.to_string size_exp); *)
      curr_type, 
      Unknown (SingleTop, SingleTop) :: addr_untaint_cons, 
      SingleExp.SingleVarSet.empty,
      None
    end else
      let useful_vars = MemOffset.get_vars (addr_exp, size_exp) in
      let orig_addr_offset =
        addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, size_exp))
      in
      begin match sub_sol_func addr_exp, sub_sol_func size_exp with
      | Some (simp_l, simp_r), Some (_, simp_size) ->
        let simp_addr_offset = 
          simp_l,
          SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, simp_r, simp_size))
        in
        let try_set_slot_type =
          match anno_opt with
          | None -> None
          | Some slot_info -> Some (MemType.set_slot_mem_type smt_ctx true false curr_type.mem_type simp_addr_offset slot_info new_type)
        in
        begin match try_set_slot_type with
        | Some (new_mem, write_constraints, true) ->
          { curr_type with mem_type = new_mem }, 
          write_constraints @ addr_untaint_cons, 
          useful_vars,
          anno_opt
        | _ -> (* Annotation not pass check or no annotation *)
        (* TODO: Still need to check with SMT solver after resolving range with opt_offset!!! *)
          begin match MemType.set_mem_type smt_ctx true curr_type.mem_type orig_addr_offset simp_addr_offset new_type with
          | Some (new_mem, write_constraints, slot_anno) -> 
            { curr_type with mem_type = new_mem }, 
            write_constraints @ addr_untaint_cons, 
            useful_vars,
            Some slot_anno
          | None -> 
            (* Printf.printf "set_st_op_type unknown addr orig %s simp %s\n" (MemOffset.to_string orig_addr_offset) (MemOffset.to_string simp_addr_offset); *)
            (* Use simp_addr_offset since we do not need to distinguish between eq and subset for resolving unknown address *)
            curr_type, 
            Unknown simp_addr_offset :: addr_untaint_cons, 
            useful_vars,
            None
          end
        end
      | _ -> 
        (* Printf.printf "set_st_op_type cannot simplify for addr_exp %s\n" (SingleExp.to_string addr_exp); *)
        curr_type, 
        Unknown (SingleTop, SingleTop) :: addr_untaint_cons, 
        useful_vars,
        None
      end
    
  let set_st_op_type_slot
      (smt_ctx: SmtEmitter.t)
      (check_addr: bool)
      (update_init_range: bool)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64)  *)
      (anno_opt: MemAnno.slot_t option)
      (new_type: entry_t) :
      t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    match anno_opt with
    | None -> arch_type_error "set_st_op_type_slot slot annotation is None"
    | Some slot_info ->
      (* let addr_type = get_mem_op_type curr_type disp base index scale in *)
      let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
      let size_type = Entry.repl_local_var curr_type.local_var_map size_type in
      let addr_untaint_cons = (Entry.get_untaint_constraint addr_type) @ (Entry.get_untaint_constraint size_type) in
      let addr_exp = Entry.get_single_exp addr_type in
      let size_exp = Entry.get_single_exp size_type in
      let useful_vars = MemOffset.get_vars (addr_exp, size_exp) in
      let orig_addr_offset = 
        (addr_exp, 
        SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, size_exp))) 
      in
      let new_mem, write_constraints, pass_check = 
        MemType.set_slot_mem_type smt_ctx check_addr update_init_range curr_type.mem_type orig_addr_offset slot_info new_type
      in
      if pass_check then
        { curr_type with mem_type = new_mem }, 
        write_constraints @ addr_untaint_cons, 
        useful_vars
      else
        arch_type_error (Printf.sprintf "set_st_op_type_slot: Annotation %s does not match memory slot %s"
          (MemAnno.slot_to_string (Some slot_info)) (MemOffset.to_string orig_addr_offset))

  let set_st_op_type
      (smt_ctx: SmtEmitter.t)
      (prop_mode: prop_mode_t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (addr_type: entry_t) (size_type: entry_t)
      (* (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) *)
      (anno_opt: MemAnno.slot_t option)
      (new_type: entry_t) :
      t * (Constraint.t list) * SingleExp.SingleVarSet.t * (MemAnno.slot_t option) =
    match prop_mode with
    | TypeInferDep -> set_st_op_type_no_slot smt_ctx sub_sol_func curr_type addr_type size_type anno_opt new_type
    | TypeInferTaint | TypeInferInit ->
      let new_mem, cons, useful_var = 
        set_st_op_type_slot smt_ctx false (prop_mode = TypeInferInit) curr_type addr_type size_type anno_opt new_type
      in
      new_mem, cons, useful_var, anno_opt
    | TypeCheck -> arch_type_error "set_st_op_type TypeCheck not implemented"

  let get_src_op_type
      (smt_ctx: SmtEmitter.t)
      (prop_mode: prop_mode_t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (src: Isa.operand) :
      entry_t * t * (Constraint.t list) * Isa.operand =
    match src with
    | ImmOp imm -> Entry.get_const_type imm, curr_type, [], src
    | RegOp r -> get_reg_type curr_type r, curr_type, [], src
    | MemOp (disp, base, index, scale) ->
      get_mem_op_type curr_type disp base index scale, curr_type, [], src
    | LdOp (disp, base, index, scale, size, (slot_anno, taint_anno) (* TODO: check offset and generate constraints *)) ->
      let addr_type = get_mem_op_type curr_type  disp base index scale in
      let size_type = Entry.get_const_type (ImmNum size) in
      let src_type, src_constraint, src_useful, slot_anno =
        get_ld_op_type smt_ctx prop_mode sub_sol_func curr_type addr_type size_type slot_anno
      in
      let ld_op_constraint = Entry.update_ld_taint_constraint src_type taint_anno in
      (
        src_type,
        { curr_type with useful_var = SingleExp.SingleVarSet.union curr_type.useful_var src_useful },
        ld_op_constraint @ src_constraint,
        LdOp (disp, base, index, scale, size, (slot_anno, taint_anno))
      )
    | StOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a label op")
  
  let set_dest_op_type
      (smt_ctx: SmtEmitter.t)
      (prop_mode: prop_mode_t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (dest: Isa.operand)
      (new_type: entry_t) :
      t * (Constraint.t list) * Isa.operand =
    match dest with
    | RegOp r -> set_reg_type curr_type r new_type, [], dest
    | StOp (disp, base, index, scale, size, (slot_anno, taint_anno)) ->
      let addr_type = get_mem_op_type curr_type  disp base index scale in
      let size_type = Entry.get_const_type (ImmNum size) in
      let new_type, st_op_constraint = Entry.update_st_taint_constraint new_type taint_anno in
      let next_type, dest_constraint, dest_useful, slot_anno =
        set_st_op_type smt_ctx prop_mode sub_sol_func curr_type addr_type size_type slot_anno new_type
      in
      { next_type with useful_var = SingleExp.SingleVarSet.union next_type.useful_var dest_useful }, 
      st_op_constraint @ dest_constraint,
      StOp (disp, base, index, scale, size, (slot_anno, taint_anno))
    | _ -> arch_type_error ("set_dest_op_type: dest is not reg or st op")

  let get_orig_simp_off
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option) (pc: int)
      (left_type: entry_t) (right_type: entry_t) : 
      MemOffset.t * (MemOffset.t option) * SingleExp.SingleVarSet.t =
    let sub_helper (e: SingleExp.t) : MemOffset.t option =
      match sub_sol_func (e, pc) with
        | Some r -> (* RangeExp.to_mem_offset2 r *)
          begin match RangeExp.to_mem_offset2 r with
          | Some off -> Some off
          | None -> Some (e, e)
          end
        | None -> None
    in
    let orig_l = Entry.get_single_exp left_type in
    let orig_r = Entry.get_single_exp right_type in
    if orig_l = SingleTop || orig_r = SingleTop then
      (orig_l, orig_r), None, SingleExp.SingleVarSet.empty
    else
      (orig_l, orig_r),
      (match sub_helper orig_l, sub_helper orig_r with
      | Some (simp_l, _), Some (_, simp_r) -> Some (simp_l, simp_r)
      | _ -> None),
      MemOffset.get_vars (orig_l, orig_r)

  let add_offset_rsp 
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t) (offset: int64) : t =
    let prop_mode = curr_type.prop_mode in
    let rsp_type, curr_type, _, _ = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.RegOp Isa.RSP) in
    let new_rsp_type = 
      Entry.repl_local_var curr_type.local_var_map 
        (Entry.exe_bop_inst (prop_mode = TypeCheck) Isa.Add rsp_type (Entry.get_const_type (Isa.ImmNum offset))) 
    in
    let curr_type, _, _ = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.RegOp Isa.RSP) new_rsp_type in
    curr_type

  let add_constraints
      (curr_type: t) (new_constraints: Constraint.t list) : t =
    {
      curr_type with constraint_list = (List.map (fun x -> x, curr_type.pc) new_constraints) @ curr_type.constraint_list
    }

  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (curr_type: t)
      (inst: Isa.instruction) :
      t * Isa.instruction =
    (* We do not update pc in this function! Should update outside!!! *)
    let curr_type = { curr_type with flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ()) } in
    let prop_mode = curr_type.prop_mode in
    match inst with
    | BInst (bop, dest, src0, src1) ->
      let src0_type, curr_type, src0_constraint, src0 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint, src1 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src1 in
      let dest_type = Entry.exe_bop_inst (prop_mode = TypeCheck) bop src0_type src1_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint, dest = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type dest dest_type in
      let ldst_bind_constraint = List.concat_map (Isa.get_ld_st_related_taint_constraint dest) [src0; src1] in
      let next_type = add_constraints next_type (src0_constraint @ src1_constraint @ dest_constraint @ ldst_bind_constraint) in
      { next_type with local_var_map = new_local_var },
      BInst (bop, dest, src0, src1)
    | UInst (uop, dest, src) ->
      let src_type, curr_type, src_constraint, src = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src in
      let dest_type = 
        begin match uop with
        (* | MovS -> Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type *)
        | Mov -> Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type
        | MovZ -> Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type
        | _ -> Entry.exe_uop_inst uop src_type
        end 
      in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint, dest = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type dest dest_type in
      let ldst_bind_constraint = Isa.get_ld_st_related_taint_constraint dest src in
      let next_type = add_constraints next_type (src_constraint @ dest_constraint @ ldst_bind_constraint) in
      { next_type with local_var_map = new_local_var },
      UInst (uop, dest, src)
    | TInst (top, dest, src_list) ->
      let curr_type, src_op_type_list =
        List.fold_left_map (
          fun (curr_type: t) (src: Isa.operand) ->
            let src_type, curr_type, src_constraint, src = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src in
            add_constraints curr_type src_constraint,
            (src_type, src)
        ) curr_type src_list
      in
      let src_type_list, src_list = List.split src_op_type_list in
      let dest_type = Entry.exe_top_inst top src_type_list in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint, dest = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type dest dest_type in
      let ldst_bind_constraint = List.concat_map (Isa.get_ld_st_related_taint_constraint dest) src_list in
      let next_type = add_constraints next_type (dest_constraint @ ldst_bind_constraint) in
      { next_type with local_var_map = new_local_var },
      TInst (top, dest, src_list)
    | RepMovs (size, (dest_slot_anno, dest_taint_anno), (src_slot_anno, src_taint_anno)) ->
      let rcx_type = get_reg_type curr_type RCX in
      let src_addr_type = get_reg_type curr_type RSI in
      let dest_addr_type = get_reg_type curr_type RDI in
      let size_type = Entry.get_mem_op_type None None (Some rcx_type) size in
      let src_type, src_constraint, src_useful, src_slot_anno = 
        get_ld_op_type smt_ctx prop_mode sub_sol_func curr_type src_addr_type size_type src_slot_anno 
      in
      let ld_op_constraint = Entry.update_ld_taint_constraint src_type src_taint_anno in
      let new_type, st_op_constraint = Entry.update_st_taint_constraint src_type dest_taint_anno in
      let next_type, dest_constraint, dest_useful, dest_slot_anno =
        set_st_op_type smt_ctx prop_mode sub_sol_func curr_type dest_addr_type size_type dest_slot_anno new_type
      in
      let next_type = add_constraints next_type (src_constraint @ ld_op_constraint @ dest_constraint @ st_op_constraint) in
      { next_type with
        useful_var = next_type.useful_var |> SingleExp.SingleVarSet.union src_useful |> SingleExp.SingleVarSet.union dest_useful },
      RepMovs (size, (dest_slot_anno, dest_taint_anno), (src_slot_anno, src_taint_anno))
    | RepLods (size, (src_slot_anno, src_taint_anno)) ->
      let rcx_type = get_reg_type curr_type RCX in
      let src_addr_type = get_reg_type curr_type RSI in
      let size_type = Entry.get_mem_op_type None None (Some rcx_type) size in
      let src_type, src_constraint, src_useful, src_slot_anno = 
        get_ld_op_type smt_ctx prop_mode sub_sol_func curr_type src_addr_type size_type src_slot_anno 
      in
      let ld_op_constraint = Entry.update_ld_taint_constraint src_type src_taint_anno in
      let new_type = Entry.set_taint_with_other (Entry.get_top_untaint_type ()) src_type in
      let next_type = set_reg_type curr_type RAX new_type in
      let next_type = add_constraints next_type (src_constraint @ ld_op_constraint) in
      { next_type with
        useful_var = next_type.useful_var |> SingleExp.SingleVarSet.union src_useful },
      RepLods (size, (src_slot_anno, src_taint_anno))
    | RepStos (size, (dest_slot_anno, dest_taint_anno)) ->
      let rcx_type = get_reg_type curr_type RCX in
      let dest_addr_type = get_reg_type curr_type RDI in
      let size_type = Entry.get_mem_op_type None None (Some rcx_type) size in
      Printf.printf "RepStos rcx %s rdi %s size %s\n" (Entry.to_string rcx_type) (Entry.to_string dest_addr_type) (Entry.to_string size_type);
      let src_type = Entry.set_taint_with_other (Entry.get_top_untaint_type ()) (get_reg_type curr_type RAX) in
      let new_type, st_op_constraint = Entry.update_st_taint_constraint src_type dest_taint_anno in
      let next_type, dest_constraint, dest_useful, dest_slot_anno =
        set_st_op_type smt_ctx prop_mode sub_sol_func curr_type dest_addr_type size_type dest_slot_anno new_type
      in
      let next_type = add_constraints next_type (dest_constraint @ st_op_constraint) in
      { next_type with
        useful_var = next_type.useful_var |> SingleExp.SingleVarSet.union dest_useful },
      RepStos (size, (dest_slot_anno, dest_taint_anno))
    | Xchg (dest0, dest1, src0, src1) ->
      let src0_type, curr_type, src0_constraint, src0 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint, src1 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src1 in
      let next_type, dest0_constraint, dest0 = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type dest0 src1_type in
      let next_type, dest1_constraint, dest1 = set_dest_op_type smt_ctx prop_mode sub_sol_func next_type dest1 src0_type in
      let ldst_bind_constraint =
        (Isa.get_ld_st_related_taint_constraint dest0 src0) @ (Isa.get_ld_st_related_taint_constraint dest1 src1)
      in
      add_constraints next_type (src0_constraint @ src1_constraint @ dest0_constraint @ dest1_constraint @ ldst_bind_constraint),
      Xchg (dest0, dest1, src0, src1)
      (* next_type, 
      src0_constraint @ src1_constraint @ dest0_constraint @ dest1_constraint *)
    | Cmp (src0, src1) ->
      let src0_type, curr_type, src0_constraint, src0 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint, src1 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src1 in
      let src0_type = Entry.repl_local_var curr_type.local_var_map src0_type in
      let src1_type = Entry.repl_local_var curr_type.local_var_map src1_type in
      let useful_vars = 
        SingleExp.SingleVarSet.union 
          (SingleExp.get_vars (Entry.get_single_exp src0_type))
          (SingleExp.get_vars (Entry.get_single_exp src1_type))
      in
      let curr_type = add_constraints curr_type (src0_constraint @ src1_constraint) in
      { curr_type with flag = (src0_type, src1_type); 
        useful_var = SingleExp.SingleVarSet.union curr_type.useful_var useful_vars },
      Cmp (src0, src1)
    | Test (src0, src1) ->
      let src0_type, curr_type, src0_constraint, src0 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint, src1 = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src1 in
      let dest_type = Entry.repl_local_var curr_type.local_var_map (Entry.exe_bop_inst (prop_mode = TypeCheck) Isa.And src0_type src1_type) in
      let useful_vars = SingleExp.get_vars (Entry.get_single_exp dest_type) in
      let curr_type = add_constraints curr_type (src0_constraint @ src1_constraint) in
      { curr_type with flag = (dest_type, Entry.get_const_type (Isa.ImmNum 0L)); 
        useful_var = SingleExp.SingleVarSet.union curr_type.useful_var useful_vars },
      Test (src0, src1)
    | Push (src, mem_anno) ->
      let size = Isa.get_op_size src in
      let curr_type = add_offset_rsp smt_ctx sub_sol_func curr_type (Int64.neg size) in
      (* let rsp_type, curr_type, _, _ = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst (prop_mode = TypeCheck) Isa.Sub rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let curr_type, _, _ = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.RegOp Isa.RSP) new_rsp_type in *)
      let src_type, curr_type, src_constraint, _ = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type src in
      let next_type, dest_constraint, new_st_op = 
        set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.StOp (None, Some Isa.RSP, None, None, size, mem_anno)) src_type 
      in
      begin match new_st_op with
      | StOp (_, _, _, _, _, mem_anno) -> 
        add_constraints next_type (src_constraint @ dest_constraint),
        Push (src, mem_anno)
      | _ -> arch_type_error "Get different stop after push"
      end
      (* next_type, src_constraint @ dest_constraint *)
    | Pop (dst, mem_anno) ->
      let size = Isa.get_op_size dst in
      let src_type, curr_type, src_constraint, new_ld_op = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.LdOp (None, Some Isa.RSP, None, None, size, mem_anno)) in
      let next_type, dest_constraint, _ = set_dest_op_type smt_ctx prop_mode sub_sol_func curr_type dst src_type in
      let next_type = add_offset_rsp smt_ctx sub_sol_func next_type size in
      (* let rsp_type, curr_type, _, _ = get_src_op_type smt_ctx prop_mode sub_sol_func curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst (prop_mode = TypeCheck) Isa.Add rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let next_type, _, _ = set_dest_op_type smt_ctx prop_mode sub_sol_func next_type (Isa.RegOp Isa.RSP) new_rsp_type in *)
      begin match new_ld_op with
      | LdOp (_, _, _, _, _, mem_anno) ->
        add_constraints next_type (src_constraint @ dest_constraint),
        Pop (dst, mem_anno)
      | _ -> arch_type_error "Get different ldop after pop"
      end
      (* next_type, src_constraint @ dest_constraint *)
    | Nop | Syscall | Hlt -> curr_type, inst
    | _ -> arch_type_error (Printf.sprintf "inst %s not implemented" (Sexplib.Sexp.to_string (Isa.sexp_of_instruction inst)))

  let add_block_subtype
      (label: Isa.label)
      (curr_type: t) (block_subtype: block_subtype_t list) :
      block_subtype_t list =
    List.map (
      fun (x, x_sub) ->
        if x.label = label then x, curr_type :: x_sub
        else x, x_sub
    ) block_subtype

  let update_with_end_type
      (final_type: t) (block_subtype: block_subtype_t list) :
      block_subtype_t list =
      List.map (
        fun (x, x_sub) ->
          if x.label = final_type.label then begin
            let new_x = update_one_with_another_helper x final_type in
            (* Printf.printf "update_with_end_type %s %s %s\n" 
              new_x.label
              (String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list new_x.useful_var)))
              (String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list x.useful_var))); *)
            (* { x with 
              useful_var = final_type.useful_var;
              full_not_taken_hist = final_type.full_not_taken_hist;
              constraint_list = final_type.constraint_list;
            },  *)
            new_x, x_sub
          end
          else x, x_sub
      ) block_subtype

  let update_branch_hist_get_not_taken_cond
      (smt_ctx: SmtEmitter.t)
      (cond: Isa.branch_cond) (curr_type: t) :
      t * t * (SmtEmitter.exp_t list) =
    let taken_type_option = CondType.get_taken_type cond curr_type.flag in
    match taken_type_option with
    | None -> curr_type, curr_type, []
    | Some taken_type -> 
      let not_taken_type = CondType.not_cond_type taken_type in
      let not_taken_cond = 
        if CondType.has_top not_taken_type then [] 
        else [ CondType.to_smt_expr smt_ctx not_taken_type ]
      in
      { curr_type with branch_hist = (taken_type, curr_type.pc) :: curr_type.branch_hist}, 
      { curr_type with branch_hist = (not_taken_type, curr_type.pc) :: curr_type.branch_hist},
      not_taken_cond

  let type_prop_branch
      (smt_ctx: SmtEmitter.t)
      (curr_type: t)
      (inst: Isa.instruction)
      (block_subtype: block_subtype_t list) :
      t * (block_subtype_t list) =
    let _ = smt_ctx in
    match inst with
    | Jmp (label, _) ->
      (* 1. Update full_not_taken_hist *)
      let curr_type = { curr_type with full_not_taken_hist = curr_type.branch_hist } in
      (* 1. Add curr_type to target block's subtype *)
      let block_subtype = (add_block_subtype label curr_type block_subtype) in
      (* 2. This is the end of the current block, so update useful vars of this block in block_subtype *)
      let block_subtype = (update_with_end_type curr_type block_subtype) in (* Maybe need to update local var map too... *)
      curr_type, block_subtype
    | Jcond (cond, label, _) ->
      (* Add constraint: branch cond is untainted!!! *)
      let cond_l, cond_r = curr_type.flag in
      let cond_untaint_constraint = (Entry.get_untaint_constraint cond_l) @ (Entry.get_untaint_constraint cond_r) in
      let taken_type, not_taken_type, not_taken_cond = 
        update_branch_hist_get_not_taken_cond smt_ctx cond curr_type 
      in
      (* 1. Add curr_type + taken to target block's subtype *)
      let block_subtype = (add_block_subtype label taken_type block_subtype) in
      (* 2. Update ctx *)
      SmtEmitter.add_assertions smt_ctx not_taken_cond;
      (* 3. Update useful var and return curr_type + not taken *)
      let l_flag, r_flag = not_taken_type.flag in
      let useful_var = 
        SingleExp.SingleVarSet.union 
          (SingleExp.get_vars (Entry.get_single_exp l_flag))
          (SingleExp.get_vars (Entry.get_single_exp r_flag))
      in
      let not_taken_type = add_constraints not_taken_type cond_untaint_constraint in
      {not_taken_type with useful_var = SingleExp.SingleVarSet.union not_taken_type.useful_var useful_var}, block_subtype
    | _ -> arch_type_error (Printf.sprintf "type_prop_branch: %s not supported" (Isa.string_of_instruction inst))

  let type_prop_call
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: SingleExp.t -> MemOffset.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (target_func_name: Isa.label)
      (orig_call_anno: CallAnno.t) : t * CallAnno.t =
    Printf.printf "type_prop_call %s pc %d\n" target_func_name curr_type.pc;
    Entry.pp_local_var 0 curr_type.local_var_map;
    pp_arch_type 0 curr_type;
    match FuncInterface.find_fi func_interface_list target_func_name with
    | Some func_interface ->
      let check_callee_context =
        begin match curr_type.prop_mode with
        | TypeInferDep | TypeCheck -> true
        | _ -> false
        end
      in
      let curr_type = add_offset_rsp smt_ctx sub_sol_func curr_type (-8L) in
      let new_reg, new_mem, new_constraints, new_useful_vars, call_slot_info, var_map =
        FuncInterface.func_call smt_ctx check_callee_context sub_sol_func func_interface
          curr_type.global_var curr_type.local_var_map curr_type.reg_type curr_type.mem_type
      in
      let call_anno = (
        match curr_type.prop_mode with
        | TypeInferTaint ->
          CallAnno.get_call_anno 
            (List.map Entry.get_single_taint_exp curr_type.reg_type)
            call_slot_info
            (Entry.get_single_var_map var_map)
            (Entry.get_taint_var_map var_map)
            sub_sol_func
            func_interface.base_info
        | _ -> orig_call_anno
      ) in
      let new_constraints = List.map (fun x -> (x, curr_type.pc)) new_constraints in
      let curr_type =
        { curr_type with
          reg_type = new_reg;
          mem_type = new_mem;
          flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
          constraint_list = new_constraints @ curr_type.constraint_list;
          useful_var = SingleExp.SingleVarSet.union new_useful_vars curr_type.useful_var
        }
      in
      let curr_type = add_offset_rsp smt_ctx sub_sol_func curr_type 8L in
      curr_type,
      call_anno
    | _ -> arch_type_error (Printf.sprintf "[type_prop_call] Func %s interface not resolved yet" target_func_name)

  let type_prop_inst
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (inst: Isa.instruction)
      (block_subtype: block_subtype_t list) :
      (t * (block_subtype_t list)) * Isa.instruction =
    (* Update pc here!!! *)
    (* Printf.printf "Prop inst %d %s\n" curr_type.pc (Isa.string_of_instruction inst); *)
    (* let unknown_before = List.length (Constraint.get_unknown curr_type.constraint_list) in *)
    let sub_sol_func (e: SingleExp.t) : MemOffset.t option =
      (* Only return None if no solution is found. If cannot convert to mem offset, use [e, e]. *)
      match sub_sol_func (e, curr_type.pc) with
      | Some r -> (* RangeExp.to_mem_offset2 r *)
        begin match RangeExp.to_mem_offset2 r with
        | Some off -> Some off
        | None -> Some (e, e)
        end
      | None -> None
    in
    let (next_type, block_subtype), inst = 
      match inst with
      | Jmp _ | Jcond _ ->
        type_prop_branch smt_ctx curr_type inst block_subtype, inst
      | Call (target_func_name, orig_call_anno (* TODO: update the call annotation *)) ->
        let next_type, call_anno =
          type_prop_call smt_ctx sub_sol_func func_interface_list curr_type target_func_name orig_call_anno
        in
        (* let open Sexplib in
        Sexp.output_hum stdout (CallAnno.sexp_of_t call_anno); *)
        (next_type,
        (* let _ = func_interface_list in
        let _ = target_func_name in
        Printf.printf "Warning: haven't implemented so far!\n";
        curr_type, *)
        block_subtype),
        Call (target_func_name, call_anno)
      | _ ->
        let next_type, inst = type_prop_non_branch smt_ctx sub_sol_func curr_type inst in
        (next_type, block_subtype), inst
    in
    (* let unknown_after = List.length (Constraint.get_unknown next_type.constraint_list) in
    (
      if unknown_after - unknown_before = 0 then () 
      else Printf.printf "PC %d inst %s introduces unknown addr\n" curr_type.pc (Isa.string_of_instruction inst)
    ); *)
    (* Printf.printf "type_prop_inst %s useful_vars %s\n" (Isa.string_of_instruction inst) (String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list next_type.useful_var))); *)
    ({next_type with pc = next_type.pc + 1}, block_subtype), inst

  let type_prop_block
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (block: Isa.instruction list)
      (block_subtype: block_subtype_t list) :
      (t * (block_subtype_t list)) * (Isa.instruction list) =
    let (curr_type, block_subtype), block = 
      List.fold_left_map (
        fun (curr, b_sub) inst -> 
          type_prop_inst smt_ctx sub_sol_func func_interface_list curr inst b_sub
      ) (curr_type, block_subtype) block
    in
    if curr_type.label = Isa.ret_label then
      let curr_type = 
        { curr_type with 
        useful_var = 
          SingleExp.SingleVarSet.union
            (RegType.get_callee_useful_var curr_type.reg_type)
            (MemType.get_shared_useful_var_quick_cmp smt_ctx curr_type.mem_type) 
        } 
      in
      (curr_type, update_with_end_type curr_type block_subtype), block
    else (curr_type, block_subtype), block

  let get_branch_hist
      (block_subtype_list: block_subtype_t list)
      (branch_pc: int) : (CondType.t * int) list =
    let find_branch_hist = List.find_map (
      fun (_, sub_list) ->
        List.find_map (fun x -> if x.pc = branch_pc then Some x.branch_hist else None) sub_list
    ) block_subtype_list
    in
    match find_branch_hist with
    | None -> arch_type_error (Printf.sprintf "get_branch_hist cannot find branch_pc %d" branch_pc)
    | Some branch_hist -> branch_hist

  (* let get_branch_hist 
      (block_subtype_list: block_subtype_t list) 
      (target_pc: int) (branch_pc: int) : (CondType.t * int) list =
    match List.find_opt (fun (x, _) -> x.pc = target_pc) block_subtype_list with
    | None -> arch_type_error (Printf.sprintf "get_branch_hist cannot find target_pc %d for branch_pc %d" target_pc branch_pc)
    | Some (_, sub_list) ->
      begin match List.find_map (fun x -> if x.pc = branch_pc then Some x.branch_hist else None) sub_list with
      | None -> arch_type_error (Printf.sprintf "get_branch_hist cannot find branch_pc %d with target_pc %d" branch_pc target_pc)
      | Some branch_hist -> branch_hist
      end *)

  let get_pc_cond_from_branch_hist
      (branch_hist: (CondType.t * int) list) (pc: int) : CondType.t option =
    List.find_map (fun (cond, cond_pc) -> if pc = cond_pc then Some cond else None) branch_hist

  let get_branch_cond
      (block_subtype_list: block_subtype_t list) 
      (branch_pc: int) : CondType.t option =
    let branch_hist = get_branch_hist block_subtype_list branch_pc in
    get_pc_cond_from_branch_hist branch_hist branch_pc

  let get_local_var_set (a_type: t) : SingleExp.SingleVarSet.t =
    let helper (acc: SingleExp.SingleVarSet.t) (x: entry_t) =
      SingleExp.SingleVarSet.union (SingleExp.get_vars (Entry.get_single_exp x)) acc
    in
    let reg_var_set = List.fold_left helper SingleExp.SingleVarSet.empty a_type.reg_type in
    let mem_var_set = MemType.fold_left helper SingleExp.SingleVarSet.empty a_type.mem_type in
    SingleExp.SingleVarSet.union reg_var_set mem_var_set

  let update_reg_mem_type (update_func: entry_t -> entry_t) (a_type: t) : t =
    { a_type with
      reg_type = List.map update_func a_type.reg_type;
      mem_type = MemType.map update_func a_type.mem_type }

  let find_one_base_info
      (base: Isa.imm_var_id) (reg_type: RegType.t) (mem_type: MemType.t) :
      CallAnno.base_info =
    let find_reg_idx =
      List.find_index (
        fun (x: entry_t) -> SingleExp.cmp (Entry.get_single_exp x) (SingleVar base) = 0
      ) reg_type
    in
    match find_reg_idx with
    | Some reg_idx -> BaseAsReg (Isa.get_full_reg_by_idx reg_idx)
    | None ->
      let find_mem_entry =
        List.find_map (
          fun (ptr, part_mem) ->
            List.find_map (
              fun (off, _, entry) ->
                if SingleExp.cmp (Entry.get_single_exp entry) (SingleVar base) = 0 then
                  Some (ptr, off)
                else None
            ) part_mem
        ) mem_type
      in
      begin match find_mem_entry with
      | Some (ptr, off) -> BaseAsSlot (ptr, off)
      | _ ->
        if base < 0 then BaseAsGlobal
        else arch_type_error (Printf.sprintf "[find_base_info] cannot find base %d" base)
      end

  let find_all_base_info 
      (reg_type: RegType.t) (mem_type: MemType.t) : CallAnno.base_info MemType.mem_content =
    List.map (
      fun (ptr, part_mem) ->
        let base_info = find_one_base_info ptr reg_type mem_type in
        ptr,
        List.map (
          fun (off, range, _) -> off, range, base_info
        ) part_mem
    ) mem_type

  let get_func_interface
      (smt_ctx: SmtEmitter.t)
      (func_name: Isa.label)
      (func_type: t list)
      (context: SingleContext.t list)
      (sub_sol: int -> entry_t -> entry_t) : FuncInterface.t =
    let in_state = List.find (fun (x: t) -> x.label = func_name) func_type in
    let out_state = List.find (fun (x: t) -> x.label = Isa.ret_label) func_type in

    SmtEmitter.push smt_ctx;
    (* MemType.gen_implicit_mem_constraints smt_ctx in_state.mem_type; *)
    SingleContext.add_assertions smt_ctx context;

    (* let helper (pc: int) (e: SingleEntryType.t) : SingleEntryType.t =
      let r = 
        SingleSubtype.sub_sol_single_to_range 
          infer_state.single_subtype infer_state.input_var_set (e, pc) 
      in
      match r with
      | Single exp -> exp
      | _ -> SingleTop
    in *)
    let in_mem = MemType.merge_local_mem_quick_cmp smt_ctx in_state.mem_type in
    let res: FuncInterface.t = {
      func_name = func_name;
      in_reg = in_state.reg_type;
      in_mem = in_mem;
      context = context;
      out_reg = List.map (sub_sol out_state.pc) out_state.reg_type;
      out_mem = MemType.map (sub_sol out_state.pc) (MemType.merge_local_mem_quick_cmp smt_ctx out_state.mem_type);
      base_info = find_all_base_info in_state.reg_type in_mem;
    }
    in
    SmtEmitter.pop smt_ctx 1;
    res

  let get_branch_target_type
      (block_subtype_list: block_subtype_t list)
      (branch_block_label: Isa.label)
      (branch_block_idx: int)
      (branch_target_label: Isa.label) :
      t * t =
    let find_result =
      List.fold_left (
        fun (acc: int option * block_subtype_t option) (entry: block_subtype_t) ->
          let acc_pc, acc_block = acc in
          let block, _ = entry in
          (
            match acc_pc with
            | Some _ -> acc_pc
            | None ->
              if block.label = branch_block_label then Some (block.pc + branch_block_idx)
              else acc_pc
          ), (
            match acc_block with
            | Some _ -> acc_block
            | None ->
              if block.label = branch_target_label then Some entry
              else acc_block
          )
      ) (None, None) block_subtype_list
    in
    match find_result with
    | Some branch_pc, Some (target_block, target_sub_list) ->
      List.find (fun (x: t) -> x.pc = branch_pc) target_sub_list, target_block
    | _ -> 
      arch_type_error (
        Printf.sprintf "cannot find block for branch at block %s (idx: %d) to target block %s" 
          branch_block_label branch_block_idx branch_target_label
      )

end

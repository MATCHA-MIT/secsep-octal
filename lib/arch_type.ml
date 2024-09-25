open Pretty_print
open Entry_type
open Cond_type_new
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

module ArchType (Entry: EntryType) = struct
  exception ArchTypeError of string
  let arch_type_error msg = raise (ArchTypeError ("[Arch Type Error] " ^ msg))

  type entry_t = Entry.t
  module MemAnno = FullMemAnno

  module Isa = Isa (MemAnno)
  module RegType = RegType (Entry)
  module MemType = MemType (Entry)
  module CondType = CondType (Entry)

  module FuncInterface = FuncInterface (Entry)

  type t = {
    label: Isa.label;
    pc: int;
    reg_type: RegType.t;
    mem_type: MemType.t;
    flag: entry_t * entry_t;
    branch_hist: (CondType.t * int) list;
    full_not_taken_hist: (CondType.t * int) list;
    constraint_list: (Constraint.t * int) list;
    (* smt_ctx: SmtEmitter.t; *)
    local_var_map: Entry.local_var_map_t;
    useful_var: SingleExp.SingleVarSet.t;
    global_var: SingleExp.SingleVarSet.t
    (* Maybe add constraint set here!!! *)
  }

  type block_subtype_t = t * (t list)

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
       : t =
    let idx0, reg_type = RegType.init_reg_type start_var in
    Printf.printf "BB %s vars for reg: [%s, %s)\n" label (Entry.to_string start_var) (Entry.to_string idx0);
    {
      label = label;
      pc = start_pc;
      reg_type = reg_type;
      mem_type = mem_type;
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      branch_hist = [];
      full_not_taken_hist = [];
      constraint_list = [];
      local_var_map = Entry.get_empty_var_map;
      useful_var = SingleExp.SingleVarSet.empty;
      global_var = global_var
    }

  let init_from_layout 
      (label: Isa.label)
      (start_var: entry_t) (start_pc: int)
      (mem_layout: 'a MemType.mem_content)
      (global_var: SingleExp.SingleVarSet.t) : entry_t * t =
    let idx0, reg_type = RegType.init_reg_type start_var in
    Printf.printf "BB %s vars for reg: [%s, %s)\n" label (Entry.to_string start_var) (Entry.to_string idx0);
    let idx1, mem_type = MemType.init_mem_type_from_layout idx0 mem_layout in
    Printf.printf "BB %s vars for mem: [%s, %s)\n" label (Entry.to_string idx0) (Entry.to_string idx1);
    idx1, {
      label = label;
      pc = start_pc;
      reg_type = reg_type;
      mem_type = mem_type;
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      branch_hist = [];
      full_not_taken_hist = [];
      constraint_list = [];
      local_var_map = Entry.get_empty_var_map;
      useful_var = SingleExp.SingleVarSet.empty;
      global_var = global_var
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
      (global_var: SingleExp.SingleVarSet.t) : entry_t * (block_subtype_t list) =
    let helper (acc_var: entry_t) (bb: Isa.basic_block) : entry_t * (t * (t list)) =
      let label = bb.label in
      let acc_var, block_type = (init_from_layout label acc_var start_pc mem_layout global_var) in
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

  let get_ld_op_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) :
      entry_t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    (* TODO: Add constriant: addr_type is untainted *)
    (* TODO: ld/st op taint is not handled!!! *)
    let addr_type = get_mem_op_type curr_type disp base index scale in
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let addr_exp = Entry.get_single_exp addr_type in
    let addr_untaint_cons = Entry.get_untaint_constraint addr_type in
    if addr_exp = SingleTop then begin
      Printf.printf "addr_exp = SingleTop\n";
      Entry.get_top_type (), Unknown (SingleTop, SingleTop) :: addr_untaint_cons, SingleExp.SingleVarSet.empty
    end else
      let useful_vars = SingleExp.get_vars addr_exp in
      match sub_sol_func (addr_exp, curr_type.pc) with
      | Some opt_exp ->
        let opt_offset = RangeExp.to_mem_offset opt_exp size in
        (* let opt_offset = None in let _ = sub_sol_func in *)
        let orig_addr_offset = 
            (addr_exp, 
            SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, SingleExp.SingleConst size))) 
        in
        let simp_addr_offset = (
          match opt_offset with
          | Some o -> o
          | None -> orig_addr_offset
        ) in
        (* TODO: Still need to check with SMT solver after resolving range with opt_offset!!! *)
        begin match MemType.get_mem_type smt_ctx curr_type.mem_type orig_addr_offset simp_addr_offset with
        | Some (_, (off_w, off_r, e_t)) -> e_t, Subset (simp_addr_offset, off_r, off_w) :: addr_untaint_cons, useful_vars
        | None -> 
          Printf.printf "get_ld_op_type unknown addr orig %s simp %s\n" (MemOffset.to_string orig_addr_offset) (MemOffset.to_string simp_addr_offset);
          (* Use simp_addr_offset since we do not need to distinguish between eq and subset for resolving unknown address *)
          Entry.get_top_type (), Unknown simp_addr_offset :: addr_untaint_cons, useful_vars
        end
      | _ -> 
        Printf.printf "get_ld_op_type cannot simplify for addr_exp %s\n" (SingleExp.to_string addr_exp);
        Entry.get_top_type (), Unknown (SingleTop, SingleTop) :: addr_untaint_cons, useful_vars

  let set_st_op_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (curr_type: t)
      (disp: Isa.immediate option) (base: Isa.register option)
      (index: Isa.register option) (scale: Isa.scale option)
      (size: int64) (new_type: entry_t) :
      t * (Constraint.t list) * SingleExp.SingleVarSet.t =
    (* TODO: Add constriant: addr_type is untainted *)
    (* TODO: ld/st op taint is not handled!!! *)
    let addr_type = get_mem_op_type curr_type disp base index scale in
    let addr_type = Entry.repl_local_var curr_type.local_var_map addr_type in
    let addr_exp = Entry.get_single_exp addr_type in
    let addr_untaint_cons = Entry.get_untaint_constraint addr_type in
    if addr_exp = SingleTop then
      curr_type, Unknown (SingleTop, SingleTop) :: addr_untaint_cons, SingleExp.SingleVarSet.empty
    else
      let useful_vars = SingleExp.get_vars addr_exp in
      match sub_sol_func (addr_exp, curr_type.pc) with
      | Some opt_exp ->
        let opt_offset = RangeExp.to_mem_offset opt_exp size in
        (* let opt_offset = None in let _ = sub_sol_func in *)
        let orig_addr_offset = 
            (addr_exp, 
            SingleExp.eval (SingleExp.SingleBExp (SingleExp.SingleAdd, addr_exp, SingleExp.SingleConst size))) 
        in
        let simp_addr_offset = (
          match opt_offset with
          | Some o -> o
          | None -> orig_addr_offset
        ) in
        (* TODO: Still need to check with SMT solver after resolving range with opt_offset!!! *)
        begin match MemType.set_mem_type smt_ctx true curr_type.mem_type orig_addr_offset simp_addr_offset new_type with
        | Some (new_mem, write_constraints) -> 
          { curr_type with mem_type = new_mem }, write_constraints @ addr_untaint_cons, useful_vars
        | None -> 
          (* Printf.printf "set_st_op_type unknown addr orig %s simp %s\n" (MemOffset.to_string orig_addr_offset) (MemOffset.to_string simp_addr_offset); *)
          (* Use simp_addr_offset since we do not need to distinguish between eq and subset for resolving unknown address *)
          curr_type, Unknown simp_addr_offset :: addr_untaint_cons, useful_vars
        end
      | None -> 
        (* Printf.printf "set_st_op_type cannot simplify for addr_exp %s\n" (SingleExp.to_string addr_exp); *)
        curr_type, Unknown (SingleTop, SingleTop) :: addr_untaint_cons, useful_vars
    
  let get_src_op_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (curr_type: t)
      (src: Isa.operand) :
      entry_t * t * (Constraint.t list) =
    match src with
    | ImmOp imm -> (Entry.get_const_type imm, curr_type, [])
    | RegOp r -> (get_reg_type curr_type r, curr_type, [])
    | MemOp (disp, base, index, scale) ->
      (get_mem_op_type curr_type disp base index scale, curr_type, [])
    | LdOp (disp, base, index, scale, size, mem_anno (* TODO: check offset and generate constraints *)) ->
      let src_type, src_constraint, src_useful =
        get_ld_op_type smt_ctx sub_sol_func curr_type disp base index scale size
      in
      let ld_op_constraint = match MemAnno.get_taint mem_anno with
        | None -> []
        | Some taint -> Entry.update_ld_taint_constraint src_type taint
      in
      (src_type, { curr_type with useful_var = SingleExp.SingleVarSet.union curr_type.useful_var src_useful }, ld_op_constraint @ src_constraint)
    | StOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a st op")
    | LabelOp _ -> arch_type_error ("get_src_op_type: cannot get src op type of a label op")
  
  let set_dest_op_type
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (curr_type: t)
      (dest: Isa.operand)
      (new_type: entry_t) :
      t * (Constraint.t list) =
    match dest with
    | RegOp r -> (set_reg_type curr_type r new_type, [])
    | StOp (disp, base, index, scale, size, mem_anno) ->
      let new_type, st_op_constraint = match MemAnno.get_taint mem_anno with
        | None -> new_type, []
        | Some taint -> Entry.update_st_taint_constraint new_type taint
      in
      let next_type, dest_constraint, dest_useful =
        set_st_op_type smt_ctx sub_sol_func curr_type disp base index scale size new_type
      in
      ({ next_type with useful_var = SingleExp.SingleVarSet.union next_type.useful_var dest_useful }, 
      st_op_constraint @ dest_constraint)
    | _ -> arch_type_error ("set_dest_op_type: dest is not reg or st op")

  let constriant_list_add_pc
      (constraint_list: Constraint.t list) (pc: int) :
      (Constraint.t * int) list =
    List.map (fun x -> x, pc) constraint_list

  let add_constraints
      (curr_type: t) (new_constraints: Constraint.t list) : t =
    {
      curr_type with constraint_list = (List.map (fun x -> x, curr_type.pc) new_constraints) @ curr_type.constraint_list
    }

  let type_prop_non_branch
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (curr_type: t)
      (inst: Isa.instruction) :
      t =
    (* We do not update pc in this function! Should update outside!!! *)
    let curr_type = { curr_type with flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ()) } in
    match inst with
    | BInst (bop, dest, src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src1 in
      let dest_type = Entry.exe_bop_inst bop src0_type src1_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx sub_sol_func curr_type dest dest_type in
      let next_type = add_constraints next_type (src0_constraint @ src1_constraint @ dest_constraint) in
      { next_type with local_var_map = new_local_var }
    (* | UInst (Mov, dest, src)
    | UInst (Lea, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map src_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint
    | UInst (MovS, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint
    | UInst (MovZ, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx curr_type src in
      let dest_type = Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx curr_type dest dest_type in
      { next_type with local_var_map = new_local_var }, src_constraint @ dest_constraint *)
    | UInst (uop, dest, src) ->
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src in
      let dest_type = 
        begin match uop with
        | MovS -> Entry.ext_val SignExt 0L (Isa.get_op_size dest) src_type
        | MovZ -> Entry.ext_val ZeroExt 0L (Isa.get_op_size dest) src_type
        | _ -> Entry.exe_uop_inst uop src_type
        end 
      in
      let new_local_var, dest_type = Entry.update_local_var curr_type.local_var_map dest_type curr_type.pc in
      let next_type, dest_constraint = set_dest_op_type smt_ctx sub_sol_func curr_type dest dest_type in
      let next_type = add_constraints next_type (src_constraint @ dest_constraint) in
      { next_type with local_var_map = new_local_var }
    | Xchg (dest0, dest1, src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src1 in
      let next_type, dest0_constraint = set_dest_op_type smt_ctx sub_sol_func curr_type dest0 src1_type in
      let next_type, dest1_constraint = set_dest_op_type smt_ctx sub_sol_func next_type dest1 src0_type in
      add_constraints next_type (src0_constraint @ src1_constraint @ dest0_constraint @ dest1_constraint)
      (* next_type, 
      src0_constraint @ src1_constraint @ dest0_constraint @ dest1_constraint *)
    | Cmp (src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src1 in
      let src0_type = Entry.repl_local_var curr_type.local_var_map src0_type in
      let src1_type = Entry.repl_local_var curr_type.local_var_map src1_type in
      let useful_vars = 
        SingleExp.SingleVarSet.union 
          (SingleExp.get_vars (Entry.get_single_exp src0_type))
          (SingleExp.get_vars (Entry.get_single_exp src1_type))
      in
      let curr_type = add_constraints curr_type (src0_constraint @ src1_constraint) in
      { curr_type with flag = (src0_type, src1_type); 
        useful_var = SingleExp.SingleVarSet.union curr_type.useful_var useful_vars }
    | Test (src0, src1) ->
      let src0_type, curr_type, src0_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src0 in
      let src1_type, curr_type, src1_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src1 in
      let dest_type = Entry.repl_local_var curr_type.local_var_map (Entry.exe_bop_inst Isa.And src0_type src1_type) in
      let useful_vars = SingleExp.get_vars (Entry.get_single_exp dest_type) in
      let curr_type = add_constraints curr_type (src0_constraint @ src1_constraint) in
      { curr_type with flag = (dest_type, Entry.get_const_type (Isa.ImmNum 0L)); 
        useful_var = SingleExp.SingleVarSet.union curr_type.useful_var useful_vars }
    | Push (src, mem_anno) ->
      let size = Isa.get_op_size src in
      let rsp_type, curr_type, _ = get_src_op_type smt_ctx sub_sol_func curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst Isa.Sub rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let curr_type, _ = set_dest_op_type smt_ctx sub_sol_func curr_type (Isa.RegOp Isa.RSP) new_rsp_type in
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx sub_sol_func curr_type src in
      let next_type, dest_constraint = 
        set_dest_op_type smt_ctx sub_sol_func curr_type (Isa.StOp (None, Some Isa.RSP, None, None, size, mem_anno)) src_type 
      in
      add_constraints next_type (src_constraint @ dest_constraint)
      (* next_type, src_constraint @ dest_constraint *)
    | Pop (dst, mem_anno) ->
      let size = Isa.get_op_size dst in
      let src_type, curr_type, src_constraint = get_src_op_type smt_ctx sub_sol_func curr_type (Isa.LdOp (None, Some Isa.RSP, None, None, size, mem_anno)) in
      let next_type, dest_constraint = set_dest_op_type smt_ctx sub_sol_func curr_type dst src_type in
      let rsp_type, curr_type, _ = get_src_op_type smt_ctx sub_sol_func curr_type (Isa.RegOp Isa.RSP) in
      let new_rsp_type = 
        Entry.repl_local_var curr_type.local_var_map 
          (Entry.exe_bop_inst Isa.Add rsp_type (Entry.get_const_type (Isa.ImmNum size))) 
      in
      let next_type, _ = set_dest_op_type smt_ctx sub_sol_func next_type (Isa.RegOp Isa.RSP) new_rsp_type in
      add_constraints next_type (src_constraint @ dest_constraint)
      (* next_type, src_constraint @ dest_constraint *)
    | Nop | Syscall | Hlt -> curr_type
    | _ -> arch_type_error "inst not implemented"

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
      t * block_subtype_t list =
    let _ = smt_ctx in
    match inst with
    | Jmp label ->
      (* 1. Add curr_type to target block's subtype *)
      let block_subtype = (add_block_subtype label curr_type block_subtype) in
      (* 2. This is the end of the current block, so update useful vars of this block in block_subtype *)
      let block_subtype = (update_with_end_type curr_type block_subtype) in (* Maybe need to update local var map too... *)
      curr_type, block_subtype
    | Jcond (cond, label) ->
      (* TODO: Add constraint: branch cond is untainted!!! *)
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
      {not_taken_type with useful_var = SingleExp.SingleVarSet.union not_taken_type.useful_var useful_var}, block_subtype
    | _ -> arch_type_error (Printf.sprintf "type_prop_branch: %s not supported" (Isa.string_of_instruction inst))

  let type_prop_call
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (target_func_name: Isa.label) : t =
    Printf.printf "type_prop_call %s\n" target_func_name;
    (* Entry.pp_local_var 0 curr_type.local_var_map;
    pp_arch_type 0 curr_type; *)
    let sub_sol_func (e: SingleExp.t) : MemOffset.t option =
      match sub_sol_func (e, curr_type.pc) with
      | Some r -> RangeExp.to_mem_offset2 r
      | None -> None
    in
    let new_reg, new_mem, new_constraints, new_useful_vars =
      FuncInterface.func_call smt_ctx sub_sol_func func_interface_list
        curr_type.global_var curr_type.local_var_map curr_type.reg_type curr_type.mem_type target_func_name
    in
    let new_constraints = List.map (fun x -> (x, curr_type.pc)) new_constraints in
    { curr_type with
      reg_type = new_reg;
      mem_type = new_mem;
      flag = (Entry.get_top_untaint_type (), Entry.get_top_untaint_type ());
      constraint_list = new_constraints @ curr_type.constraint_list;
      useful_var = SingleExp.SingleVarSet.union new_useful_vars curr_type.useful_var
    }

  let type_prop_inst
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (inst: Isa.instruction)
      (block_subtype: block_subtype_t list) :
      t * block_subtype_t list =
    (* Update pc here!!! *)
    (* Printf.printf "Prop inst %d %s\n" curr_type.pc (Isa.string_of_instruction inst); *)
    (* let unknown_before = List.length (Constraint.get_unknown curr_type.constraint_list) in *)
    let next_type, block_subtype = 
      match inst with
      | Jmp _ | Jcond _ ->
        type_prop_branch smt_ctx curr_type inst block_subtype
      | Call target_func_name ->
        type_prop_call smt_ctx sub_sol_func func_interface_list curr_type target_func_name,
        (* let _ = func_interface_list in
        let _ = target_func_name in
        Printf.printf "Warning: haven't implemented so far!\n";
        curr_type, *)
        block_subtype
      | _ ->
        type_prop_non_branch smt_ctx sub_sol_func curr_type inst, block_subtype
    in
    (* let unknown_after = List.length (Constraint.get_unknown next_type.constraint_list) in
    (
      if unknown_after - unknown_before = 0 then () 
      else Printf.printf "PC %d inst %s introduces unknown addr\n" curr_type.pc (Isa.string_of_instruction inst)
    ); *)
    (* Printf.printf "type_prop_inst %s useful_vars %s\n" (Isa.string_of_instruction inst) (String.concat "," (List.map string_of_int (SingleExp.SingleVarSet.to_list next_type.useful_var))); *)
    {next_type with pc = next_type.pc + 1}, block_subtype

  let type_prop_block
      (smt_ctx: SmtEmitter.t)
      (sub_sol_func: (SingleExp.t * int) -> RangeExp.t option)
      (func_interface_list: FuncInterface.t list)
      (curr_type: t)
      (block: Isa.instruction list)
      (block_subtype: block_subtype_t list) :
      t * (block_subtype_t list) =
    let curr_type, block_subtype = 
      List.fold_left (
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
      curr_type, update_with_end_type curr_type block_subtype
    else curr_type, block_subtype

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

end

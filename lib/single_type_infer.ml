open Isa
open Single_exp
open Single_entry_type
open Mem_offset_new
open Single_context
open Constraint
(* open Constraint *)
open Single_subtype
open Smt_emitter
open Pretty_print
open Full_mem_anno
open Branch_anno_type
open Sexplib.Std

module SingleTypeInfer = struct
  exception SingleTypeInferError of string

  module Isa = Isa (FullMemAnno)

  let single_type_infer_error msg = raise (SingleTypeInferError ("[Single Type Infer Error] " ^ msg))

  module ArchType = SingleSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface

  type t = {
    func_name: Isa.label;
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    single_subtype: SingleSubtype.t;
    next_var: SingleEntryType.t;
    input_var_set: SingleEntryType.SingleVarSet.t;
    context: SingleContext.t list;
    smt_ctx: SmtEmitter.t;
  }
  [@@deriving sexp]

  let state_list_to_file (filename: string) (infer_result: t list) =
    let open Sexplib in
    let channel = open_out filename in
    Sexp.output_hum channel (sexp_of_list sexp_of_t infer_result)

  let state_list_from_file (filename: string) : t list =
    let open Sexplib in
    let channel = open_in filename in
    let s_exp = Sexp.input_sexp channel in
    list_of_sexp t_of_sexp s_exp

  let pp_func_type (lvl: int) (infer_state: t) =
    List.iter (fun x -> ArchType.pp_arch_type lvl x) infer_state.func_type

  let pp_ocaml_state (lvl: int) (buf: Buffer.t) (infer_state: t) =
    PP.bprint_lvl lvl buf "{\n";
    PP.bprint_lvl (lvl + 1) buf "func_name = \"%s\";\n" infer_state.func_name;
    PP.bprint_lvl (lvl + 1) buf "func =\n"; Isa.pp_ocaml_block_list (lvl + 2) buf infer_state.func; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "func_type =\n"; ArchType.pp_ocaml_arch_type_list (lvl + 2) buf infer_state.func_type; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "single_subtype =\n"; SingleSubtype.pp_ocaml_single_subtype (lvl + 2) buf infer_state.single_subtype; PP.bprint_lvl (lvl + 2) buf ";\n";
    PP.bprint_lvl (lvl + 1) buf "next_var = SingleTop;\n";
    PP.bprint_lvl (lvl + 1) buf "input_var_set = %s;\n" (SingleExp.var_set_to_ocaml_string infer_state.input_var_set);
    PP.bprint_lvl (lvl + 1) buf "smt_ctx = SmtEmitter.init_smt_ctx ();\n";
    PP.bprint_lvl lvl buf "}\n"

  let init
      (prog: Isa.prog)
      (func_name: string)
      (func_mem_interface: ArchType.MemType.t) : t =
    let global_var_list = List.map (fun (_, x) -> x) (Isa.StrM.to_list prog.imm_var_map) in
    let global_var_set = SingleExp.SingleVarSet.of_list global_var_list in
    let min_global_var =
      List.fold_left (
        fun acc x -> if x < acc then x else acc
      ) 0 global_var_list
    in
    let start_pc = 1 - min_global_var in
    (* I am still not sure whether to use offset or ptr+offset in MemType, but just add base here :) *)
    let func_mem_interface = ArchType.MemType.add_dummy_stack_slot func_mem_interface in
    let func_mem_interface = ArchType.MemType.add_base_to_offset func_mem_interface in
    let max_var =
      ArchType.MemType.fold_left (
        fun acc (x: SingleEntryType.t) ->
          match x with
          | SingleVar v -> if v > acc then v else acc
          | SingleTop -> acc
          | _ -> single_type_infer_error "[init] func_mem_interface contains dep type exp"
      ) (Isa.total_reg_num) func_mem_interface
    in
    let start_var = SingleEntryType.SingleVar (max_var + 1) in
    let input_var_set = 
      SingleEntryType.SingleVarSet.of_list 
      (List.init (max_var - min_global_var + 1) (fun x -> x + min_global_var)) 
    in
    let func_body = (Isa.get_func prog func_name).body in
    let (_, next_var), arch_type_list =
      List.fold_left_map (
        fun (start_pc, start_var) (bb: Isa.basic_block) ->
          let next_pc = start_pc + List.length bb.insts + 1 in
          if bb.label = func_name then begin
            Printf.printf "BB %s pc = %d\n" bb.label start_pc;
            (
              (next_pc, start_var), 
              ArchType.init_func_input_from_layout 
                bb.label (SingleVar 0) start_pc func_mem_interface 
                global_var_set ArchType.TypeInferDep
            )
          end else begin
            Printf.printf "BB %s pc = %d\n" bb.label start_pc;
            let next_var, arch_type =
              ArchType.init_from_layout 
                bb.label start_var start_pc func_mem_interface 
                global_var_set ArchType.TypeInferDep
            in
            ((next_pc, next_var), arch_type)
          end
      ) (start_pc, start_var) func_body
    in
    (* ArchType.pp_arch_type_list 0 arch_type_list; *)
    {
      func_name = func_name;
      func = func_body;
      func_type = arch_type_list;
      single_subtype = [];
      next_var = next_var;
      input_var_set = input_var_set;
      context = [];
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks
    (func_interface_list: FuncInterface.t list)
    (* (infer_state: t) : t * (ArchType.block_subtype_t list) = *)
    (infer_state: t) (iter_left: int) : t * (ArchType.block_subtype_t list) =
    let helper 
        (acc: (ArchType.block_subtype_t list) * (Isa.instruction list list)) 
        (block: Isa.basic_block) (block_type: ArchType.t) : 
        (ArchType.block_subtype_t list) * (Isa.instruction list list) =
      SmtEmitter.push infer_state.smt_ctx;
      SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_subtype block_type.useful_var;
      (* Printf.printf "Block %s solver \n%s\n" block.label (Z3.Solver.to_string solver); *)
      (* Printf.printf "type_prop_block %s\n" block.label; *)
      let _ = iter_left in
      let block_subtype, update_block_list = acc in
      let (_, block_subtype), block =
        (* if iter_left = 1 && block.label = ".L2" then begin
          Printf.printf "skip prop %s\n" block.label;
          block_type, block_subtype
        end else *)
        ArchType.type_prop_block infer_state.smt_ctx 
          (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_subtype infer_state.input_var_set) 
          func_interface_list block_type block.insts block_subtype
      in
      (* Printf.printf "After prop block %s\n" block.label; *)
      SmtEmitter.pop infer_state.smt_ctx 1;
      block_subtype, block :: update_block_list
    in
    let block_subtype = ArchType.init_block_subtype_list_from_block_type_list infer_state.func_type in
    Printf.printf "func len %d, type len %d\n" (List.length infer_state.func) (List.length infer_state.func_type);
    let block_subtype, rev_new_block_list = List.fold_left2 helper (block_subtype, []) infer_state.func infer_state.func_type in
    Printf.printf "block_subtype len %d\n" (List.length block_subtype);
    let new_func = 
      List.map2 (
        fun (x: Isa.basic_block) (y: Isa.instruction list) -> { x with insts = y }
      ) infer_state.func (List.rev rev_new_block_list)
    in
    { infer_state with 
      func = new_func;
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type },
    block_subtype
    (* let single_subtype, block_subtype = SingleSubtype.init block_subtype in
    Printf.printf "2\n";
    let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype infer_state.input_var_set solver_iter in
    { infer_state with 
      func_type = ArchType.update_with_block_subtype block_subtype infer_state.func_type; 
      single_subtype = single_subtype 
    } *)

  let update_mem (infer_state: t) : t =
    (* For each unknown offset, we do two things in this function:
      1. Check whether the offset belongs to some memory slot if adding some extra context (heuristic);
      2. Check whether the offset refers to a new local stack slot and update local stack if needed. *)
    SmtEmitter.push infer_state.smt_ctx;
    let mem_type = (List.nth infer_state.func_type 0).mem_type in
    let update_list = ArchType.MemType.init_stack_update_list mem_type in
    let helper 
        (acc: ((MemOffset.t * bool) list) * (SingleContext.t list)) 
        (a_type: ArchType.t) : 
        ((MemOffset.t * bool) list) * (SingleContext.t list) =
      let acc_update_list, acc_context = acc in
      let unknown_list = Constraint.get_unknown a_type.constraint_list in
      (* MemOffset.pp_unknown_list 0 unknown_list; *)
      let unknown_range = 
        List.map (
          fun ((l, r), pc) -> 
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (l, pc),
            SingleSubtype.sub_sol_single_to_range infer_state.single_subtype infer_state.input_var_set (r, pc)
        ) unknown_list
      in
      let new_offset_list = List.filter_map MemOffset.from_range unknown_range in
      (* Check whether the each offset belongs to some memory slot if add some contraints *)
      let is_val_offset_list, not_val_offset_list = List.partition (MemOffset.is_val infer_state.input_var_set) new_offset_list in
      let is_val_offset_list, new_context_list_list =
        List.partition_map (ArchType.MemType.get_heuristic_mem_type infer_state.smt_ctx mem_type) is_val_offset_list
      in
      let new_offset_list = is_val_offset_list @ not_val_offset_list in
      (* Printf.printf "update_mem\n";
      MemOffset.pp_off_list 0 new_offset_list;  *)
      MemOffset.insert_new_offset_list infer_state.smt_ctx acc_update_list new_offset_list,
      List.flatten (acc_context :: new_context_list_list)
    in
    let update_list, new_context_list = List.fold_left helper (update_list, []) infer_state.func_type in
    let next_var, func_type =
      List.fold_left_map (
        fun (acc: SingleEntryType.t) (a_type: ArchType.t) ->
          let acc, new_mem = ArchType.MemType.update_mem acc a_type.mem_type update_list in
          acc, { a_type with mem_type = new_mem }
      ) infer_state.next_var infer_state.func_type
    in
    let local_var_set = 
      List.fold_left (
        fun acc entry -> 
          SingleExp.SingleVarSet.union acc (ArchType.get_local_var_set entry)
      ) SingleExp.SingleVarSet.empty func_type
    in
    let single_subtype = SingleSubtype.filter_entry infer_state.single_subtype local_var_set in
    SmtEmitter.pop infer_state.smt_ctx 1;
    { infer_state with
      func_type = func_type;
      single_subtype = single_subtype;
      next_var = next_var;
      context = new_context_list @ infer_state.context;
    }
    (* TODO: Also remove removed vars in single_subtype *)

  let check_or_assert_callee_context (infer_state: t) : bool * t =
    (* Return resolved, context list*)
    let has_callee_unknwon_context =
      List.find_opt (
        fun (x: ArchType.t) -> Constraint.has_callee_unknown_context x.constraint_list
      ) infer_state.func_type <> None
    in
    if has_callee_unknwon_context then false, infer_state
    else
      let context_list = List.concat_map (
        fun (x: ArchType.t) -> Constraint.get_callee_context x.constraint_list
      ) infer_state.func_type
      in
      let val_context_list = List.filter (SingleContext.is_val (SingleExp.is_val infer_state.input_var_set)) context_list in
      SmtEmitter.push infer_state.smt_ctx;
      let assert_list = 
        match SingleContext.check_or_assert infer_state.smt_ctx val_context_list with
        | Some assert_list -> assert_list
        | None -> single_type_infer_error "check_or_assert_callee_context unsat context"
      in
      SmtEmitter.pop infer_state.smt_ctx 1;
      List.is_empty context_list, { infer_state with context = assert_list @ infer_state.context }

  let clean_up_func_type (infer_state: t) : t =
    { infer_state with
      func_type = List.map ArchType.clean_up infer_state.func_type
    }

  let update_branch_anno (block_subtype: ArchType.block_subtype_t list) (func: Isa.basic_block list) : Isa.basic_block list =
    let helper (block_label: Isa.label) (idx: int) (inst: Isa.instruction) : Isa.instruction =
      match inst with
      | Jcond (cond, target, _) ->
        let branch_block_type, target_block_type = 
          ArchType.get_branch_target_type block_subtype block_label idx target
        in
        let anno = 
          BranchAnno.get_branch_anno 
            branch_block_type.reg_type branch_block_type.mem_type branch_block_type.local_var_map
            target_block_type.reg_type target_block_type.mem_type target_block_type.useful_var
        in
        Jcond (cond, target, Some anno)
      | Jmp (target, _) ->
        let branch_block_type, target_block_type = 
          ArchType.get_branch_target_type block_subtype block_label idx target
        in
        let anno = 
          BranchAnno.get_branch_anno 
            branch_block_type.reg_type branch_block_type.mem_type branch_block_type.local_var_map
            target_block_type.reg_type target_block_type.mem_type target_block_type.useful_var
        in
        Jmp (target, Some anno)
      | _ -> inst
    in
    List.map (
      fun (x: Isa.basic_block) ->
        {x with insts = List.mapi (helper x.label) x.insts}
    ) func

  let infer_one_func
      (prog: Isa.prog)
      (func_interface_list: FuncInterface.t list)
      (func_name: Isa.label)
      (func_mem_interface: ArchType.MemType.t)
      (iter: int)
      (solver_iter: int) : t =
    let init_infer_state = init prog func_name func_mem_interface in
    let rec helper (state: t) (iter_left: int) : t =
      if iter_left = 0 then
        (* { state with context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) } *)
        (* NOTE: Since we do not have block subtype here, we cannot update branch anno here. We update when iter_left = 1 (see below) *)
        { state with context = state.context @ (ArchType.MemType.get_mem_boundary_constraint (List.hd state.func_type).mem_type) }
      else begin
        let curr_iter = iter - iter_left + 1 in
        (* Prepare SMT context *)
        SmtEmitter.push state.smt_ctx;
        (* ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type; *)
        SingleContext.add_assertions state.smt_ctx (ArchType.MemType.get_mem_boundary_constraint (List.hd state.func_type).mem_type);
        SingleContext.add_assertions state.smt_ctx state.context;
        (* gen_implicit_mem_constraints state; *)
        (* 1. Prop *)
        Printf.printf "\n\nInfer iter %d type_prop_all_blocks%!\n\n" curr_iter;
        let state, block_subtype = type_prop_all_blocks func_interface_list state iter_left in
        (* 2. Insert stack addr in unknown list to mem type *)
        let unknown_resolved = 
          List.fold_left 
            (fun acc (x: ArchType.t) -> acc + List.length (Constraint.get_unknown x.constraint_list)) 
            0 state.func_type = 0
        in

        let num_unknown, label_unknown_list = 
          List.fold_left_map (
            fun (acc: int) (x: ArchType.t) -> 
              let unknown_list = Constraint.get_unknown x.constraint_list in
              acc + List.length unknown_list,
              (x.label, unknown_list)
          ) 0 state.func_type 
        in
        Printf.printf "After infer, %d unknown off, unknown list:\n" num_unknown;
        List.iter (
          fun (label, unknown_list) -> 
            Printf.printf "%s\n" label;
            MemOffset.pp_unknown_list 0 unknown_list
        ) label_unknown_list;
        
        Printf.printf "\n\n%s: Infer iter %d update_mem%!\n\n" func_name curr_iter;
        let state = update_mem state in
        Printf.printf "\n\nInfer iter %d after update_mem%!\n\n" curr_iter;
        pp_func_type 0 state;

        (* 2.5. Check or assert func call context *)
        let callee_context_resolved, state = check_or_assert_callee_context state in

        (* 3. Single type infer *)
        let single_subtype, block_subtype = SingleSubtype.init func_name block_subtype in
        let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype state.input_var_set solver_iter in
        let state = { state with single_subtype = single_subtype } in

        SmtEmitter.pop state.smt_ctx 1;

        Printf.printf "After infer, single subtype%!\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype;
        if unknown_resolved && callee_context_resolved then begin
          (* Directly return if unknown are all resolved. *)
          Printf.printf "\n\nSuccessfully resolved all memory accesses for %s at iter %d%!\n\n" func_name curr_iter;
          (* { state with context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) } *)
          { state with 
            func = update_branch_anno block_subtype state.func;
            context = state.context @ (ArchType.MemType.get_mem_boundary_constraint (List.hd state.func_type).mem_type) }
        end else begin
          let state = if iter_left = 1 then { state with func = update_branch_anno block_subtype state.func } else state in
          helper (clean_up_func_type state) (iter_left - 1)
        end
      end
    in
    helper init_infer_state iter

  let get_func_interface
      (infer_state: t) : FuncInterface.t =
    let sub_sol = 
      SingleSubtype.sub_sol_single_to_single_func_interface 
        infer_state.single_subtype infer_state.input_var_set 
    in
    ArchType.get_func_interface
      infer_state.smt_ctx
      infer_state.func_name
      infer_state.func_type
      infer_state.context
      sub_sol

  let pp_ocaml_infer_result (lvl: int) (buf: Buffer.t) (func_type_list: t list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun x -> 
        pp_ocaml_state (lvl + 1) buf x;
        PP.bprint_lvl (lvl + 1) buf ";\n"
    ) func_type_list;
    PP.bprint_lvl lvl buf "]\n"

  let infer
      (prog: Isa.prog)
      (func_mem_interface_list: (Isa.label * ArchType.MemType.t) list)
      (iter: int)
      (solver_iter: int) : t list =
    (* TODO: The correct order is for each function, infer its single type, then taint type, then next function.
      So this function should be moved to a upper-level model that infer all types. *)
    let helper 
        (acc: FuncInterface.t list) (entry: Isa.label * ArchType.MemType.t) :
        (FuncInterface.t list) * t =
      let func_name, func_mem_interface = entry in
      let infer_state = infer_one_func prog acc func_name func_mem_interface iter solver_iter in
      let func_interface = get_func_interface infer_state in
      Printf.printf "Infer state of func %s\n" func_name;
      pp_func_type 0 infer_state;
      FuncInterface.pp_func_interface 0 func_interface;
      (* let buf = Buffer.create 1000 in
      pp_ocaml_state 0 buf infer_state;
      Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)
      func_interface :: acc, infer_state
    in
    (* let func_mem_interface_list = List.filteri (fun i _ -> i >= 2 && i <= 3) func_mem_interface_list in *)
    (* let func_mem_interface_list = [List.nth func_mem_interface_list 2 ] in *)
    let _, infer_result = List.fold_left_map helper [] func_mem_interface_list in
    (* let buf = Buffer.create 1000 in
    pp_ocaml_infer_result 0 buf infer_result;
    Printf.printf "let %s_single_infer_state : SingleTypeInfer.t list =\n" prog_name;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)
    infer_result

end

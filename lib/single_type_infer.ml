open Isa
open Single_exp
open Single_entry_type
open Mem_offset_new
(* open Range_exp *)
open! Cond_type_new
open Single_context
open Constraint
(* open Constraint *)
open Func_interface
open Single_subtype
open Single_input_var_cond_subtype
(* open Single_block_invariance *)
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
    block_subtype: ArchType.block_subtype_t list; (* Tmp field: gen pipeline output for fast test. *)
    next_var: SingleEntryType.t;
    input_var_set: SingleEntryType.SingleVarSet.t;
    var_type_map: SingleEntryType.var_type_map_t;
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
    let func_mem_interface = ArchType.MemType.add_ret_addr_stack_slot func_mem_interface in
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
    let var_type_map = ArchType.MemType.get_var_type_map func_mem_interface in
    (* ArchType.pp_arch_type_list 0 arch_type_list; *)
    {
      func_name = func_name;
      func = func_body;
      func_type = arch_type_list;
      single_subtype = [];
      block_subtype = [];
      next_var = next_var;
      input_var_set = input_var_set;
      var_type_map = var_type_map;
      context = [];
      smt_ctx = SmtEmitter.init_smt_ctx ();
    }

  let type_prop_all_blocks
    (func_interface_list: FuncInterface.t list)
    (* (infer_state: t) : t * (ArchType.block_subtype_t list) = *)
    (infer_state: t) (iter_left: int) : t * (ArchType.block_subtype_t list) =
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let helper 
        (acc: (ArchType.block_subtype_t list) * (Isa.instruction list list)) 
        (block: Isa.basic_block) (block_type: ArchType.t) : 
        (ArchType.block_subtype_t list) * (Isa.instruction list list) =
      SmtEmitter.push infer_state.smt_ctx;
      SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_subtype block_type.useful_var;
      (* if block_type.label = ".L196" then () else *)
      SingleContext.add_assertions infer_state.smt_ctx block_type.context;
      SingleContext.add_assertions infer_state.smt_ctx block_type.tmp_context;
      (* Printf.printf "Block %s solver \n%s\n" block.label (Z3.Solver.to_string solver); *)
      Printf.printf "type_prop_block %s%!\n" block.label;
      SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx;
      let _ = iter_left in
      let block_subtype, update_block_list = acc in
      (* let sub_sol_func (exp_pc: SingleExp.t * int) : RangeExp.t option =
        let eval_align = SingleExp.eval_align ptr_align_list in
        match SingleSubtype.sub_sol_single_to_range_opt infer_state.single_subtype infer_state.input_var_set exp_pc with
        | None -> None
        | Some (Single e) -> Some (Single (eval_align e))
        | Some (Range (l, r, step)) -> Some (Range (eval_align l, eval_align r, step))
        | Some (SingleSet e_list) -> Some (SingleSet (List.map eval_align e_list))
        | Some Top -> Some Top
      in *)
      let sub_sol_func = 
        SingleSubtype.sub_sol_single_to_offset_opt 
          (SingleExp.eval_align ptr_align_list)
          infer_state.single_subtype infer_state.input_var_set
      in
      let sub_sol_list_func =
        SingleSubtype.sub_sol_single_to_offset_list
          (SingleExp.eval_align ptr_align_list)
          infer_state.single_subtype infer_state.input_var_set
      in
      let (_, block_subtype), block =
        (* if iter_left = 1 && block.label = ".L2" then begin
          Printf.printf "skip prop %s\n" block.label;
          block_type, block_subtype
        end else *)
        ArchType.type_prop_block infer_state.smt_ctx 
          sub_sol_func sub_sol_list_func
          (* (SingleSubtype.sub_sol_single_to_range_opt infer_state.single_subtype infer_state.input_var_set)  *)
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
    Printf.printf "Update mem smt ctx\n";
    SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx;
    let mem_type = (List.hd infer_state.func_type).mem_type in
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper mem_type in
    let update_list = ArchType.MemType.init_stack_update_list mem_type in
    let helper 
        (acc_update_list: ((MemOffset.t * bool) list))
        (a_type: ArchType.t) : 
        ((MemOffset.t * bool) list) * ArchType.t =
      SmtEmitter.push infer_state.smt_ctx;
      (* NOTE: I don't know why, remove adding single_subtype sol make infer faster,
          while add context and tmp_context make infer faster.
          Keep all of them for now. *)
      (* SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_subtype a_type.useful_var; *)
      SingleContext.add_assertions infer_state.smt_ctx a_type.context;
      SingleContext.add_assertions infer_state.smt_ctx a_type.tmp_context;
      let unknown_list = Constraint.get_unknown a_type.constraint_list in

      (* 1. Get heuristic mem type *)
      let rec add_branch_hist_helper
          (not_taken_hist: (SingleCondType.t * int) list)
          (off_pc: MemOffset.t * int) :
          (SingleCondType.t * int) list =
        match not_taken_hist with
        | [] -> []
        | (cond, cond_pc) :: tl ->
          let _, pc = off_pc in
          if pc > cond_pc then begin
            if not (SingleCondType.has_top cond) then
            SmtEmitter.add_assertions infer_state.smt_ctx [ SingleCondType.to_smt_expr infer_state.smt_ctx cond ];
            (* Printf.printf "pc %d add cond_pc %d cond %s\n" pc cond_pc (SingleCondType.to_string cond); *)
            add_branch_hist_helper tl off_pc
          end else
            not_taken_hist
      in
      let _ = add_branch_hist_helper in
      let _, unknown_or_new_ctx_list =
        List.fold_left_map (
          fun (not_taken_list: (SingleCondType.t * int) list)
              (off_pc: MemOffset.t * int) ->
            let not_taken_list = add_branch_hist_helper not_taken_list off_pc in
            not_taken_list,
            ArchType.MemType.get_heuristic_mem_type 
              infer_state.smt_ctx 
              (SingleSubtype.sub_sol_offset_to_offset_list 
                (SingleExp.eval_align ptr_align_list)
                infer_state.single_subtype infer_state.input_var_set)
              infer_state.input_var_set
              infer_state.var_type_map
              a_type.mem_type
              off_pc
        ) (List.rev a_type.full_not_taken_hist) (List.rev unknown_list)
      in
      let unknown_list, local_new_context_list_list =
        List.partition_map (fun x -> x) unknown_or_new_ctx_list
      in
      
      (* TODO: Check why using the following code is much faster. *)
      (* let unknown_list, local_new_context_list_list =
        List.partition_map (
          ArchType.MemType.get_heuristic_mem_type 
            infer_state.smt_ctx 
            (SingleSubtype.sub_sol_offset_to_offset_list 
            (SingleExp.eval_align ptr_align_list)
              infer_state.single_subtype infer_state.input_var_set)
            infer_state.input_var_set
            infer_state.var_type_map
            a_type.mem_type
        ) unknown_list
      in *)

      (* 2. Insert new stack slots *)
      (* MemOffset.pp_unknown_list 0 unknown_list; *)
      let unknown_range = 
        List.map (
          fun ((l, r), pc) -> 
            SingleSubtype.sub_sol_single_to_range 
              (SingleExp.eval_align ptr_align_list)
              infer_state.single_subtype infer_state.input_var_set (l, pc),
            SingleSubtype.sub_sol_single_to_range 
              (SingleExp.eval_align ptr_align_list)
              infer_state.single_subtype infer_state.input_var_set (r, pc)
        ) unknown_list
      in
      let new_offset_list = List.filter_map MemOffset.from_range unknown_range in
      Printf.printf "update_mem\n";
      MemOffset.pp_off_list 0 new_offset_list; 
      let result = 
        MemOffset.insert_new_offset_list infer_state.smt_ctx acc_update_list new_offset_list,
        { a_type with tmp_context = List.flatten (a_type.tmp_context :: local_new_context_list_list) }
      in
      SmtEmitter.pop infer_state.smt_ctx 1;
      result
    in
    (* Printf.printf "Update mem func type:\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_t infer_state.func_type)); *)
    let update_list, func_type = List.fold_left_map helper update_list infer_state.func_type in
    let next_var, func_type =
      List.fold_left_map (
        fun (acc: SingleEntryType.t) (a_type: ArchType.t) ->
          let acc, new_mem = ArchType.MemType.update_mem acc a_type.mem_type update_list in
          acc, { a_type with mem_type = new_mem }
      ) infer_state.next_var func_type
    in
    let local_var_set = 
      List.fold_left (
        fun acc entry -> 
          SingleExp.SingleVarSet.union acc (ArchType.get_local_var_set entry)
      ) SingleExp.SingleVarSet.empty func_type
    in
    let single_subtype = SingleSubtype.filter_entry infer_state.single_subtype local_var_set in
    { infer_state with
      func_type = func_type;
      single_subtype = single_subtype;
      next_var = next_var;
    }
    (* TODO: Also remove removed vars in single_subtype *)

  let check_or_assert_callee_context (infer_state: t) : bool * t =
    (* Return resolved, context list*)
    let has_callee_unknown_context =
      List.find_opt (
        fun (x: ArchType.t) -> Constraint.has_callee_unknown_context x.constraint_list
      ) infer_state.func_type <> None
    in
    if has_callee_unknown_context then begin
      Printf.printf "has_callee_unknown_context\n";
      false, infer_state
    end else
      let context_list = List.concat_map (
        fun (x: ArchType.t) -> Constraint.get_callee_context x.constraint_list
      ) infer_state.func_type
      in
      Printf.printf "Callee context list\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t context_list));
      SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx;
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

  let pp_graph
      (block_subtype: ArchType.block_subtype_t list) : unit =
    List.iter (
      fun (entry: ArchType.block_subtype_t) ->
        let target, br_list = entry in
        List.iter (
          fun (br: ArchType.t) ->
            Printf.printf "%s %s\n" br.label target.label;
            ()
        ) br_list;
        ()
    ) block_subtype;
    ()

  let infer_one_func
      (prog: Isa.prog)
      (func_interface_list: FuncInterface.t list)
      (func_name: Isa.label)
      (func_mem_interface: ArchType.MemType.t)
      (iter: int)
      (solver_iter: int) : t =
    let init_infer_state = init prog func_name func_mem_interface in
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper func_mem_interface in
    let rec helper (state: t) (iter_left: int) : t =
      if iter_left = 0 then
        (* { state with context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) } *)
        (* NOTE: Since we do not have block subtype here, we cannot update branch anno here. We update when iter_left = 1 (see below) *)
        { state with 
          func_type = List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                { x with context = x.context @ SingleSubtype.get_block_context state.single_subtype x.useful_var}
            ) state.func_type;
          context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) }
      else begin
        let curr_iter = iter - iter_left + 1 in
        (* Prepare SMT context *)
        SmtEmitter.push state.smt_ctx;
        (* ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type; *)
        SingleContext.add_assertions state.smt_ctx (ArchType.MemType.get_mem_boundary_constraint (List.hd state.func_type).mem_type);
        SingleContext.add_assertions state.smt_ctx state.context;
        (* if func_name = "SHA512_Update" then
          SingleContext.add_assertions state.smt_ctx [ Cond (Le, SingleConst 0L, SingleVar 27) ]; *)
        (* gen_implicit_mem_constraints state; *)
        (* 1. Prop *)
        Printf.printf "\n\nInfer iter %d type_prop_all_blocks%!\n\n" curr_iter;
        let state, block_subtype = type_prop_all_blocks func_interface_list state iter_left in
        let state = { state with block_subtype = block_subtype } in
        (* 2. Get heuristic mem type or insert stack addr in unknown list to mem type *)
        let unknown_resolved = 
          List.fold_left 
            (fun acc (x: ArchType.t) -> acc + List.length (Constraint.get_unknown x.constraint_list)) 
            0 state.func_type = 0
        in

        let num_unknown, label_unknown_list (* label_unknown_list *) = 
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

        (* 3. Check or assert func call context *)
        let callee_context_resolved, state = 
          if unknown_resolved then check_or_assert_callee_context state
          else begin
            Printf.printf "we do not check_or_assert_callee_context before all addr are resolved\n";
            false, state
          end
        in

        (* 4. Single type infer *)
        let single_subtype, block_subtype = SingleSubtype.init func_name block_subtype in
        (* Printf.printf "Block_subtype\n";
        pp_graph block_subtype; *)
        let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype state.input_var_set solver_iter in
        let state = { state with single_subtype = single_subtype } in

        (* 5. Input var block cond infer *)
        let pc_cond_map =
          SingleInputVarCondSubtype.solve
            state.smt_ctx
            (SingleSubtype.sub_sol_single_var (SingleExp.eval_align ptr_align_list) single_subtype state.input_var_set)
            block_subtype
        in
        (* Printf.printf "pc_cond_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleInputVarCondSubtype.sexp_of_pc_cond_t pc_cond_map)); *)
        let func_type =
          List.map2 (
            fun (x: ArchType.t) (pc_cond: SingleInputVarCondSubtype.pc_cond_t) ->
              let (label, _), cond_set = pc_cond in
              if x.label = label then
                { x with 
                  context = List.map (
                    fun (x: SingleInputVarCondSubtype.CondType.t) -> 
                      SingleContext.Cond x
                  ) (SingleInputVarCondSubtype.CondSet.to_list cond_set)
                }
              else
                single_type_infer_error "label does not match"
          ) state.func_type pc_cond_map
        in

        (* 6. Extra block invariance infer (resolve tmp_context) *)
        (* let func_type = 
          if unknown_resolved then
            SingleBlockInvariance.solve state.smt_ctx state.input_var_set func_type block_subtype 5
          else
            func_type
        in *)

        let state = { state with func_type = func_type } in

        SmtEmitter.pop state.smt_ctx 1;

        Printf.printf "After infer, single subtype%!\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype;
        if unknown_resolved && callee_context_resolved then begin
          (* Directly return if unknown are all resolved. *)
          Printf.printf "\n\nSuccessfully resolved all memory accesses for %s at iter %d%!\n\n" func_name curr_iter;
          (* { state with context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) } *)
          { state with 
            func = update_branch_anno block_subtype state.func;
            func_type = List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                { x with context = x.context @ (SingleSubtype.get_block_context state.single_subtype x.useful_var)}
            ) state.func_type;
            context = state.context @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type) }
        end else begin
          let state = (
            if iter_left = 1 then begin
              Printf.printf "After infer, %d unknown off, unknown list:\n" num_unknown;
              List.iter (
                fun (label, unknown_list) -> 
                  Printf.printf "%s\n" label;
                  MemOffset.pp_unknown_list 0 unknown_list
              ) label_unknown_list;
              { state with func = update_branch_anno block_subtype state.func }
            end else state
          ) in
          helper (clean_up_func_type state) (iter_left - 1)
        end
      end
    in
    helper init_infer_state iter

  let get_func_interface
      (infer_state: t) : FuncInterface.t =
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper (List.hd infer_state.func_type).mem_type in
    let sub_sol = 
      SingleSubtype.sub_sol_single_to_single_func_interface 
        (SingleExp.eval_align ptr_align_list)
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

  let filter_func_interface
      (func_mem_interface_list: (Isa.label * ArchType.MemType.t) list)
      (func_name_list: Isa.label list) : (Isa.label * ArchType.MemType.t) list =
    let func_name_set = Isa.StringSet.of_list func_name_list in
    List.filter (
      fun (x, _) -> Isa.StringSet.mem x func_name_set
    ) func_mem_interface_list

  let infer
      (prog: Isa.prog)
      (func_mem_interface_list: (Isa.label * ArchType.MemType.t) list)
      (general_func_interface_list: FuncInterfaceConverter.TaintFuncInterface.t list)
      (iter: int)
      (solver_iter: int) : t list =
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
    let general_func_interface_list = FuncInterfaceConverter.get_single_func_interface general_func_interface_list in
    (* let func_mem_interface_list = List.filteri (fun i _ -> i = 12) func_mem_interface_list in *)
    (* let func_mem_interface_list = [List.nth func_mem_interface_list 2 ] in *)
    let func_mem_interface_list = 
      filter_func_interface func_mem_interface_list [
        "sha512_block_data_order";
        "SHA512_Update";
      ] 
    in
    Printf.printf "%d\n" (List.length func_mem_interface_list);
    Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.MemType.sexp_of_t (List.map snd func_mem_interface_list)));
    let _, infer_result = List.fold_left_map helper general_func_interface_list func_mem_interface_list in
    (* let buf = Buffer.create 1000 in
    pp_ocaml_infer_result 0 buf infer_result;
    Printf.printf "let %s_single_infer_state : SingleTypeInfer.t list =\n" prog_name;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)
    infer_result

end

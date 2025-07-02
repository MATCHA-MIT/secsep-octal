open Set_sexp
open Isa
open Single_exp
open Single_entry_type
open Mem_offset_new
(* open Range_exp *)
open Mem_type_new
open Stack_spill_info
open Cond_type_new
open Single_context
open Constraint
(* open Constraint *)
open Func_interface
open Single_subtype
open Single_input_var_cond_subtype
open Single_block_invariance
open Single_ret_invariance
open Mem_alive
open Smt_emitter
open Pretty_print
open Full_mem_anno
open Branch_anno
open Call_anno
open Block_alive
open Func_input
open Sexplib.Std

module SingleTypeInfer = struct
  exception SingleTypeInferError of string

  module Isa = Isa (FullMemAnno) (BranchAnno) (CallAnno)

  let single_type_infer_error msg = raise (SingleTypeInferError ("[Single Type Infer Error] " ^ msg))

  module ArchType = SingleSubtype.ArchType
  module FuncInterface = ArchType.FuncInterface
  module BlockAlive = BlockAlive (SingleEntryType)
  module MemAlive = MemAlive (SingleEntryType)

  type t = {
    func_name: Isa.label;
    func: Isa.basic_block list;
    func_type: ArchType.t list;
    stack_spill_info: StackSpillInfo.t;
    alive_blocks: StringSet.t;
    single_subtype: SingleSubtype.t;
    block_subtype: ArchType.block_subtype_t list; (* Tmp field: gen pipeline output for fast test. *)
    next_var: SingleEntryType.t;
    input_var_set: SingleEntryType.SingleVarSet.t;
    var_type_map: SingleEntryType.var_type_map_t;
    (* state_context: SingleContext.t list; *)
    ret_subtype_list: (Isa.imm_var_id * (SingleExp.t list)) list;
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

  let init_extra_call_var_map
      (func_body: Isa.basic_block list)
      (func_type: ArchType.t list)
      (func_interface_list: FuncInterface.t list)
      (next_var: SingleExp.t) : SingleExp.t * (ArchType.t list) =
    let next_var_id =
      match next_var with
      | SingleVar v -> v
      | _ -> single_type_infer_error "next_var is not a var"
    in
    let get_one_func_var_map
        (next_var: int)
        (call_info: int * Isa.label) : int * (int * SingleExp.local_var_map_t) =
      let call_pc, callee_name = call_info in
      let func_interface_opt =
        List.find_opt (
          fun (x: FuncInterface.t) -> x.func_name = callee_name
        ) func_interface_list
      in
      match func_interface_opt with
      | None -> single_type_infer_error (Printf.sprintf "Func %s not found\n" callee_name)
      | Some func_interface ->
        let next_var, var_map =
          List.fold_left_map (
            fun (next_var: int) (child_var_id, _) ->
              next_var + 1, (child_var_id, SingleExp.SingleVar next_var)
          ) next_var func_interface.out_single_subtype_list
        in
        next_var, (call_pc, var_map)
    in
    let update_one_block_callee_var_map
        (next_var: int)
        (block_and_type: Isa.basic_block * ArchType.t) : int * ArchType.t =
      let block, block_type = block_and_type in
      let call_info_list =
        List.mapi (
          fun (idx: int) (inst: Isa.instruction) ->
            match inst with
            | Call (callee_label, _) ->
              Some (block_type.pc + idx, callee_label)
            | _ -> None
        ) block.insts |> (List.filter_map (fun x -> x))
      in
      let next_var, var_map =
        List.fold_left_map get_one_func_var_map next_var call_info_list
      in
      next_var, { block_type with extra_call_context_map_list = var_map }
    in
    let next_var_id, func_type =
      List.fold_left_map update_one_block_callee_var_map next_var_id (List.combine func_body func_type)
    in
    SingleExp.SingleVar next_var_id, func_type
    
  let init
      (prog: Isa.prog)
      (func_name: string)
      (func_mem_interface: ArchType.MemType.t)
      (stack_spill_info: StackSpillInfo.t)
      (func_interface_list: FuncInterface.t list) : t =
    let func_body = (Isa.get_func_of_prog prog func_name).body in
    let global_var_list = List.map (fun (_, x) -> x) (Isa.StrM.to_list prog.imm_var_map) in
    let global_var_set = SingleExp.SingleVarSet.of_list global_var_list in
    let min_global_var =
      List.fold_left (
        fun acc x -> if x < acc then x else acc
      ) 0 global_var_list
    in
    let start_pc = 1 - min_global_var in
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
    let next_var, arch_type_list = init_extra_call_var_map func_body arch_type_list func_interface_list next_var in
    let var_type_map = ArchType.MemType.get_var_type_map func_mem_interface in
    (* ArchType.pp_arch_type_list 0 arch_type_list; *)
    {
      func_name = func_name;
      func = func_body;
      func_type = arch_type_list;
      stack_spill_info = stack_spill_info;
      alive_blocks = StringSet.of_list (List.map (fun (x: ArchType.t) -> x.label) arch_type_list);
      single_subtype = [];
      block_subtype = [];
      next_var = next_var;
      input_var_set = input_var_set;
      var_type_map = var_type_map;
      (* state_context = []; *)
      ret_subtype_list = [];
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
      ArchType.add_assertions infer_state.smt_ctx block_type;
      Printf.printf "type_prop_block %s%!\n" block.label;
      (* SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx; *)
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
          (StackSpillInfo.is_spill infer_state.stack_spill_info)
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
    (* SmtEmitter.pp_smt_ctx 0 infer_state.smt_ctx; *)
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
      ArchType.add_assertions infer_state.smt_ctx a_type;
      let unknown_list = Constraint.get_unknown a_type.constraint_list in

      (* 1. Get heuristic mem type
            Very important: handle branch cond carefully, only add (taken1|taken2|...|assertion) to tmp_ctx
            It does not matter too much about whether we add all ctx to z3. 
            Missing ctx only cause assert more things that already true based on ctx.
            But it is very important to not assert too strong conditions. *)
      let full_taken_hist =
        List.map (
          fun (not_taken_cond, pc) -> 
            SingleCondType.not_cond_type not_taken_cond, pc
        ) a_type.full_not_taken_hist
      in
      let get_tmp_ctx_helper
          (off_pc: MemOffset.t * int) : 
          (MemOffset.t * int, (SingleContext.t list)) Either.t =
        let _, pc = off_pc in
        let result =
          ArchType.MemType.get_heuristic_mem_type 
            infer_state.smt_ctx 
            (SingleSubtype.sub_sol_offset_to_offset_list 
            (SingleExp.eval_align ptr_align_list)
              infer_state.single_subtype infer_state.input_var_set)
            infer_state.input_var_set
            infer_state.var_type_map
            a_type.mem_type
            off_pc
        in
        match result with
        | Left _ -> result
        | Right tmp_ctx ->
          let taken_cond_list =
            List.filter_map (
              fun (taken_cond, cond_pc) ->
                if pc > cond_pc then Some (SingleContext.Cond taken_cond)
                else None
            ) full_taken_hist
          in
          (* Make assertion only for the case where the branch before the load/store is not taken. *)
          Right (List.map (
            fun (x: SingleContext.t) -> SingleContext.ctx_or x (Or taken_cond_list)
          ) tmp_ctx)
      in
      let unknown_list, local_new_context_list_list =
        List.partition_map get_tmp_ctx_helper unknown_list
      in

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
      let helper (acc: bool) (a_type: ArchType.t) : bool * ArchType.t =
        let context_pc_list = Constraint.get_callee_context a_type.constraint_list in
        if List.is_empty context_pc_list then acc, a_type
        else begin
        SmtEmitter.push infer_state.smt_ctx;
        (* It is optional to add solution to smt_ctx here *)
        (* SingleSubtype.update_block_smt_ctx infer_state.smt_ctx infer_state.single_subtype a_type.useful_var; *)
        ArchType.add_assertions infer_state.smt_ctx a_type;
        (* Printf.printf "check_or_assert_callee_context block %s\n" a_type.label;
        List.iter (
          fun (ctx_pc: SingleContext.t list * int) ->
            let ctx_list, pc = ctx_pc in
            Printf.printf "pc%d\n%s\n" pc (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t ctx_list))
        ) context_pc_list; *)
        let full_taken_hist =
          List.map (
            fun (not_taken_cond, pc) -> 
              SingleCondType.not_cond_type not_taken_cond, pc
          ) a_type.full_not_taken_hist
        in
        let get_tmp_ctx_helper
            (ctx_pc: (SingleContext.t list) * int) : SingleContext.t list =
          let ctx, pc = ctx_pc in
          let assert_list = 
            match SingleContext.check_or_assert infer_state.smt_ctx ctx with
            | Some assert_list -> assert_list
            | None -> single_type_infer_error "check_or_assert_callee_context unsat context"
          in
          let taken_cond_list =
            List.filter_map (
              fun (taken_cond, cond_pc) ->
                if pc > cond_pc then Some (SingleContext.Cond taken_cond)
                else None
            ) full_taken_hist
          in
          List.map (fun x -> SingleContext.ctx_or x (Or taken_cond_list)) assert_list
        in
        let local_new_context_list_list =
          List.map get_tmp_ctx_helper context_pc_list
        in
        let result =
          { a_type with
            tmp_context = List.flatten (a_type.tmp_context :: local_new_context_list_list) }
        in
        SmtEmitter.pop infer_state.smt_ctx 1;
        false,
        result
        end
      in

      let resolved, func_type = List.fold_left_map helper true infer_state.func_type in
      resolved,
      { infer_state with func_type = func_type }
      
      (* let context_list = List.concat_map (
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
      List.is_empty context_list, { infer_state with context = assert_list @ infer_state.context } *)

  let clean_up_func_type (infer_state: t) : t =
    { infer_state with
      func_type = List.map ArchType.clean_up infer_state.func_type
    }

  let update_branch_anno (block_subtype: ArchType.block_subtype_t list) (func: Isa.basic_block list) : Isa.basic_block list =
    let helper (block_label: Isa.label) (idx: int) (inst: Isa.instruction) : Isa.instruction =
      match inst with
      | Jcond (cond, target, _) ->
        begin match ArchType.get_branch_target_type block_subtype block_label idx target with
        | Some (branch_block_type, target_block_type) ->
          let anno = 
            BranchAnno.get_branch_anno 
              branch_block_type.reg_type branch_block_type.mem_type branch_block_type.local_var_map
              target_block_type.reg_type target_block_type.mem_type target_block_type.useful_var
          in
          Jcond (cond, target, Some anno)
        | None -> Jcond (cond, target, None)
        end
      | Jmp (target, _) ->
        begin match ArchType.get_branch_target_type block_subtype block_label idx target with
        | Some (branch_block_type, target_block_type) ->
          let anno = 
            BranchAnno.get_branch_anno 
              branch_block_type.reg_type branch_block_type.mem_type branch_block_type.local_var_map
              target_block_type.reg_type target_block_type.mem_type target_block_type.useful_var
          in
          Jmp (target, Some anno)
        | None -> Jmp (target, None)
        end
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

  let add_var_bound_constraint (state: t) : t =
    let useful_constrained_var = List.fold_left (fun (acc: SingleEntryType.SingleVarSet.t) (arch_type: ArchType.t) ->
      Printf.printf "useful constrained var in %s: %s\n"
        arch_type.label
        (arch_type.useful_constrained_var |> SingleEntryType.SingleVarSet.sexp_of_t |> Sexplib.Sexp.to_string_hum);
      let new_vars = List.fold_left (fun acc (block_var: Isa.imm_var_id) ->
        let range = SingleSubtype.sub_sol_single_to_range
          (fun x -> x) (* I *)
          state.single_subtype
          state.input_var_set
          (SingleVar block_var, arch_type.pc)
        in
        let input_vars = Range_exp.RangeExp.get_var range in
        Printf.printf "block var %d --> input vars: %s\n"
          block_var
          (input_vars |> SingleEntryType.SingleVarSet.sexp_of_t |> Sexplib.Sexp.to_string_hum);
        SingleEntryType.SingleVarSet.union input_vars acc
      ) SingleEntryType.SingleVarSet.empty (SingleEntryType.SingleVarSet.to_list arch_type.useful_constrained_var)
      in
      SingleEntryType.SingleVarSet.union acc new_vars
    ) SingleEntryType.SingleVarSet.empty state.func_type
    in
    {state with
      func_type = List.map (fun (arch_type: ArchType.t) ->
        (* only add constraints for the entry block *)
        if arch_type.label <> state.func_name then arch_type else

        let vars = SingleEntryType.SingleVarSet.inter state.input_var_set useful_constrained_var in
        (* Printf.printf "input and userful constrainted vars: %s\n" (SingleEntryType.SingleVarSet.sexp_of_t vars |> Sexplib.Sexp.to_string_hum); *)
        (* ignore callee saved regs *)
        let callee_saved_reg_vars = arch_type.reg_type
          |> List.filteri (fun i _ -> Isa.is_reg_idx_callee_saved i)
          |> List.filter_map (fun s ->
               match s with
               | SingleEntryType.SingleVar v -> Some v
               | _ -> None
             )
        in
        (* Printf.printf "removing callee saved reg vars: %s\n" (SingleEntryType.SingleVarSet.sexp_of_t (SingleEntryType.SingleVarSet.of_list callee_saved_reg_vars) |> Sexplib.Sexp.to_string_hum); *)
        (* ignore callee saved regs *)
        let vars = SingleEntryType.SingleVarSet.diff vars (SingleEntryType.SingleVarSet.of_list callee_saved_reg_vars) in
        (* ignore pointers *)
        let memory_ptr_vars = arch_type.mem_type
          |> List.map (fun (mem_part: SingleEntryType.t Mem_type_new.MemTypeBasic.mem_part) ->
            let (ptr, _), _ = mem_part in ptr
          )
        in
        (* Printf.printf "removing pointer vars: %s\n" (SingleEntryType.SingleVarSet.sexp_of_t (SingleEntryType.SingleVarSet.of_list memory_ptr_vars) |> Sexplib.Sexp.to_string_hum); *)
        let vars = SingleEntryType.SingleVarSet.diff vars (SingleEntryType.SingleVarSet.of_list memory_ptr_vars) in

        Printf.printf "func %s block %s: adding bound constr for %d vars: %s\n"
          state.func_name arch_type.label
          (SingleEntryType.SingleVarSet.cardinal vars)
          (SingleEntryType.SingleVarSet.sexp_of_t vars |> Sexplib.Sexp.to_string_hum);
        let new_context = List.fold_left (
          fun c_list (var: Isa.imm_var_id) ->
            (SingleContext.Cond (Le, SingleConst 0L, SingleVar var)) ::
            (SingleContext.Cond (Lt, SingleVar var, SingleConst (Int64.shift_left 1L 31))) ::
            c_list
        ) arch_type.context (SingleEntryType.SingleVarSet.to_list vars)
        in
        { arch_type with context = new_context }
      ) state.func_type;
    }

  let infer_one_func
      (prog: Isa.prog)
      (func_interface_list: FuncInterface.t list)
      (func_name: Isa.label)
      (func_mem_interface: ArchType.MemType.t)
      (stack_spill_info: StackSpillInfo.t)
      (iter: int)
      (solver_iter: int) : t =
    let init_infer_state = init prog func_name func_mem_interface stack_spill_info func_interface_list in
    let ptr_align_list = ArchType.MemType.get_mem_align_constraint_helper func_mem_interface in
    let rec helper (state: t) (iter_left: int) : t =
      if iter_left = 0 then
        (* NOTE: Since we do not have block subtype here, we cannot update branch anno here. We update when iter_left = 1 (see below) *)
        { state with 
          func_type = List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                { x with 
                  context = x.context 
                    @ x.blk_br_context 
                    @ SingleSubtype.get_block_context state.single_subtype x
                    @ ArchType.MemType.get_all_mem_constraint x.mem_type
                }
            ) state.func_type;
          (* state_context = state.state_context 
            @ (List.hd state.func_type).context 
            @ (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type)  *)
        }
      else begin
        let curr_iter = iter - iter_left + 1 in
        (* Prepare SMT context *)
        SmtEmitter.push state.smt_ctx;
        (* ArchType.MemType.gen_implicit_mem_constraints state.smt_ctx (List.hd state.func_type).mem_type; *)
        (* SingleContext.add_assertions state.smt_ctx (ArchType.MemType.get_mem_boundary_constraint (List.hd state.func_type).mem_type); *)
        SingleContext.add_assertions state.smt_ctx (ArchType.MemType.get_all_mem_constraint (List.hd state.func_type).mem_type);
        (* SingleContext.add_assertions state.smt_ctx state.state_context; *)
        (* if func_name = "SHA512_Update" then
          SingleContext.add_assertions state.smt_ctx [ Cond (Le, SingleConst 0L, SingleVar 27) ]; *)
        (* gen_implicit_mem_constraints state; *)
        (* 1. Prop *)
        Printf.printf "\n\nInfer iter %d type_prop_all_blocks%!\n\n" curr_iter;
        let state, block_subtype = type_prop_all_blocks func_interface_list state iter_left in
        Printf.printf "Control flow\n%s\n" (
          block_subtype |> ArchType.get_block_subtype_label 
          |> sexp_of_list ArchType.sexp_of_block_subtype_label_t 
          |> Sexplib.Sexp.to_string_hum
        );
        Printf.printf "Useful vars\n%!";
        ArchType.pp_arch_type_useful_var_list 0 state.func_type;

        (* Get alive blocks *)
        let state = { state with alive_blocks = BlockAlive.solve state.func_name block_subtype } in
        (* NOTE: In single infer, prop does not use alive block info from last round to avoid symbolic exe/prop any blocks,
           since the alive info might be out-of-date, and we do not want to miss any block that might be target.
           Hence, a dead block (with no entry) can appear as a sub block due to symbolic executing it.
           But if it is known to be dead in this round of prop, i.e., no entrance, then it will not appear as a sup block 
           (we do not need to solve for its single vars).
           Hence, we filiter out dead blocks from sub block list to avoid complicating single subtype solve. *)
        let block_subtype =
          List.map (
            fun (sup_blk, sub_list) ->
              sup_blk,
              List.filter (
                fun (sub_blk: ArchType.t) ->
                  StringSet.mem sub_blk.label state.alive_blocks
              ) sub_list
          ) block_subtype
        in

        (* 2. Get heuristic mem type or insert stack addr in unknown list to mem type *)
        let unknown_resolved = 
          List.fold_left (
            fun acc (x: ArchType.t) ->
              if StringSet.mem x.label state.alive_blocks then 
                acc + List.length (Constraint.get_unknown x.constraint_list)
              else acc
          ) 0 state.func_type = 0
        in

        let num_unknown, label_unknown_list (* label_unknown_list *) = 
          List.fold_left_map (
            fun (acc: int) (x: ArchType.t) -> 
              if StringSet.mem x.label state.alive_blocks then 
              let unknown_list = Constraint.get_unknown x.constraint_list in
              acc + List.length unknown_list,
              (x.label, x.pc, unknown_list)
              else acc, (x.label, x.pc, [])
          ) 0 state.func_type 
        in
        Printf.printf "After infer, %d unknown off, unknown list:\n" num_unknown;
        List.iter (
          fun (label, pc, unknown_list) -> 
            Printf.printf "%s %d\n" label pc;
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
        Printf.printf "\n\n%s: Infer iter %d after check_or_assert_callee_context resolved %b%!\n\n" func_name curr_iter callee_context_resolved;

        (* 4. Single type infer *)
        (* let single_subtype, block_subtype = SingleSubtype.init func_name block_subtype in
        (* Printf.printf "Block_subtype\n";
        pp_graph block_subtype; *)
        let single_subtype = SingleSubtype.solve_vars single_subtype block_subtype state.input_var_set solver_iter in
        let state = 
          { state with 
            func_type = 
              ArchType.update_with_block_subtype_helper (
                fun (x: ArchType.t) (y: ArchType.t) -> { x with useful_var = y.useful_var }
              ) block_subtype state.func_type; 
            (* We need to update useful var *)
            single_subtype = single_subtype; 
            block_subtype = block_subtype 
          } 
        in
        Printf.printf "After infer, single subtype%!\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype; *)

        (* 4. Input var block cond infer *)
        let pc_cond_map =
          SingleInputVarCondSubtype.solve
            state.smt_ctx
            (SingleSubtype.sub_sol_single_var (SingleExp.eval_align ptr_align_list) state.single_subtype state.input_var_set)
            block_subtype
        in
        (* Printf.printf "pc_cond_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleInputVarCondSubtype.sexp_of_pc_cond_t pc_cond_map)); *)
        let func_type =
          List.map2 (
            fun (x: ArchType.t) (pc_cond: SingleInputVarCondSubtype.pc_cond_t) ->
              let (label, _), cond_set = pc_cond in
              if x.label = label then
                { x with 
                  blk_br_context = List.map (
                    fun (x: SingleInputVarCondSubtype.CondType.t) -> 
                      SingleContext.Cond x
                  ) (SingleInputVarCondSubtype.CondSet.to_list cond_set)
                }
              else
                single_type_infer_error "label does not match"
          ) state.func_type pc_cond_map
        in
        Printf.printf "\n\n%s: Infer iter %d after SingleInputVarCondSubtype%!\n\n" func_name curr_iter;

        (* 5. Extra block invariance infer (resolve tmp_context) *)
        let func_type, tmp_context_resolved = 
          if unknown_resolved && callee_context_resolved then
            let func_type = List.map (SingleSubtype.add_range_constraint_to_block_tmp_context state.single_subtype) func_type in
            SingleBlockInvariance.solve state.smt_ctx true state.input_var_set func_type block_subtype state.single_subtype 100
          else
            func_type, false
        in
        Printf.printf "\n\n%s: Infer iter %d after SingleBlockInvariance resolved %b%!\n\n" func_name curr_iter tmp_context_resolved;

        (* 6. Update dead_pc of dead blocks (after infer finished) *)
        let func_type =
          List.map (
            fun (a_type: ArchType.t) ->
              if StringSet.mem a_type.label state.alive_blocks then a_type
              else { a_type with dead_pc = a_type.pc }
          ) func_type
        in
        Printf.printf "\n\n%s: Infer iter %d after update dead_pc%!\n\n" func_name curr_iter;

        (* 7. Add extra ret invariance (after resolve everything) *)
        let func_type, ret_subtype_list =
          let sub_range_func = 
            SingleSubtype.sub_sol_single_to_range 
            (SingleExp.eval_align ptr_align_list)
            state.single_subtype state.input_var_set
          in
          if unknown_resolved && callee_context_resolved && tmp_context_resolved then
            SingleRetInvariance.gen_ret_invariance 
              state.smt_ctx sub_range_func state.input_var_set
              func_type block_subtype state.single_subtype
          else
            func_type, []
        in

        (* 8. Prop not alive ptr *)
        let func_type = MemAlive.solve block_subtype func_type in

        let state = { state with func_type = func_type; ret_subtype_list = ret_subtype_list } in

        (* 9. Single type infer *)
        (* Put this as the last step so that prop always use the latest solution than other steps,
           This ensures prop rules out impossible branches before input var block cond infer *)
        Printf.printf "\n\n%s: Infer iter %d before init single_subtype%!\n\n" func_name curr_iter;
        let single_subtype, block_subtype = SingleSubtype.init func_name state.input_var_set block_subtype in
        SingleSubtype.pp_single_subtype 0 single_subtype;
        Printf.printf "\n\n%s: Infer iter %d after init single_subtype len %d %!\n\n" func_name curr_iter (List.length single_subtype);
        (* Printf.printf "Block_subtype\n";
        pp_graph block_subtype; *)
        let single_subtype = 
          SingleSubtype.solve_vars state.smt_ctx single_subtype block_subtype state.input_var_set solver_iter
          (* |> SingleSubtype.remove_top_subtype  *)
          (* Optimization to speedup sub_sol *)
        in
        Printf.printf "After infer, single subtype len %d%!\n" (List.length single_subtype);
        SingleSubtype.pp_single_subtype 0 single_subtype;
        let single_subtype = SingleSubtype.remove_top_subtype single_subtype in
        let state = 
          { state with 
            func_type = 
              ArchType.update_with_block_subtype_helper (
                fun (x: ArchType.t) (y: ArchType.t) -> { x with useful_var = y.useful_var }
              ) block_subtype state.func_type; 
            (* We need to update useful var *)
            single_subtype = single_subtype; 
            block_subtype = block_subtype 
          } 
        in
        (* Printf.printf "After infer, single subtype%!\n";
        SingleSubtype.pp_single_subtype 0 state.single_subtype; *)

        SmtEmitter.pop state.smt_ctx 1;

        if unknown_resolved && callee_context_resolved && tmp_context_resolved then begin
          (* Directly return if unknown are all resolved. *)
          Printf.printf "\n\nSuccessfully resolved all memory accesses for %s at iter %d%!\n\n" func_name curr_iter;
          (* Printf.printf "After infer, single subtype%!\n";
          SingleSubtype.pp_single_subtype 0 state.single_subtype; *)

          (* Begin of merge subtype set sol *)
          (* We need to first add solution to context, to support merge, 
             but we do not want to keep unmerged solution in ctx,
             so we use tmp_func_type as a helper state and regenerate func_type later. *)
          let tmp_func_type = 
            List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                { x with 
                  context = x.context 
                    @ x.blk_br_context 
                    @ SingleSubtype.get_block_context single_subtype x
                    @ ArchType.MemType.get_all_mem_constraint x.mem_type
                }
            ) state.func_type
          in
          let single_subtype = 
            SingleSubtype.merge_all_set_sol 
              state.smt_ctx state.input_var_set 
              tmp_func_type block_subtype single_subtype 
          in
          Printf.printf "\n\nAfter merge single set sol\n";
          SingleSubtype.pp_single_subtype 0 single_subtype;
          (* End of merge subtype set sol *)

          let func_type = 
            List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                { x with 
                  context = x.context 
                    @ x.blk_br_context 
                    @ SingleSubtype.get_block_context single_subtype x
                    @ ArchType.MemType.get_all_mem_constraint x.mem_type
                }
            ) state.func_type
          in
          let func_type =
            List.map (
              fun (x: SingleSubtype.ArchType.t) ->
                if x.label = func_name then x else
                { x with
                  context = x.context @ (List.hd func_type).context }
            ) func_type
          in
          { state with 
            func = update_branch_anno block_subtype state.func;
            func_type = func_type;
            single_subtype = single_subtype;
            (* state_context = state.state_context 
              @ (List.hd func_type).context 
              @ (ArchType.MemType.get_all_mem_constraint (List.hd func_type).mem_type); *)
          }
        end else begin
          let state = (
            if iter_left = 1 then begin
              Printf.printf "Infer failed for %s\n" func_name;
              Printf.printf "After infer, %d unknown off, unknown list:\n" num_unknown;
              List.iter (
                fun (label, pc, unknown_list) -> 
                  Printf.printf "%s %d\n" label pc;
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
      (* infer_state.state_context *)
      infer_state.ret_subtype_list
      sub_sol sub_sol

  let pp_ocaml_infer_result (lvl: int) (buf: Buffer.t) (func_type_list: t list) =
    PP.bprint_lvl lvl buf "[\n";
    List.iter (
      fun x -> 
        pp_ocaml_state (lvl + 1) buf x;
        PP.bprint_lvl (lvl + 1) buf ";\n"
    ) func_type_list;
    PP.bprint_lvl lvl buf "]\n"

  let filter_func_input
      (func_mem_input_list: FuncInput.entry_t list)
      (func_name_list: Isa.label list) : FuncInput.entry_t list =
    let func_name_set = StringSet.of_list func_name_list in
    List.filter (
      fun (x: FuncInput.entry_t) -> StringSet.mem x.func_name func_name_set
    ) func_mem_input_list

  let infer
      (prog: Isa.prog)
      (func_input_list: FuncInput.entry_t list) (* this should already contain global symbols *)
      (general_func_interface_list: FuncInterfaceConverter.TaintFuncInterface.t list)
      (cached_func_interface_list: FuncInterface.t list)
      (iter: int)
      (solver_iter: int) : (FuncInterface.t list) * (t list) =
    let helper 
        (acc: FuncInterface.t list) (entry: FuncInput.entry_t) :
        (FuncInterface.t list) * t =
      let func_name = entry.func_name in
      let func_mem_interface = MemTypeBasic.map (fun (x, _) -> x) entry.mem_type in
      Printf.printf "Inferring func %s\n" func_name;
      let infer_state = infer_one_func prog acc func_name func_mem_interface entry.stack_spill_info iter solver_iter in
      let infer_state = add_var_bound_constraint infer_state in
      let func_interface = get_func_interface infer_state in
      Printf.printf "Infer state of func %s\n" func_name;
      pp_func_type 0 infer_state;
      Printf.printf "Interface of %s:\n%s\n"
        func_name
        (func_interface |> FuncInterface.sexp_of_t |> Sexplib.Sexp.to_string_hum);
      func_interface :: acc, infer_state
    in
    let general_func_interface_list = FuncInterfaceConverter.get_single_func_interface general_func_interface_list in

    (* Uncomment this to select functions to infer *)
    (* let func_input_list = 
      filter_func_input func_input_list [
        (* "ge_p3_tobytes";
        "fe_tobytes";
        "fe_mul_impl"; *)
        (* "fe_mul_impl_self2";
        "fe_mul_ttt_self1"; *)
        (* "fe_mul_impl";
        "table_select";
        "ge_madd";
        "ge_p2_dbl";
        "x25519_ge_p1p1_to_p3";
        "x25519_ge_p1p1_to_p2";
        "x25519_ge_scalarmult_base"; *)
        (* "SHA512_Init";
        "sha512_block_data_order";
        "SHA512_Final";
        "SHA512_Update";
        "SHA512"; *)
        "poly1305_donna_mul";
        "poly1305_update";
        "CRYPTO_poly1305_update";
      ] 
    in *)

    Printf.printf "%d\n" (List.length func_input_list);
    Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list FuncInput.MemType.sexp_of_t (List.map (fun (x: FuncInput.entry_t) -> x.mem_type) func_input_list)));

    let infer_func_name_set =
      List.map (fun (x: FuncInput.entry_t) -> x.func_name) func_input_list |> StringSet.of_list
    in
    let cached_func_interface_list =
      List.filter (fun (x: FuncInterface.t) -> not (StringSet.mem x.func_name infer_func_name_set)) cached_func_interface_list
    in

    List.fold_left_map helper (cached_func_interface_list @ general_func_interface_list) func_input_list
    (* let buf = Buffer.create 1000 in
    pp_ocaml_infer_result 0 buf infer_result;
    Printf.printf "let %s_single_infer_state : SingleTypeInfer.t list =\n" prog_name;
    Printf.printf "%s" (String.of_bytes (Buffer.to_bytes buf)); *)

end

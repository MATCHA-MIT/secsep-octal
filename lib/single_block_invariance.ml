open Isa_basic
open Single_exp
open Single_entry_type
open Branch_anno
open Cond_type_new
open Single_context
open Arch_type
open Single_subtype
open Smt_emitter
open Sexplib.Std

module SingleBlockInvariance = struct
  exception SingleBlockInvarianceError of string

  let single_block_invariance_error msg = raise (SingleBlockInvarianceError ("[Single Block Invariance Error] " ^ msg))

  module ArchType = ArchType (SingleEntryType)

  type br_block_t = {
    label: IsaBasic.label;
    pc: int;
    br_context: SingleContext.t list;
    not_br_context: (SingleContext.t * SingleExp.SingleVarSet.t) list;
    br_var_map: SingleExp.local_var_map_t;
  }
  [@@deriving sexp]

  type block_input_t = IsaBasic.label * (br_block_t list)
  [@@deriving sexp]

  type t = block_input_t list
  [@@deriving sexp]

  let init 
      (input_var_set: SingleExp.SingleVarSet.t)
      (func_type: ArchType.t list) (* I add this so that I can use the most updated ArchType to update context *)
      (block_subtype_list: ArchType.block_subtype_t list)
      (single_subtype: SingleSubtype.t) : t =
    let helper_inner 
        (target_block: ArchType.t)
        (branch_block: ArchType.t) : br_block_t =
      let br_var_map = 
        BranchAnno.get_branch_anno 
          branch_block.reg_type branch_block.mem_type branch_block.local_var_map
          target_block.reg_type target_block.mem_type target_block.useful_var
      in
      Printf.printf "br_var_map branch_block\n%s\ntarget_block\n%s\n" 
        (Sexplib.Sexp.to_string_hum (ArchType.sexp_of_t branch_block))
        (Sexplib.Sexp.to_string_hum (ArchType.sexp_of_t target_block));
      Printf.printf "br_var_map\n%s\n" (Sexplib.Sexp.to_string_hum (SingleExp.sexp_of_local_var_map_t br_var_map));
      let br_var_map = SingleExp.add_local_global_var br_var_map input_var_set in
      (* The following code replace loop counter with there solution (the bound) on exiting the loop.
         The key insight here is that the extra invariance is always about constraining the bound
         instead of the counter itself. *)
      let br_var_map =
        List.map (
          fun (var_idx, exp) ->
            match SingleSubtype.sub_sol_single_to_range_naive_repl false single_subtype input_var_set branch_block.pc exp with
            | Single simp_exp -> var_idx, simp_exp
            | _ -> var_idx, exp
        ) br_var_map
      in

      let get_not_br_info_helper (cond_and_pc: SingleCondType.t * int) : SingleContext.t * SingleExp.SingleVarSet.t =
        let cond, cond_pc = cond_and_pc in
        let vars = SingleCondType.get_vars cond in
        let vars = SingleExp.SingleVarSet.diff vars input_var_set in
        let counter_var_list =
          List.filter_map (
            fun (x: IsaBasic.imm_var_id) ->
              match SingleSubtype.sub_sol_single_to_range_naive_repl false single_subtype input_var_set cond_pc (SingleVar x) with
              | Range _ -> Some x
              | _ -> None
          ) (SingleExp.SingleVarSet.to_list vars)
        in
        Cond (SingleCondType.not_cond_type cond),
        SingleExp.SingleVarSet.of_list counter_var_list
      in

      { label = branch_block.label;
        pc = branch_block.pc;
        br_context = List.map (fun (x, _) -> SingleContext.Cond x) branch_block.branch_hist;
        not_br_context = List.map get_not_br_info_helper branch_block.branch_hist;
        br_var_map = br_var_map;
      }
    in
    let helper_outer
        (target_block: ArchType.t)
        (block_subtype: ArchType.block_subtype_t) : block_input_t =
      let _, branch_block_list = block_subtype in
      target_block.label,
      List.map (helper_inner target_block) branch_block_list
    in
    List.map2 helper_outer func_type block_subtype_list

  let solve_one_pair
      (smt_ctx: SmtEmitter.t)
      (single_subtype: SingleSubtype.t)
      (target_block: ArchType.t)
      (func_type: ArchType.t list)
      (br_block_info: br_block_t) : ArchType.t list =
    let branch_block = ArchType.get_arch_type func_type br_block_info.label in
    Printf.printf "Branch block\n%s\n" (Sexplib.Sexp.to_string_hum (ArchType.sexp_of_t branch_block));
    Printf.printf "Br block info\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_br_block_t br_block_info));
    
    (* 1. Setup context of executing the branch, which consists of
          (1) branch_block.context
          (2) branch_block.tmp_context
          (3) br_block_info.br_context (branch hist at jump)
          We assume the function context is already initialized in smt_ctx.
    *)
    SmtEmitter.push smt_ctx;
    SingleSubtype.update_block_smt_ctx smt_ctx single_subtype branch_block.useful_var;
    ArchType.add_assertions smt_ctx branch_block;
    SingleContext.add_assertions smt_ctx br_block_info.br_context;
    
    (* 2. Get target_block's tmp cxt (invariance) that needs to be satisfied under branch_block's context *)
    (* Note: if target block added extra call var related variables to its tmp context, 
      then the repl_context_var will fail due to missing var in context map when trying to pass the tmp_ctx to its entrance *)
    let invariance_list = 
      List.map (SingleContext.repl (SingleExp.repl_context_var br_block_info.br_var_map)) target_block.tmp_context 
    in
    let new_br_context =
      match SingleContext.check_or_assert smt_ctx invariance_list with
      | None -> 
        SmtEmitter.pp_smt_ctx 0 smt_ctx;
        Printf.printf "Jump from %s %d to %s %d\n" br_block_info.label br_block_info.pc target_block.label target_block.pc;
        Printf.printf "Unsat invariance\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t invariance_list));
        single_block_invariance_error "Unsat invaraince"
      | Some cond_list -> cond_list
    in

    let filter_relevent_branch_cond 
        (x: SingleContext.t) (cond_info: SingleContext.t * SingleExp.SingleVarSet.t) : 
        SingleContext.t option =
      let cond, cond_loop_vars = cond_info in
      let x_vars = SingleContext.get_vars x in
      let x_cond_loop_vars = SingleExp.SingleVarSet.inter x_vars cond_loop_vars in
      if SingleExp.SingleVarSet.is_empty cond_loop_vars then
        (* We keep non-loop cond *)
        Some cond
      else if SingleExp.SingleVarSet.is_empty x_cond_loop_vars then
        (* We remove irrelevant loop cond *)
        None
      else Some cond
    in
    let get_assert_context
        (not_br_info: (SingleContext.t * SingleExp.SingleVarSet.t) list) (x: SingleContext.t) : SingleContext.t =
      let relevent_not_br = List.filter_map (filter_relevent_branch_cond x) not_br_info in
      SingleContext.ctx_or (SingleContext.Or relevent_not_br) x
    in
    let assert_br_context = List.map (get_assert_context br_block_info.not_br_context) new_br_context in
    
    (* let assert_br_context = List.map (fun (x: SingleContext.t) -> SingleContext.Or (x :: (List.map fst br_block_info.not_br_context))) new_br_context in *)

    SmtEmitter.pop smt_ctx 1;

    (* 3. Update the branch_block's tmp context (invariance to be resolved) *)
    if List.length new_br_context = 0 then func_type
    else
      if target_block.label = branch_block.label then begin
        Printf.printf "new_br_context\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t new_br_context));
        Printf.printf "target tmp context %d branch tmp context %d\n" (List.length target_block.tmp_context) (List.length branch_block.tmp_context);
        single_block_invariance_error (Printf.sprintf "Unsat invariance when jump from %s %d to %s\n" branch_block.label branch_block.pc target_block.label)
      end else
        ArchType.set_arch_type func_type
          { branch_block with tmp_context = assert_br_context @ branch_block.tmp_context }

  let solve_one_block
      (smt_ctx: SmtEmitter.t)
      (allow_add_new_input_context: bool)
      (input_var_set: SingleExp.SingleVarSet.t)
      (single_subtype: SingleSubtype.t)
      (func_type: ArchType.t list)
      (block_input: block_input_t) : ArchType.t list =
    let target_label, br_block_list = block_input in
    let target_block = ArchType.get_arch_type func_type target_label in
    Printf.printf "Solve Block %s%!\n" target_label;
    if List.length target_block.tmp_context = 0 then begin
      Printf.printf "Block %s resolved, skip\n" target_label;
      func_type
    (* end else if IsaBasic.is_label_function_entry target_block.label then begin
      Printf.printf "Func entry %s, skip\n" target_label;
      func_type *)
    end else begin
      if IsaBasic.is_label_function_entry target_block.label then begin
        (if List.length br_block_list > 0 then
          single_block_invariance_error "Function input block has other input\n"
        else
          Printf.printf "Func entry %s, add tmp_context to context tmp_context\n%s\n" target_label (Sexplib.Sexp.to_string_hum (sexp_of_list SingleContext.sexp_of_t target_block.tmp_context)));
        if allow_add_new_input_context then
          let resolved, not_resolved =
            List.partition (
              fun (x: SingleContext.t) ->
                SingleContext.is_val (SingleExp.is_val input_var_set) x
            ) target_block.tmp_context
          in
          ArchType.set_arch_type func_type
          { target_block with 
            context = target_block.context @ resolved;
            tmp_context = not_resolved }
        else func_type
      end else begin
        Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (ArchType.sexp_of_t target_block));
        let func_type = List.fold_left (solve_one_pair smt_ctx single_subtype target_block) func_type br_block_list in
        Printf.printf "After resolve for block %s\n" target_label;
        (* Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_t func_type)); *)
        ArchType.set_arch_type func_type
        { target_block with 
          context = target_block.context @ target_block.tmp_context;
          tmp_context = [] }
      end
    end

  let is_tmp_resolved (func_type: ArchType.t list) : bool =
    List.fold_left (
      fun (acc: bool) (arch_type: ArchType.t) ->
        acc && (List.is_empty arch_type.tmp_context)
    ) true func_type

  let rec solve_iter
      (smt_ctx: SmtEmitter.t)
      (allow_add_new_input_context: bool)
      (input_var_set: SingleExp.SingleVarSet.t)
      (func_type: ArchType.t list) (block_input_list: t)
      (single_subtype: SingleSubtype.t)
      (iter: int) : ArchType.t list * bool =
    Printf.printf "Single block invariance solve iter %d\n" iter;
    let resolved = is_tmp_resolved func_type in
    if iter = 0 || resolved then func_type, resolved
    else
    let func_type = List.fold_left (solve_one_block smt_ctx allow_add_new_input_context input_var_set single_subtype) func_type block_input_list in
    solve_iter smt_ctx allow_add_new_input_context input_var_set func_type block_input_list single_subtype (iter - 1)

  let solve
      (smt_ctx: SmtEmitter.t)
      (allow_add_new_input_context: bool)
      (input_var_set: SingleExp.SingleVarSet.t)
      (func_type: ArchType.t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (single_subtype: SingleSubtype.t)
      (iter: int) : ArchType.t list * bool =
    let block_input_list = init input_var_set func_type block_subtype_list single_subtype in
    Printf.printf "Br Context\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t block_input_list));
    solve_iter smt_ctx allow_add_new_input_context input_var_set func_type block_input_list single_subtype iter

  (* TODO: We may need to use tighter/weaker invariance when adding tmp_context to context.
          Currently I did not take branch history into consideration. *)

end

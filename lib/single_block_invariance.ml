open Isa_basic
open Single_exp
open Single_entry_type
open Branch_anno_type
open Single_context
open Arch_type
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
      (block_subtype_list: ArchType.block_subtype_t list) : t =
    let helper_inner 
        (target_block: ArchType.t)
        (branch_block: ArchType.t) : br_block_t =
      let br_var_map = 
        BranchAnno.get_branch_anno 
          branch_block.reg_type branch_block.mem_type branch_block.local_var_map
          target_block.reg_type target_block.mem_type target_block.useful_var
      in
      let br_var_map = SingleExp.add_local_global_var br_var_map input_var_set in
      { label = branch_block.label;
        pc = branch_block.pc;
        br_context = List.map (fun (x, _) -> SingleContext.Cond x) branch_block.branch_hist;
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
      (target_block: ArchType.t)
      (func_type: ArchType.t list)
      (br_block_info: br_block_t) : ArchType.t list =
    let branch_block = ArchType.get_arch_type func_type br_block_info.label in
    
    (* 1. Setup context of executing the branch, which consists of
          (1) branch_block.context
          (2) branch_block.tmp_context
          (3) br_block_info.br_context (branch hist at jump)
          We assume the function context is already initialized in smt_ctx.
    *)
    SmtEmitter.push smt_ctx;
    SingleContext.add_assertions smt_ctx branch_block.context;
    SingleContext.add_assertions smt_ctx branch_block.tmp_context;
    SingleContext.add_assertions smt_ctx br_block_info.br_context;
    
    (* 2. Get target_block's tmp cxt (invariance) that needs to be satisfied under branch_block's context *)
    let invariance_list = 
      List.map (SingleContext.repl (SingleExp.repl_context_var br_block_info.br_var_map)) target_block.tmp_context 
    in
    let new_br_context =
      match SingleContext.check_or_assert smt_ctx invariance_list with
      | None -> 
        SmtEmitter.pp_smt_ctx 0 smt_ctx;
        Printf.printf "Unsat invariance\n%s\n" (Sexplib.Sexp.to_string (sexp_of_list SingleContext.sexp_of_t invariance_list));
        single_block_invariance_error "Unsat invaraince"
      | Some cond_list -> cond_list
    in

    SmtEmitter.pop smt_ctx 1;

    (* 3. Update the branch_block's tmp context (invariance to be resolved) *)
    if List.length new_br_context = 0 then func_type
    else
      if target_block.label = branch_block.label then
      single_block_invariance_error (Printf.sprintf "Unsat invaraince when jump from %s %d to %s\n" branch_block.label branch_block.pc target_block.label)
      else
        ArchType.set_arch_type func_type
          { branch_block with tmp_context = new_br_context @ branch_block.tmp_context }

  let solve_one_block
      (smt_ctx: SmtEmitter.t)
      (func_type: ArchType.t list)
      (block_input: block_input_t) : ArchType.t list =
    let target_label, br_block_list = block_input in
    let target_block = ArchType.get_arch_type func_type target_label in
    if List.length target_block.tmp_context = 0 then begin
      Printf.printf "Block %s resolved, skip\n" target_label;
      func_type
    end else begin
      let func_type = List.fold_left (solve_one_pair smt_ctx target_block) func_type br_block_list in
      Printf.printf "After resolve for block %s\n" target_label;
      Printf.printf "%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list ArchType.sexp_of_t func_type));
      ArchType.set_arch_type func_type
        { target_block with tmp_context = [] }
    end

  let is_tmp_resolved (func_type: ArchType.t list) : bool =
    List.fold_left (
      fun (acc: bool) (arch_type: ArchType.t) ->
        acc && (List.is_empty arch_type.tmp_context)
    ) true func_type

  let rec solve_iter
      (smt_ctx: SmtEmitter.t)
      (func_type: ArchType.t list) (block_input_list: t)
      (iter: int) : ArchType.t list =
    Printf.printf "Single block invariance solve iter %d\n" iter;
    if iter = 0 || is_tmp_resolved func_type then func_type
    else
    let func_type = List.fold_left (solve_one_block smt_ctx) func_type block_input_list in
    solve_iter smt_ctx func_type block_input_list (iter - 1)

  let solve
      (smt_ctx: SmtEmitter.t)
      (input_var_set: SingleExp.SingleVarSet.t)
      (func_type: ArchType.t list)
      (block_subtype_list: ArchType.block_subtype_t list)
      (iter: int) : ArchType.t list =
    let block_input_list = init input_var_set func_type block_subtype_list in
    Printf.printf "Br Context\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t block_input_list));
    solve_iter smt_ctx func_type block_input_list iter

  (* TODO: Reuse code from single_input_var_cond_subtype.ml *)
  (* We may only need to remove local vars, check whether the cond hist contains local var or not. 
    We can keep block var. This is different from single_input_var_cond_subtype. *)

end

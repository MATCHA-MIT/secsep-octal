open Isa_basic
open Single_exp
open Single_entry_type
open Cond_type_new
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

  let init (block_subtype_list: ArchType.block_subtype_t list) : (ArchType.t list) * t =
    let convert_helper (cond: ArchType.CondType.t) : SingleCondType.t =
      let c, l, r = cond in
      match c with
      | Eq -> (Eq, l, r)
      | Ne -> (Ne, l, r)
      | Le -> (Le, l, r)
      | Lt -> (Lt, l, r)
      | Be -> (Be, l, r)
      | Bt -> (Bt, l, r)
    in
    let helper_inner 
        (target_block: ArchType.t)
        (branch_block: ArchType.t) : br_block_t =
      { label = branch_block.label;
        pc = branch_block.pc;
        br_context = List.map (fun (x, _) -> SingleContext.Cond (convert_helper x)) branch_block.branch_hist;
        br_var_map = 
          BranchAnno.get_branch_anno 
          branch_block.reg_type branch_block.mem_type branch_block.local_var_map
          target_block.reg_type target_block.mem_type target_block.useful_var;
      }
    in
    let helper_outer
        (block_subtype: ArchType.block_subtype_t) : ArchType.t * block_input_t =
      let target_block, branch_block_list = block_subtype in
      target_block,
      (target_block.label,
      List.map (helper_inner target_block) branch_block_list)
    in
    List.split (List.map helper_outer block_subtype_list)

  (* let solve_one_pair
      (smt_ctx: Smt)
      (target_label: IsaBasic.label)
      (func_type: ArchType.t list)
      (br_block_info: br_block_t) : ArchType.t list =
    let target_block = ArchType.get_arch_type func_type target_label in
    let branch_block = ArchType.get_arch_type func_type br_block_info.label in *)


  (* TODO: Reuse code from single_input_var_cond_subtype.ml *)
  (* We may only need to remove local vars, check whether the cond hist contains local var or not. 
    We can keep block var. This is different from single_input_var_cond_subtype. *)

end

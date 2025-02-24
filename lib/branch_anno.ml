open Single_exp
open Single_entry_type
open Reg_type_new
open Mem_type_new
open Sexplib.Std

module BranchAnno = struct
  exception BranchAnnoError of string
  let branch_anno_error msg = raise (BranchAnnoError ("[Branch Annotation Error] " ^ msg))

  type t = SingleExp.local_var_map_t option
  [@@deriving sexp]

  module RegType = RegType (SingleEntryType)
  module MemType = MemType (SingleEntryType)

  let add_var_map
      (branch_local_var_map: SingleExp.local_var_map_t)
      (target_useful_var: SingleExp.SingleVarSet.t)
      (context_var_map: SingleExp.local_var_map_t)
      (branch_type: SingleExp.t) (target_type: SingleExp.t) :
      SingleExp.local_var_map_t =
    if SingleExp.is_val target_useful_var target_type then
      SingleExp.add_local_var_simp (SingleExp.repl_local_var branch_local_var_map) context_var_map target_type branch_type
    else
      SingleExp.add_local_var context_var_map target_type SingleTop

  let get_branch_anno
      (branch_reg_type: RegType.t) (branch_mem_type: MemType.t)
      (branch_local_var_map: SingleExp.local_var_map_t)
      (target_reg_type: RegType.t) (target_mem_type: MemType.t)
      (target_useful_var: SingleExp.SingleVarSet.t) :
      SingleExp.local_var_map_t =
    (* NOTE: We assume that target block only uses block local vars or SingleTop to represent their reg/mem slot type. *)
    let helper = add_var_map branch_local_var_map target_useful_var in
    let var_map = List.fold_left2 helper [] branch_reg_type target_reg_type in
    MemType.fold_left2 helper var_map branch_mem_type target_mem_type

end

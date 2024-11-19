open Single_entry_type
open Cond_type_new
open Arch_type

module SingleInputVarCondSubtype = struct
  exception SingleInputVarCondSubtypeError of string
  let single_input_var_cond_subtype_error msg = raise (SingleInputVarCondSubtypeError ("[Single Input Var Cond Subtype Error] " ^ msg))

  module CondType = CondType (SingleEntryType)
  module ArchType = ArchType (SingleEntryType)

  type pc_cond_t = int * (CondType.t list)
  type pc_cond_subtype_t = int * (pc_cond_t list)

  type t = pc_cond_subtype_t list

  let get_not_taken_branch_hist
      (sub_single_helper: SingleEntryType.t -> SingleEntryType.t option)
      (block_subtype_list: ArchType.block_subtype_t list) : (int * ((CondType.t * (CondType.t option)) list)) list =
    let sub_cond_helper (cond_pc: CondType.t * int) : CondType.t * CondType.t option =
      let cond, _ = cond_pc in
      let c, l, r = cond in
      match sub_single_helper l with
      | None -> cond, None
      | Some simp_l ->
        begin match sub_single_helper r with
        | None -> cond, None
        | Some simp_r -> cond, Some (c, simp_l, simp_r)
        end
    in
    List.map (
      fun (entry: ArchType.block_subtype_t) ->
        let block, _ = entry in
        block.pc,
        List.map sub_cond_helper block.full_not_taken_hist
    ) block_subtype_list

  let get_simp_branch_hist_from_full_not_taken_hist
      (full_not_taken_hist_list: (int * ((CondType.t * (CondType.t option)) list)) list)
      (arch_type: ArchType.t) : pc_cond_t =
    let rec helper 
        (simp_hist: CondType.t list)
        (branch_hist: CondType.t list)
        (full_not_taken_hist: (CondType.t * (CondType.t option)) list) : 
        CondType.t list =
      match branch_hist, full_not_taken_hist with
      | [], _ -> simp_hist
      | br_hd :: tl, (orig, Some simp) :: not_taken_tl ->
        if CondType.cmp br_hd orig then
          helper (simp :: simp_hist) tl not_taken_tl
        else if CondType.cmp br_hd (CondType.not_cond_type orig) then
          helper ((CondType.not_cond_type simp) :: simp_hist) tl not_taken_tl
        else single_input_var_cond_subtype_error "Branch hist does not match not taken hist"
      | _ :: tl, (_, None) :: not_taken_tl ->
        helper simp_hist tl not_taken_tl
      | _, [] -> single_input_var_cond_subtype_error "full_not_taken_hist is shorter than branch hist"
    in
    let full_not_taken_hist_opt =
      List.find_map (
        fun (pc, hist) -> if pc = arch_type.pc then Some hist else None
      ) full_not_taken_hist_list
    in
    match full_not_taken_hist_opt with
    | None -> single_input_var_cond_subtype_error (Printf.sprintf "Cannot find block %s\n" arch_type.label)
    | Some full_not_taken_hist ->
      arch_type.pc,
      helper [] (List.rev (List.map fst arch_type.branch_hist)) (List.rev full_not_taken_hist)

  let init_input_var_cond_subtype
      (sub_single_helper: SingleEntryType.t -> SingleEntryType.t option)
      (block_subtype_list: ArchType.block_subtype_t list) : t =
    let full_not_taken_branch_hist_list = get_not_taken_branch_hist sub_single_helper block_subtype_list in
    List.map (
      fun (entry: ArchType.block_subtype_t) ->
        let block, sub_block_list = entry in
        block.pc,
        List.map (get_simp_branch_hist_from_full_not_taken_hist full_not_taken_branch_hist_list) sub_block_list
    ) block_subtype_list
        
end

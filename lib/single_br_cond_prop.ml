open Single_exp_basic
open Single_entry_type
open Cond_type_new
open Single_context
open Range_exp
open Arch_type
open Arch_context_map
open Set_sexp
open Sexplib.Std

module SingleBrCondProp = struct
  exception SingleBrCondPropError of string
  let single_br_cond_prop_error msg = raise (SingleBrCondPropError ("[Single Br Cond Prop Error] " ^ msg))

  module CondType = SingleCondType
  module ArchType = ArchType (SingleEntryType)

  module CondSet = SingleCondSet

  module LabelCondMap = StrMapSexp(CondSet)

  type sol_map_t = LabelCondMap.t
  [@@deriving sexp]

  type sub_cond_t = string * int * CondSet.t (* sub label, pc, cond set *)
  [@@deriving sexp]

  type sub_cond_map_t = string * (sub_cond_t list) (* sup label, sub_cond list *)
  [@@deriving sexp]

  type type_pc_t = SingleEntryType.t * int
  [@@deriving sexp]

  module SimpCache = Map.Make(
    struct
      type t = type_pc_t
      let compare (e1, pc1) (e2, pc2) =
        let cmp_pc = Int.compare pc1 pc2 in
        if cmp_pc = 0 then
          SingleEntryType.cmp e1 e2
        else cmp_pc
    end
  )

  type simp_cache_t = SingleEntryType.t SimpCache.t

  let build_simp_cache
      (sub_single_helper: type_pc_t -> RangeExp.t option)
      (block_subtype_list: ArchType.block_subtype_t list) : simp_cache_t =
    let simp_one_exp (acc: simp_cache_t) (e: SingleEntryType.t) (pc: int) : simp_cache_t =
      match sub_single_helper (e, pc - 1) with (* very important!!! we use cond_pc - 1 for sub sol since we do not want to eval with taken cond!!! *)
      | Some (Single simp_e) ->
        if SingleEntryType.cmp e simp_e <> 0 then 
          SimpCache.add (e, pc) simp_e acc
        else acc
      | _ -> acc
    in
    let simp_one_cond (acc: simp_cache_t) (cond_pc: CondType.t * int) : simp_cache_t =
      let (_, el, er), pc = cond_pc in
      let acc = simp_one_exp acc el pc in
      simp_one_exp acc er pc
    in
    let helper (acc: simp_cache_t) (block_subtype: ArchType.block_subtype_t) : simp_cache_t =
      List.fold_left simp_one_cond acc (fst block_subtype).full_not_taken_hist
    in
    List.fold_left helper SimpCache.empty block_subtype_list

  let reverse_map_exp
      (reverse_context_mapped_var_set: IntSet.t)
      (reverse_context_map: SingleEntryType.local_var_map_t) 
      (e: SingleEntryType.t) : SingleEntryType.t option =
    if SingleEntryType.is_val reverse_context_mapped_var_set e then
      let trans_e = SingleEntryType.repl_var reverse_context_map e in
      if trans_e = SingleTop then None
      else Some trans_e
    else None

  let try_simp_cond
      (simp_helper: SingleEntryType.t -> SingleEntryType.t option)
      (cond_exp: CondType.t) : CondType.t option =
    let cond, el, er = cond_exp in
    match simp_helper el with
    | None -> None
    | Some mapped_el ->
      begin match simp_helper er with
      | None -> None
      | Some mapped_er -> Some (cond, mapped_el, mapped_er)
      end

  let init
      (sub_single_helper: type_pc_t -> RangeExp.t option)
      (input_var_set: IntSet.t)
      (ctx_map_map: ArchContextMap.t)
      (block_subtype_list: ArchType.block_subtype_t list) : sub_cond_map_t list =
    let simp_cache = build_simp_cache sub_single_helper block_subtype_list in
    let map_one_exp 
        (reverse_context_mapped_var_set: IntSet.t)
        (reverse_context_map: SingleEntryType.local_var_map_t) 
        (pc: int) (e: SingleEntryType.t) : SingleEntryType.t option =
      let simp_e =
        match SimpCache.find_opt (e, pc) simp_cache with (* very important!!! we use cond_pc, same as what we did when build simp_cache *)
        | Some simp_e -> simp_e
        | None -> e
      in
      reverse_map_exp reverse_context_mapped_var_set reverse_context_map simp_e
    in
    let map_one_cond 
        (reverse_context_mapped_var_set: IntSet.t)
        (reverse_context_map: SingleEntryType.local_var_map_t) 
        (cond_pc: CondType.t * int) : CondType.t option =
      let cond_exp, pc = cond_pc in
      let map_helper = map_one_exp reverse_context_mapped_var_set reverse_context_map pc in
      try_simp_cond map_helper cond_exp
    in
    let gen_one_sub_block (sub_block: ArchType.t) : sub_cond_t =
      let reverse_context_map = ArchContextMap.get_reverse_map ctx_map_map sub_block.pc in
      let reverse_context_mapped_var_set = SingleEntryType.get_mapped_var_set reverse_context_map |> IntSet.union input_var_set in
      sub_block.label, sub_block.pc,
      List.filter_map (map_one_cond reverse_context_mapped_var_set reverse_context_map) sub_block.branch_hist |> CondSet.of_list
    in
    let gen_one_block_subtype (block_subtype: ArchType.block_subtype_t) : sub_cond_map_t =
      let sup_block, sub_block_list = block_subtype in
      sup_block.label,
      List.map gen_one_sub_block sub_block_list
    in
    List.map gen_one_block_subtype block_subtype_list

  let solve_one_block
      (to_solve_set: StringSet.t)
      (block_sub: sub_cond_map_t) :
      ((string * CondSet.t) option)* sub_cond_map_t =
    let label, sub_label_cond_map = block_sub in
    if not (StringSet.mem label to_solve_set) then None, block_sub else
    let new_sol_set =
      match sub_label_cond_map with
      | [] -> CondSet.empty
      | (_, _, hd) :: tl ->
        List.fold_left (
          fun (acc: CondSet.t) (_, _, sub_cond_set) ->
            CondSet.inter sub_cond_set acc
        ) hd tl
    in
    if CondSet.is_empty new_sol_set then None, block_sub else
    let new_sub_cond_list =
      List.map (
        fun (label, pc, sub_cond_set) -> 
          label, pc, CondSet.diff sub_cond_set new_sol_set
      ) sub_label_cond_map
    in
    Some (label, new_sol_set), (label, new_sub_cond_list)

  let solve_one_iter
      (input_var_set: IntSet.t)
      (ctx_map_map: ArchContextMap.t)
      (block_sol: LabelCondMap.t) 
      (block_sub_map: sub_cond_map_t list)
      (block_to_solve_set: StringSet.t) :
      LabelCondMap.t * (sub_cond_map_t list) * StringSet.t =
    let sol_list_opt, updated_block_sub_list =
      List.map (solve_one_block block_to_solve_set) block_sub_map
      |> List.split
    in
    let sol_list = List.filter_map (fun x -> x) sol_list_opt in
    let block_sol, new_sol =
      List.fold_left_map (
        fun (acc: LabelCondMap.t) (label, label_sol) ->
          match LabelCondMap.find_opt label acc with
          | Some known_sol ->
            let new_sol = CondSet.diff label_sol known_sol in
            LabelCondMap.update label (Option.map (CondSet.union label_sol)) acc,
            (label, new_sol)
          | None ->
            LabelCondMap.add label label_sol acc,
            (label, label_sol)
      ) block_sol sol_list
    in
    let new_sol = List.filter (fun (_, sol_set) -> not (CondSet.is_empty sol_set)) new_sol |> LabelCondMap.of_list in
    let update_sub_cond (acc: bool) (sub_cond: sub_cond_t) : bool * sub_cond_t =
      let sub_label, sub_pc, orig_sub_cond = sub_cond in
      match LabelCondMap.find_opt sub_label new_sol with
      | Some new_sub_cond ->
        let reverse_context_map = ArchContextMap.get_reverse_map ctx_map_map sub_pc in
        let reverse_context_mapped_var_set = SingleEntryType.get_mapped_var_set reverse_context_map |> IntSet.union input_var_set in
        let new_sub_cond = CondSet.filter_map (try_simp_cond (reverse_map_exp reverse_context_mapped_var_set reverse_context_map)) new_sub_cond in
        true, (sub_label, sub_pc, CondSet.union orig_sub_cond new_sub_cond)
      | None ->
        acc, sub_cond
    in
    let update_sub_cond_list
        (acc: StringSet.t) (label, sub_cond_list) : StringSet.t * (string * (sub_cond_t list)) =
      let update_sub, update_sub_cond_list =
        List.fold_left_map update_sub_cond false sub_cond_list
      in
      let acc = if update_sub then StringSet.add label acc else acc in
      acc, (label, update_sub_cond_list)
    in
    let update_label_set, updated_block_sub_list = List.fold_left_map update_sub_cond_list StringSet.empty updated_block_sub_list in
    block_sol, updated_block_sub_list, update_label_set
    
  let solve
      (sub_single_helper: type_pc_t -> RangeExp.t option)
      (input_var_set: IntSet.t)
      (ctx_map_map: ArchContextMap.t)
      (block_subtype_list: ArchType.block_subtype_t list)
      (func_type: ArchType.t list) : ArchType.t list =
    let block_sub_map = init sub_single_helper input_var_set ctx_map_map block_subtype_list in

    Printf.printf "single_br_cond_prop block_sub_map\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_list sexp_of_sub_cond_map_t block_sub_map));

    let rec helper 
        (sol: LabelCondMap.t) 
        (sub_map: sub_cond_map_t list)
        (to_solve_set: StringSet.t) : LabelCondMap.t =
      if StringSet.is_empty to_solve_set then sol
      else
        let sol, sub_map, to_solve_set = solve_one_iter input_var_set ctx_map_map sol sub_map to_solve_set in
        helper sol sub_map to_solve_set
    in
    let sol = helper LabelCondMap.empty block_sub_map (List.split block_sub_map |> fst |> StringSet.of_list) in

    Printf.printf "single_br_cond_prop sol\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_sol_map_t sol));

    let to_context_helper (cond: CondType.t) : SingleContext.t = Cond cond in
    List.map (
      fun (a_type: ArchType.t) ->
        match LabelCondMap.find_opt a_type.label sol with
        | Some cond_set ->
          { a_type with 
            blk_br_context = 
              (CondSet.to_list cond_set |> List.map to_context_helper) @ a_type.blk_br_context 
          }
        | None -> a_type
    ) func_type

end

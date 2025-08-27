open Single_entry_type
open Mem_offset_new
open Mem_type_new
open Arch_type
open Set_sexp

module RangeSolTemplate = struct
  exception RangeSolTemplateError of string

  let range_sol_template_error msg = raise (RangeSolTemplateError ("[Range Sol Template Error] " ^ msg))

  module ArchType = ArchType (SingleEntryType)

  module RangeSolMap = IntMapSexp (MemOffset)

  type t = RangeSolMap.t
  [@@deriving sexp]

  let get_range_context_map_var
      (in_type: ArchType.t) : IntSet.t =
    (* Insight: in the template, we want to keep offset boundary var fixed, not changed across blocks.
        On the other hand, we want to assocaite other vars in the in_type's valid region with its syntaical meaning,
        i.e., which reg/mem slot this var is referrring to,
        thereby replaced to corresponding block var when generate sol template for other blocks.
        This will help us to derive invariants (relation between reg/mem slots and valid regions).
        e.g., sha512, poly1305 where valid region of buf is [0, buf_used] *)
    let helper 
        (acc: IntSet.t * IntSet.t) (mem_slot: 'a MemTypeBasic.mem_slot) : IntSet.t * IntSet.t =
      let boundary_var, range_var = acc in
      let off, range, _ = mem_slot in
      IntSet.union boundary_var (MemOffset.get_vars off),
      IntSet.union range_var (MemRange.get_vars range)
    in
    let boundary_var, range_var =
      MemTypeBasic.fold_left_full helper (IntSet.empty, IntSet.empty) in_type.mem_type
    in
    IntSet.diff range_var boundary_var

  let get_ctx_map
      (in_range_var_set: IntSet.t)
      (in_type: ArchType.t) (a_type: ArchType.t) : SingleEntryType.local_var_map_t =
    let helper
        (acc: SingleEntryType.local_var_map_t)
        (in_type: SingleEntryType.t) (a_type: SingleEntryType.t) : SingleEntryType.local_var_map_t =
      if SingleEntryType.is_val in_range_var_set in_type then
        SingleEntryType.add_local_var_simp (fun x -> x) acc in_type a_type
      else acc
    in
    let var_map = ArchType.RegType.fold_left2 helper [] in_type.reg_type a_type.reg_type in
    MemTypeBasic.fold_left2 helper var_map in_type.mem_type a_type.mem_type

  let add_range_sol_template
      (in_range_var_set: IntSet.t) (in_type: ArchType.t) 
      (acc_sol_template: t) (a_type: ArchType.t) : t =
    let ctx_map =
      get_ctx_map in_range_var_set in_type a_type
    in
    Printf.printf "ctx_map for %s\n%s\n" a_type.label (Sexplib.Sexp.to_string_hum (SingleEntryType.sexp_of_local_var_map_t ctx_map));
    let helper 
        (acc: t) (in_slot: 'a MemTypeBasic.mem_slot) 
        (block_slot: 'a MemTypeBasic.mem_slot) : t =
      let _, in_range, _ = in_slot in
      let _, block_range, _ = block_slot in
      match in_range, block_range with
      (* We generate sol template if in_range satisfies the following two requirements:
          1. in_range is not empty
          2. in_range is not full - since we only assign range var to 
          following block's valid region when in_range is not full,
          so this condition is satified if block_range is a range var *)
      | RangeConst [ in_range_off ], RangeVar r ->
        RangeSolMap.add r (MemOffset.repl_var ctx_map in_range_off) acc
      | _ -> acc
    in
    MemTypeBasic.fold_left2_full helper acc_sol_template in_type.mem_type a_type.mem_type

  let init
      (func_type: ArchType.t list) : t =
    match func_type with
    | [] -> RangeSolMap.empty
    | in_type :: tl ->
      let in_range_var_set = get_range_context_map_var in_type in
      Printf.printf "in_range_var_set\n%s\n" (Sexplib.Sexp.to_string_hum (IntSet.sexp_of_t in_range_var_set));
      List.fold_left (add_range_sol_template in_range_var_set in_type) RangeSolMap.empty tl

  let get_sol
      (sol_template_map: t) (range_var: int) : MemOffset.t option =
    RangeSolMap.find_opt range_var sol_template_map

end

open Isa_basic
open Single_entry_type
open Mem_offset_new
open Arch_type
open Pretty_print
open Smt_emitter

module RangeSubtype = struct
  exception RangeSubtypeError of string

  let range_subtype_error msg = raise (RangeSubtypeError ("[Range Subtype Error] " ^ msg))

  type var_idx_t = MemRange.range_var_id * int
  type type_exp_t = MemRange.t * int

  type type_rel = {
    var_idx: var_idx_t;
    sol: MemRange.t option;
    off: MemOffset.t;
    subtype_list: type_exp_t list;
    (* supertype_list: MemRange.range_var_id list; *)
  }

  type t = type_rel list

  module ArchType = ArchType (SingleEntryType)

  let type_exp_to_string (x: type_exp_t) : string =
    let exp, pc = x in
    Printf.sprintf "%s(%d)" (MemRange.to_string exp) (pc)

  let sol_to_string (x: MemRange.t option) : string =
    match x with
    | Some e -> Printf.sprintf "Some %s" (MemRange.to_string e)
    | None -> "None"

  let pp_type_rel (lvl: int) (x: type_rel) =
    let var_idx, var_pc = x.var_idx in
    PP.print_lvl lvl "<RangeVar %d> at pc %d\n"  var_idx var_pc;
    PP.print_lvl (lvl + 1) "Sol: %s\n" (sol_to_string x.sol);
    PP.print_lvl (lvl + 1) "Off: %s\n" (MemOffset.to_string x.off);
    PP.print_lvl (lvl + 1) "Subtype: [\n";
    List.iter (fun sub -> PP.print_lvl (lvl + 2) "%s;\n" (type_exp_to_string sub)) x.subtype_list;
    PP.print_lvl (lvl + 1) "]\n"

  let pp_range_subtype (lvl: int) (tv_rels: t) =
    List.iter (fun x -> pp_type_rel lvl x) tv_rels

  let add_range_subtype_helper
      (tv_rel_list: t) (off: MemOffset.t) (sub: type_exp_t) (sup: var_idx_t) : t =
    let helper (acc: bool) (tv_rel: type_rel) : bool * type_rel =
      if acc then true, tv_rel
      else if tv_rel.var_idx = sup then
        true, { tv_rel with subtype_list = sub :: tv_rel.subtype_list }
      else false, tv_rel
    in
    let found, tv_rel_list = List.fold_left_map helper false tv_rel_list in
    if found then tv_rel_list
    else
      let new_tv_rel: type_rel = {
        var_idx = sup;
        sol = None;
        off = off;
        subtype_list = [ sub ]
      } in
      new_tv_rel :: tv_rel_list

  let add_range_subtype
      (tv_rel_list: t) (off: MemOffset.t) (sub: type_exp_t) (sup: type_exp_t) : t =
    match sup with
    | RangeConst _, _ -> tv_rel_list
    | RangeExp _, _ -> 
      range_subtype_error (Printf.sprintf "add_range_subtype cannot add %s -> %s" (type_exp_to_string sub) (type_exp_to_string sup))
    | RangeVar sup_idx, sup_pc ->
      add_range_subtype_helper tv_rel_list off sub (sup_idx, sup_pc)

  let get_one_block_subtype
      (tv_rel_list: t)
      (block_subtype: ArchType.block_subtype_t) : t =
    let sup_block, sub_block_list = block_subtype in
    let helper
        (acc: t) (sub_block: ArchType.t) : t =
      List.fold_left2 (
        fun (acc: t) (_, sub_part_mem) (_, sup_part_mem) ->
          List.fold_left2 (
            fun (acc: t) (_, sub_range, _) (off, sup_range, _) ->
              add_range_subtype acc off (sub_range, sub_block.pc) (sup_range, sup_block.pc)
          ) acc sub_part_mem sup_part_mem
      ) acc sub_block.mem_type sup_block.mem_type
    in
    List.fold_left helper tv_rel_list sub_block_list
  
  let get_one_block_subtype_faster
      (block_subtype: ArchType.block_subtype_t) : t =
    let sup_block, sub_block_list = block_subtype in
    let mem_subtype =
      ArchType.MemType.map_full (
        fun (off, range, _) -> off, range, ((range, sup_block.pc), [])
      ) sup_block.mem_type
    in
    let helper
        (acc: ('a * type_exp_t list) ArchType.MemType.mem_content)
        (sub_block: ArchType.t) :
        ('a * type_exp_t list) ArchType.MemType.mem_content =
      ArchType.MemType.map2_full (
        fun (off, range, (sup, sub_list)) (_, sub_range, _) ->
          off, range, (sup, (sub_range, sub_block.pc) :: sub_list)
      ) acc sub_block.mem_type
    in
    let mem_subtype = List.fold_left helper mem_subtype sub_block_list in
    ArchType.MemType.fold_left_full (
      fun (acc: t) (entry: MemOffset.t * MemRange.t * (type_exp_t * (type_exp_t list))) ->
        let off, _, (sup, sub_list) = entry in
        match sup with
        | RangeConst _, _ -> acc
        | RangeExp _, _ -> 
          range_subtype_error (Printf.sprintf "add_range_subtype cannot add sup %s" (type_exp_to_string sup))
        | RangeVar sup_idx, sup_pc ->
          { var_idx = (sup_idx, sup_pc); sol = None; off = off; subtype_list = sub_list } :: acc
    ) [] mem_subtype

  
  let simplify_subtype_range
      (repl_helper: (SingleEntryType.t * int) -> SingleEntryType.t)
      (tv_rel_list: t) : t =
    let map_helper = fun (x, y) -> MemRange.repl repl_helper y x, y in
    List.map (
      fun (x: type_rel) ->
        { x with subtype_list = List.map map_helper x.subtype_list }
    ) tv_rel_list

  let get_range_constraint
      (block_subtype: ArchType.block_subtype_t list) : t =
    let tv_rel_list_list = List.map get_one_block_subtype_faster block_subtype in
    List.flatten tv_rel_list_list

  let filter_self_subtype (tv_rel: type_rel) : type_rel =
    let var_idx, _ = tv_rel.var_idx in
    { tv_rel with
      subtype_list = 
        List.filter (
          fun (x, _) -> 
            match x with
            | MemRange.RangeVar v -> v != var_idx
            | _ -> true
        ) tv_rel.subtype_list
    }

  let try_solve_full (tv_rel: type_rel) : MemRange.t option =
    let find_not_full =
      List.find_opt (
        fun (x, _) ->
          match x with
          | MemRange.RangeConst [ off ]
          | MemRange.RangeExp (_, [ off ]) ->
            MemOffset.cmp off tv_rel.off <> 0
          | _ -> true
      ) tv_rel.subtype_list
    in
    if List.length tv_rel.subtype_list = 0 || find_not_full <> None then None
    else Some (RangeConst [ tv_rel.off ])

  let try_solve_empty 
      (range_is_val: MemRange.t -> bool) 
      (tv_rel: type_rel) : MemRange.t option =
    let find_empty =
      List.find_opt (
        fun (x, _) ->
          match x with
          | MemRange.RangeConst [] -> true
          | _ -> false
      ) tv_rel.subtype_list
    in
    if find_empty <> None then
      let find_not_val =
        List.find_opt (
          fun (x, _) -> not (range_is_val x)
        ) tv_rel.subtype_list
      in
      if find_not_val <> None then None
      else Some (RangeConst [])
    else None

  (* let try_solve_non_val
      (get_block_var: MemRange.t -> SingleEntryType.SingleVarSet.t)
      (find_type_pair: int -> int -> IsaBasic.imm_var_id -> SingleEntryType.t * (int -> SingleEntryType.t))
      (tv_rel: type_rel) : MemRange.t option =
    let find_non_empty =
      List.find_map (
        fun (sub, sub_pc) ->
          let block_var_set = get_block_var sub in
          if SingleEntryType.SingleVarSet.is_empty block_var_set then None
          else Some (sub, sub_pc, block_var_set)
      ) tv_rel.subtype_list
    in
    match find_non_empty with
    | None -> None
    | Some (sub_exp, sub_pc, block_var_set) ->
      let block_var_list = SingleEntryType.SingleVarSet.to_list block_var_set in
      let _, sup_pc = tv_rel.var_idx in
      let map_info_list =
        List.map (find_type_pair sup_pc sub_pc) block_var_list
      in *)

  let solve_one_var
      (range_is_val: MemRange.t -> bool)
      (new_sol_list: (var_idx_t * MemRange.t) list) (tv_rel: type_rel) : 
      ((var_idx_t * MemRange.t) list) * type_rel =
    let rule_list = [
      try_solve_full;
      try_solve_empty range_is_val;
    ] in
    match tv_rel.sol with
    | Some _ -> new_sol_list, tv_rel
    | None ->
      let filter_tv_rel = filter_self_subtype tv_rel in
      begin match List.find_map (fun rule -> rule filter_tv_rel) rule_list with
      | Some sol -> 
        (tv_rel.var_idx, sol) :: new_sol_list,
        { tv_rel with sol = Some sol }
      | None -> new_sol_list, tv_rel
      end

  let repl_range_val_sol
      (smt_ctx: SmtEmitter.t)
      (range_is_val: MemRange.t -> bool)
      (range_sol: ((MemRange.range_var_id * int) * MemRange.t) list)
      (tv_rel_list: t) : t =
    let range_sol =
      List.filter (
        fun (_, r) -> range_is_val r
      ) range_sol
    in
    List.map (
      fun (x: type_rel) ->
        if x.sol = None then
          let subtype_list =
            List.map (fun (sub, pc) -> MemRange.repl_range_sol smt_ctx range_sol sub, pc) x.subtype_list
          in
          { x with subtype_list = subtype_list }
        else x
    ) tv_rel_list

  let solve
      (smt_ctx: SmtEmitter.t)
      (range_is_val: MemRange.t -> bool) 
      (tv_rel_list: t) (iter: int) : t =
    let rec helper (tv_rel_list: t) (iter: int) : t =
      if iter <= 0 then tv_rel_list
      else
        let new_sol, tv_rel_list =
          List.fold_left_map (solve_one_var range_is_val) [] tv_rel_list
        in
        if List.length new_sol = 0 then tv_rel_list
        else
          let tv_rel_list = repl_range_val_sol smt_ctx range_is_val new_sol tv_rel_list in
          helper tv_rel_list (iter - 1)
    in
    helper tv_rel_list iter

  let repl_sol_arch_type
      (tv_rel_list: t)
      (a_type: ArchType.t) : ArchType.t =
    let helper (entry: MemOffset.t * MemRange.t * 'a) : MemOffset.t * MemRange.t * 'a =
      let off, range, e_type = entry in
      match range with
      | RangeConst _ -> entry
      | RangeVar v ->
        let sol_opt =
          List.find_map (
            fun (x: type_rel) ->
              let idx, _ = x.var_idx in
              if v = idx then x.sol
              else None
          ) tv_rel_list
        in
        begin match sol_opt with
        | Some sol -> off, sol, e_type
        | _ -> entry
        end
      | _ -> range_subtype_error (Printf.sprintf "We do not support repl sol for range exp %s" (MemRange.to_string range))
    in
    { a_type with mem_type = ArchType.MemType.map_full helper a_type.mem_type }

end

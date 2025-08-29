open Single_entry_type
open Reg_range
open Arch_type
open Isa_basic
(* open Single_subtype *)
open Set_sexp
open Sexplib.Std


module RegRangeSubtype = struct
  exception RegRangeSubtypeError of string

  let reg_range_subtype_error msg = raise (RegRangeSubtypeError ("[Reg Range Subtype Error] " ^ msg))

  type var_idx_t = RegRange.var_id * int
  [@@deriving sexp]

  type type_exp_t = RegRange.t * int
  [@@deriving sexp]

  type type_rel = {
    var_idx: var_idx_t;
    sol: RegRange.range_t option;
    size: int64;
    subtype_list: type_exp_t list;
  }
  [@@deriving sexp]

  type t = type_rel list
  [@@deriving sexp]

  module ArchType = ArchType (SingleEntryType)

  module IntSetMap = IntMapSexp (IntSet)

  (* let get_one_block_subtype
      (block_subtype: ArchType.block_subtype_t) : t =
    let sup_block, sub_block_list = block_subtype in
    let sup_valid_idx =
      List.mapi (
        fun i (valid, _) -> i, valid
      ) sup_block.reg_type
    in
    let sub_valid_list =
      List.fold_left (
        fun (acc: type_exp_t list list) (sub_block: ArchType.t) ->
          List.map2 (
            fun valid_list (sub_valid, _) -> (sub_valid, sub_block.pc) :: valid_list
          ) acc sub_block.reg_type
      ) (List.init IsaBasic.total_reg_num (fun _ -> [])) sub_block_list
    in
    List.map2 (
      fun (sub_list: type_exp_t list) (reg_idx, sup_valid) ->
        match sup_valid with
        | RegRange.RangeConst _ -> None
        | RangeExp _ -> reg_range_subtype_error "get_one_block_subtype fail when sup is exp"
        | RangeVar sup_idx ->
          Some {
            var_idx = (sup_idx, sup_block.pc);
            sol = None;
            size = IsaBasic.get_reg_idx_full_size reg_idx;
            subtype_list = sub_list;
          }
    ) sub_valid_list sup_valid_idx
    |> List.filter_map (fun x -> x)

  let get_var_sol_template (tv_rel_list: t) : IntSetMap.t =
    let init_one_sub_var (tv_rel: type_rel) : (int * (int list)) option =
      let sub_var_list =
        List.fold_left (
          fun (acc: IntSet.t option) (sub, _) ->
            match acc with
            | None -> None
            | Some acc_sub_var_set ->
              match sub with
              | RegRange.RangeConst (l, r) ->
                if l = 0L && r = tv_rel.size then acc
                else None
              | RangeVar v | RangeExp (v, _) -> 
                if v = (fst tv_rel.var_idx) then acc 
                else Some (IntSet.add v acc_sub_var_set)
        ) (Some IntSet.empty) tv_rel.subtype_list
      in
      Option.bind sub_var_list (fun e -> if IntSet.is_empty e then None else Some ((fst tv_rel.var_idx), IntSet.to_list e))
    in
    let get_subset_var_helper (subset_var_map: IntSetMap.t) (var_idx: int) : IntSet.t =
      Option.value (IntSetMap.find_opt var_idx subset_var_map) ~default:(IntSet.singleton var_idx)
    in
    let update_one_var (acc: bool * IntSetMap.t) (tv_sub_var: int * (int list)) : bool * IntSetMap.t =
      let _, acc_var_map = acc in
      let var, direct_sub_var_list = tv_sub_var in
      let new_subset_var =
        match direct_sub_var_list with
        | [] -> reg_range_subtype_error "unexpected case"
        | hd :: [] -> get_subset_var_helper acc_var_map hd
        | hd :: tl -> 
          List.fold_left IntSet.inter (get_subset_var_helper acc_var_map hd) (List.map (get_subset_var_helper acc_var_map) tl)
      in
      if IntSet.is_empty new_subset_var then acc
      else
        let old_subset_var = get_subset_var_helper acc_var_map var in
        if IntSet.subset new_subset_var old_subset_var then acc
        else 
          true, 
          IntSetMap.update var (fun _ -> Some (IntSet.union old_subset_var new_subset_var)) acc_var_map
    in
    let rec update_vars (acc_var_map: IntSetMap.t) (tv_sub_var_list: (int * (int list)) list) : IntSetMap.t =
      let acc_update, acc_var_map = List.fold_left update_one_var (false, acc_var_map) tv_sub_var_list in
      if acc_update then update_vars acc_var_map tv_sub_var_list
      else acc_var_map
    in
    let tv_sub_var_list = List.filter_map init_one_sub_var tv_rel_list in
    (* let subset_rel = List.map (fun (x, _) -> x, IntSet.singleton x) tv_sub_var_list |> IntSetMap.of_list in *)
    update_vars IntSetMap.empty tv_sub_var_list

  let solve_one (var_sol_template: IntSetMap.t) (tv_rel: type_rel) : RegRange.range_t option =
    if List.is_empty tv_rel.subtype_list then None else
    let var_idx = fst tv_rel.var_idx in
    let is_sol_template (sub_idx: int) : bool =
      (* check whether sub_idx can be a conservative sol to var_idx *)
      (* return true if var_idx in var_sol_template[sub_idx] *)
      match IntSetMap.find_opt sub_idx var_sol_template with
      | None -> false
      | Some var_set -> IntSet.mem var_idx var_set
    in
    let sub_list =
      List.filter (
        fun (sub, _) ->
          match sub with
          | RegRange.RangeVar v | RangeExp (v, _) ->
            if v = var_idx then false (* sol is not constrained by this subset *)
            else not (is_sol_template v)
          | RangeConst _ -> true
      ) tv_rel.subtype_list
    in
    let const_sub_list =
      List.filter_map (
        fun (sub, _) ->
          match sub with
          | RegRange.RangeConst r -> Some r
          | _ -> None
      ) sub_list
    in
    if List.length const_sub_list < List.length sub_list then None
    else Some (List.fold_left RegRange.inter_range (0L, tv_rel.size) const_sub_list)

  let solve
      (single_sol: SingleSubtype.t)
      (block_subtype_list: ArchType.block_subtype_t list) : RegRange.VarSolMap.t =
    let tv_rel_list = List.concat_map get_one_block_subtype block_subtype_list in
    let _ = SingleSubtype.get_loop_step_back_pc single_sol in
    Printf.printf "Reg Range Subtype\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t tv_rel_list));
    let var_sol_template = get_var_sol_template tv_rel_list in
    Printf.printf "Reg Range Var Sol Template\n%s\n" (Sexplib.Sexp.to_string_hum (IntSetMap.sexp_of_t var_sol_template));

    let rec helper (tv_rel_list: t) : t =
      let new_sol, tv_rel_list =
        List.fold_left_map (
          fun (acc: (RegRange.var_id * RegRange.range_t) list) (tv_rel: type_rel) ->
            if tv_rel.sol = None then
              match solve_one var_sol_template tv_rel with
              | None -> acc, tv_rel
              | Some sol -> (fst tv_rel.var_idx, sol) :: acc, { tv_rel with sol = Some sol }
            else acc, tv_rel
        ) [] tv_rel_list
      in
      if List.is_empty new_sol then tv_rel_list
      else 
        let sol_map = RegRange.VarSolMap.of_list new_sol in
        (* Printf.printf "RegrangeSubtype sol_map\n%s\n" (Sexplib.Sexp.to_string_hum (RegRange.VarSolMap.sexp_of_t sol_map)); *)
        List.map (
          fun (tv_rel: type_rel) ->
            if tv_rel.sol <> None then tv_rel else
            let sub_list =
              List.map (fun (x, pc) -> RegRange.sub_sol sol_map x, pc) tv_rel.subtype_list
            in
            { tv_rel with subtype_list = sub_list}
        ) tv_rel_list
        |> helper
    in
    let result = helper tv_rel_list in
    Printf.printf "RegRangeSubtype sol\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t result));

    let sol =
      List.filter_map (
        fun (tv_rel: type_rel) ->
          Option.map (fun x -> (fst tv_rel.var_idx), x) tv_rel.sol
      ) result
    in
    if List.length sol = List.length result then Printf.printf "Successfully infer all reg range\n";
    RegRange.VarSolMap.of_list sol *)

  let repl_sol_arch_type
      (sol: RegRange.VarSolMap.t) (a_type: ArchType.t) : ArchType.t =
    let reg_type =
      List.map (fun (valid, e) -> RegRange.sub_sol sol valid, e) a_type.reg_type
    in
    { a_type with reg_type = reg_type }

  type sub_rel_t = RegRange.var_id * (RegRange.t list)

  let get_one_block_subtype (block_subtype: ArchType.block_subtype_t) : sub_rel_t option list =
    let sup_block, sub_block_list = block_subtype in
    let sub_rel_list =
      List.map (
        fun (x, _) ->
          match x with
          | RegRange.RangeConst _ -> None
          | RangeVar v -> Some (v, [])
          | _ -> reg_range_subtype_error "unexpected case"
      ) sup_block.reg_type
    in
    List.fold_left (
      fun (acc: (sub_rel_t option) list) (sub_block: ArchType.t) ->
        List.map2 (
          fun (sub_rel_opt: sub_rel_t option) (x, _) ->
            Option.map (fun (v, sub_list) -> v, x :: sub_list) sub_rel_opt
        ) acc sub_block.reg_type
    ) sub_rel_list sub_block_list

  let get_all_block_subtype (block_subtype_list: ArchType.block_subtype_t list) : sub_rel_t list list =
    let result = List.init IsaBasic.total_reg_num (fun _ -> []) in
    List.fold_left (
      fun (acc: sub_rel_t list list) (block_subtype: ArchType.block_subtype_t) ->
        let sub_rel_list = get_one_block_subtype block_subtype in
        List.map2 (
          fun (rel_list: sub_rel_t list) (rel: sub_rel_t option) ->
            Option.fold ~none:(rel_list) ~some:(fun x -> x :: rel_list) rel
        ) acc sub_rel_list
    ) result block_subtype_list
  
  let convert_sub_rel (sub_rel: sub_rel_t) : int * RegRange.VarSolMap.t =
    (* Goal: sup_idx has sub_list: RangeExp(x, r1), RangeExp(x, r2), RangeConst(r2)
        Then, return (sup_idx, { x: r1 /\ r2, 0: r2 })
    *)
    let sub_var_map_helper (new_range: RegRange.range_t) (orig_range_opt: RegRange.range_t option) =
      match orig_range_opt with
      | None -> Some new_range
      | Some orig_range -> Some (RegRange.inter_range orig_range new_range)
    in
    let const_var_key = 0 in (* we use 0 as the key for subtypes that are RangeConst *)
    let sup_idx, sub_list = sub_rel in
    sup_idx,
    List.fold_left (
      fun (acc: RegRange.VarSolMap.t) (sub: RegRange.t) ->
        match sub with
        | RangeConst r -> IntMap.update const_var_key (sub_var_map_helper r) acc
        | RangeVar v -> 
          if v = sup_idx then acc
          else IntMap.update v (sub_var_map_helper (0L, 0L)) acc
        | RangeExp (v, r) -> 
          if v = sup_idx then acc
          else IntMap.update v (sub_var_map_helper r) acc
    ) IntMap.empty sub_list

  let sub_rel_update (x_rel: int * RegRange.VarSolMap.t) (y_rel: int * RegRange.VarSolMap.t) : int * RegRange.VarSolMap.t =
    let x, x_sub = x_rel in
    let y, y_sub = y_rel in
    match RegRange.VarSolMap.find_opt y x_sub with
    | None -> x_rel
    | Some y_union_slot ->
      let y_union_y_sub = RegRange.VarSolMap.map (RegRange.union_range y_union_slot) y_sub in
      x,
      RegRange.VarSolMap.merge (
        fun sub_idx r1 r2 ->
          if sub_idx = x then None
          else begin
            match r1, r2 with
            | Some r1, Some r2 -> Some (RegRange.inter_range r1 r2)
            | Some r, None | None, Some r -> Some r
            | None, None -> None
          end
      ) (RegRange.VarSolMap.remove y x_sub) (y_union_y_sub)

  let solve_sub_rel (reg_idx: int) (sub_rel_list: sub_rel_t list) : (int * RegRange.range_t) list =
    let sub_rel_list, unused_var_list =
      List.partition_map (
        fun (x, sub) ->
          if List.is_empty sub then Right x
          else Left (convert_sub_rel (x, sub))
      ) sub_rel_list
    in
    let sub_rel_list =
      List.fold_left (
        fun (acc: (int * RegRange.VarSolMap.t) list) (sub_rel: int * RegRange.VarSolMap.t) ->
          let sub_rel = List.fold_left sub_rel_update sub_rel acc in
          let acc = List.map (fun x -> sub_rel_update x sub_rel) acc in
          sub_rel :: acc
      ) [] sub_rel_list
    in
    let sol =
      List.map (
        fun (x, sub_map) ->
          let sub_list = RegRange.VarSolMap.to_list sub_map in
          match sub_list with
          | [] -> x, (0L, IsaBasic.get_reg_idx_full_size reg_idx)
          | (0, r) :: [] -> x, r
          | _ -> reg_range_subtype_error "unexpected case"
      ) sub_rel_list
    in
    let unused_sol = List.map (fun x -> x, (0L, 0L)) unused_var_list in
    sol @ unused_sol
    (* let sol = RegRange.VarSolMap.of_list (sol @ unused_sol) in
    Printf.printf "Sol of reg %d\n%s\n" reg_idx (Sexplib.Sexp.to_string_hum (RegRange.VarSolMap.sexp_of_t sol));
    sol *)

  let solve (block_subtype_list: ArchType.block_subtype_t list) : RegRange.VarSolMap.t =
    let all_reg_sub_rel = get_all_block_subtype block_subtype_list in
    let sol_list = List.mapi solve_sub_rel all_reg_sub_rel |> List.concat in
    let sol = RegRange.VarSolMap.of_list sol_list in
    Printf.printf "RegRange Sol\n%s\n" (Sexplib.Sexp.to_string_hum (RegRange.VarSolMap.sexp_of_t sol));
    sol

end

open Single_entry_type
open Reg_range
open Arch_type
open Isa_basic
(* open Single_subtype *)
open Reg_range_useless_infer
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

  let repl_sol_arch_type
      (sol: RegRange.VarSolMap.t) 
      (a_type: ArchType.t) : ArchType.t =
    let reg_type = List.map (fun (valid, e) -> RegRange.sub_sol sol valid, e) a_type.reg_type in
    (* let new_change_v_opt = [] in
    let new_change_v =
      List.filter_map (fun x -> x) new_change_v_opt
      |> IntSet.of_list
      |> IntSet.union a_type.change_var
    in *)
    { a_type with 
      reg_type = reg_type;
      (* change_var = IntSet.union a_type.change_var  *)
    }

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

  let get_callee_at_ret_var (block_subtype_list: ArchType.block_subtype_t list) : IntSet.t =
    let _, sub_list = List.nth block_subtype_list ((List.length block_subtype_list) - 1) in
    List.concat_map (
      fun (a_type: ArchType.t) ->
        List.mapi (
          fun i (valid, _) ->
            if IsaBasic.is_reg_idx_callee_saved i then
              match valid with
              | RegRange.RangeVar v | RangeExp (v, _) -> Some v
              | _ -> None
            else None
        ) a_type.reg_type
        |> List.filter_map (fun x -> x)
    ) sub_list
    |> IntSet.of_list

  let solve (block_subtype_list: ArchType.block_subtype_t list) : RegRange.VarSolMap.t=
    let callee_at_ret_var = get_callee_at_ret_var block_subtype_list in
    let useful_var = RegRangeUselessInfer.get_useful_range_vars false callee_at_ret_var block_subtype_list in
    (* let callee_at_call_var = RegRangeUselessInfer.get_useful_range_vars true callee_at_ret_var block_subtype_list in *)
    let all_reg_sub_rel = get_all_block_subtype block_subtype_list in
    let sol_list = List.mapi solve_sub_rel all_reg_sub_rel |> List.concat in
    let sol = RegRange.VarSolMap.of_list sol_list in
    let sol = RegRange.VarSolMap.filter_map (
      fun v r ->
        if IntSet.mem v useful_var then Some r
        else Some (0L, 0L)
    ) sol
    in
    Printf.printf "RegRange Sol\n%s\n" (Sexplib.Sexp.to_string_hum (RegRange.VarSolMap.sexp_of_t sol));
    sol

end

open Mem_offset_new
open Range_subtype
open Set_sexp
open Sexplib.Std

module RangeSubset = struct
  exception RangeSubsetError of string

  let range_subset_error msg = raise (RangeSubsetError ("[Range Subset Error] " ^ msg))

  type sub_sup_t = {
    var_idx: MemRange.range_var_id;
    supset_list: IntSet.t;
    subset_list: IntSet.t;
  }
  [@@deriving sexp]

  type t = sub_sup_t list
  [@@deriving sexp]


  let add_one_sub_sup (sub_sup_list: t) (supset_list: IntSet.t) (subset_list: IntSet.t) : t =
    (* Printf.printf "add_sub_sub_sup_sup subset %s supset %s\n" (Sexplib.Sexp.to_string (IntSet.sexp_of_t subset_list)) (Sexplib.Sexp.to_string (IntSet.sexp_of_t supset_list)); *)
    List.map (
      fun (entry: sub_sup_t) ->
        let new_supset_list =
          if IntSet.mem entry.var_idx subset_list then
            IntSet.union supset_list entry.supset_list
          else entry.supset_list
        in
        let new_subset_list = 
          if IntSet.mem entry.var_idx supset_list then
            IntSet.union subset_list entry.subset_list
          else entry.subset_list
        in
        { entry with supset_list = new_supset_list; subset_list = new_subset_list }
    ) sub_sup_list

  let add_sub_sub_sup (sub_sup_list: t) (supset_list: IntSet.t) (subset_list: IntSet.t) : t =
    let supset_list =
      List.fold_left (
        fun (acc: IntSet.t) (entry: sub_sup_t) ->
          if IntSet.mem entry.var_idx supset_list then
            IntSet.union entry.supset_list acc
          else acc
      ) supset_list sub_sup_list
    in
    add_one_sub_sup sub_sup_list supset_list subset_list

  let add_sub_sub_sup_sup (sub_sup_list: t) (supset_list: IntSet.t) (var_idx: MemRange.range_var_id) : t =
    (* Printf.printf "add_sub_sub_sup_sup var %d supset %s\n" var_idx (Sexplib.Sexp.to_string (IntSet.sexp_of_t supset_list)); *)
    let subset_list =
      List.find_map (
        fun (entry: sub_sup_t) ->
          if entry.var_idx = var_idx then Some (IntSet.add var_idx entry.subset_list)
          else None
      ) sub_sup_list |> Option.get
    in
    add_sub_sub_sup sub_sup_list supset_list subset_list

  let update_subtype_list_equal_set (subtype_list: RangeSubtype.t) : RangeSubtype.t =
    let sub_sup_list =
      List.map (
        fun (x: RangeSubtype.type_rel) ->
          { var_idx = fst x.var_idx; supset_list = IntSet.empty; subset_list = IntSet.empty }
      ) subtype_list
    in
    let sub_sup_list = 
      List.fold_left (
        fun (acc: t) (entry: RangeSubtype.type_rel) ->
          let sub_var_list =
            List.filter_map (
              fun (sub_pc: RangeSubtype.type_exp_t) ->
                match fst sub_pc with
                | RangeVar v -> Some v
                | _ -> None
            ) entry.subtype_list |> IntSet.of_list
          in
          add_sub_sub_sup_sup acc sub_var_list (fst entry.var_idx)
      ) sub_sup_list subtype_list
    in
    Printf.printf "Range subset\n%s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t sub_sup_list));
    List.map2 (
      fun (tv_rel: RangeSubtype.type_rel) (entry: sub_sup_t) ->
        (* RangeSubtype.filter_self_subtype  *)
        { tv_rel with equal_var_set = IntSet.inter entry.supset_list entry.subset_list }
    ) subtype_list sub_sup_list

  (* let is_sub_also_sup (sub_sup_list: t) (subset_var: MemRange.range_var_id) (supset_var: MemRange.range_var_id) : bool =
    let entry = List.find (fun (entry: sub_sup_t) -> entry.var_idx = subset_var) sub_sup_list in
    IntSet.mem supset_var entry.subset_list *)

end
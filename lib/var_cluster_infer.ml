open Set_sexp
open Sexplib.Std
open Sexplib

module type ConstValType = sig
  type t
  val compare : t -> t -> int
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end


module VarClusterInfer (ConstVal: ConstValType) = struct
  exception VarClusterInfererror of string

  let var_cluster_infer_error msg = raise (VarClusterInfererror ("[Var Cluster Infer Error] " ^ msg))

  type idx_t = int
  [@@deriving sexp]

  type val_t = ConstVal.t
  [@@deriving sexp]

  module VarSet = IntSet

  type t = {
    const_val_cluster: (val_t * VarSet.t) list;
    other_cluster: VarSet.t list;
  }
  [@@deriving sexp]

  let get_const_val (cluster: t) (x: idx_t) : val_t option =
    List.find_map (
      fun (const_val, cluster) ->
        if VarSet.mem x cluster then Some const_val
        else None
    ) cluster.const_val_cluster

  let get_other_cluster_id (cluster: t) (x: idx_t) : int option =
    List.find_index (
      fun cluster -> VarSet.mem x cluster
    ) cluster.other_cluster

  let get_other_cluster_min_elt (cluster: t) (x: idx_t) : idx_t option =
    List.find_map (
      fun cluster ->
        if VarSet.mem x cluster then Some (VarSet.min_elt cluster)
        else None
    ) cluster.other_cluster

  let remove_other_cluster_var (cluster: t) (x: idx_t) : t * VarSet.t =
    let match_list, remain_list =
      List.partition (
        fun var_set -> VarSet.mem x var_set
      ) cluster.other_cluster
    in
    match match_list with
    | [] -> { cluster with other_cluster = remain_list }, VarSet.singleton x
    | hd :: [] -> { cluster with other_cluster = remain_list }, hd
    | _ -> var_cluster_infer_error "find multiple match"

  let set_const_val (cluster: t) (x: idx_t) (const_val: val_t) : t =
    (* 1. Check whether x is a const val *)
    match get_const_val cluster x with
    | Some c ->
      if ConstVal.compare c const_val = 0 then cluster
      else var_cluster_infer_error "cannot make two different const equal"
    | None ->
      (* 2. Get x's cluster (if any) from the other list and remove it from other list *)
      let cluster, x_cluster = remove_other_cluster_var cluster x in
      let find_val, new_const_val_cluster = 
        List.fold_left_map (
          fun (acc: bool) (v, v_cluster) ->
            if acc then acc, (v, v_cluster)
            else if ConstVal.compare const_val v = 0 then true, (v, VarSet.union x_cluster v_cluster)
            else false, (v, v_cluster)
        ) false cluster.const_val_cluster
      in
      let new_const_val_cluster =
        if find_val then new_const_val_cluster
        else (const_val, x_cluster) :: new_const_val_cluster
      in
      (* 3. Merge x's cluster with corresponding const cluster *)
      { cluster with const_val_cluster = new_const_val_cluster }

  let update_equal_var (cluster: t) (x1: idx_t) (x2: idx_t) : t =
    if x1 = x2 then cluster
    else
      match get_const_val cluster x1, get_const_val cluster x2 with
      | Some c1, Some c2 ->
        if ConstVal.compare c1 c2 = 0 then cluster
        else var_cluster_infer_error "cannot make two different const equal"
      | Some c, None -> set_const_val cluster x2 c
      | None, Some c -> set_const_val cluster x1 c
      | None, None ->
        let remove_x1_cluster, x1_cluster = remove_other_cluster_var cluster x1 in
        if VarSet.mem x2 x1_cluster then cluster
        else
          let remove_x2_cluster, x2_cluster = remove_other_cluster_var remove_x1_cluster x2 in
          { remove_x2_cluster with other_cluster = (VarSet.union x1_cluster x2_cluster) :: remove_x2_cluster.other_cluster }

end

module MyBool = struct
include Bool
  
  let sexp_of_t = sexp_of_bool
  let t_of_sexp = bool_of_sexp

end

module BoolVarClusterInfer = VarClusterInfer (MyBool)

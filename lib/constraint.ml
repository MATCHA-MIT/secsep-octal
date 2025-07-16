open Mem_offset_new
open Single_context
open Taint_exp
open Set_sexp
open Smt_emitter
open Sexplib.Std

module Constraint = struct
  exception ConstraintError of string
  let constraint_error msg = raise (ConstraintError ("[Constraint Error] " ^ msg))

  type t =
    | Unknown of MemOffset.t
    (* | Subset of MemOffset.t * MemRange.t * MemOffset.t *)
    | RangeSubset of MemRange.range_var_id * (MemOffset.t list) (* var, off_list: off_list is subset of var *)
    | RangeUnsat of MemOffset.t * (MemOffset.t list)
    | RangeEq of MemOffset.t * MemRange.t
    | RangeOverwritten of MemRange.t
    | RangeMustKnown of MemRange.t
    | TaintSub of TaintExp.t * TaintExp.t (* (x, y) where x => y *)
    | TaintOverwritten of TaintExp.t
    | TaintMustKnown of TaintExp.t
    | CalleeContext of (SingleContext.t list)
    | CalleeUnknownContext
  [@@deriving sexp]

  let string_of_t (c: t) : string = c |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let get_unknown (constraint_list: (t * int) list) : (MemOffset.t * int) list =
    List.filter_map (
      fun (x, pc) ->
        match x with
        | Unknown off -> Some (off, pc)
        | _ -> None
    ) constraint_list

  let get_taint_sub (constraint_list: (t * int) list) : (TaintExp.t * TaintExp.t) list =
    List.filter_map (
      fun (x, _) ->
        match x with
        | TaintSub (l, r) -> Some (l, r)
        | _ -> None
    ) constraint_list

  let get_taint_overwritten (constraint_list: (t * int) list) : TaintExp.TaintVarSet.t =
    List.fold_left (
      fun (acc: TaintExp.TaintVarSet.t) (x, _) ->
        match x with
        | TaintOverwritten (TaintVar v) -> TaintExp.TaintVarSet.add v acc
        | _ -> acc
    ) TaintExp.TaintVarSet.empty constraint_list
  
  let get_taint_must_known (constraint_list: (t * int) list) : TaintExp.TaintVarSet.t * (int list) =
    List.fold_left (
      fun (acc: TaintExp.TaintVarSet.t * (int list)) (x, pc) ->
        let must_known_var_set, invalid_pc_list = acc in
        match x with
        | TaintMustKnown (TaintUnknown) -> must_known_var_set, pc :: invalid_pc_list
        | TaintMustKnown x -> 
          TaintExp.TaintVarSet.union must_known_var_set (TaintExp.get_var_set x), 
          invalid_pc_list
        | _ -> acc
    ) (TaintExp.TaintVarSet.empty, []) constraint_list
  
  let get_range_must_known (constraint_list: (t * int) list) : IntSet.t =
    let add_range_var_helper (r: MemRange.t) (s: IntSet.t) : IntSet.t =
      match MemRange.get_range_var r with
      | Some v -> IntSet.add v s
      | None -> s
    in
    List.fold_left (
      fun (acc: IntSet.t) (c, _) ->
        match c with
        | RangeMustKnown r -> add_range_var_helper r acc
        | _ -> acc
    ) IntSet.empty constraint_list

  let get_range_can_be_empty (constraint_list: (t * int) list) : IntSet.t =
    let add_range_var_helper (r: MemRange.t) (s: IntSet.t) : IntSet.t =
      match MemRange.get_range_var r with
      | Some v -> IntSet.add v s
      | None -> s
    in
    let overwritten_var_set, must_known_var_set =
      List.fold_left (
        fun (acc: IntSet.t * IntSet.t) (x, _) ->
          let overwritten_var_set, must_known_var_set = acc in
          match x with
          | RangeOverwritten r -> add_range_var_helper r overwritten_var_set, must_known_var_set
          | RangeMustKnown r -> overwritten_var_set, add_range_var_helper r must_known_var_set
          | _ -> acc
      ) (IntSet.empty, IntSet.empty) constraint_list
    in
    IntSet.diff overwritten_var_set must_known_var_set

  let gen_off_subset (smt_ctx: SmtEmitter.t) (sub_off: MemOffset.t) (range: MemRange.t) (off: MemOffset.t) : t list =
    (* check whether sub_off (solution) is the subset of range (subtype) *)
    if MemOffset.is_empty smt_ctx sub_off then [] else
    match range with
    | RangeConst ro_list ->
      begin match MemOffset.diff smt_ctx sub_off ro_list with
      | [] -> []
      | remain_list -> [ RangeUnsat (off, remain_list) ]
      end
    | RangeVar v -> [ RangeSubset (v, [ sub_off ]) ]
    | RangeExp (v, ro_list) -> 
      begin match MemOffset.diff smt_ctx sub_off ro_list with
      | [] -> []
      | remain_list -> [ RangeSubset (v, remain_list) ]
      end

  let gen_range_subset (smt_ctx: SmtEmitter.t) (sub_range: MemRange.t) (range: MemRange.t) (off: MemOffset.t) : t list =
    match sub_range with
    | RangeConst sub_o_list -> List.concat_map (fun x -> gen_off_subset smt_ctx x range off) sub_o_list
    | _ -> constraint_error (Printf.sprintf "Cannot gen range constraint for %s" (MemRange.to_string sub_range))

  let get_range_subset (constriant_list: (t * int) list) : MemOffsetSet.t IntMap.t =
    List.fold_left (
      fun (acc: MemOffsetSet.t IntMap.t) (cons, _) ->
        match cons with
        | RangeSubset (v, o_list) ->
          begin match IntMap.find_opt v acc with
          | None -> IntMap.add v (MemOffsetSet.of_list o_list) acc
          | Some o_set -> 
            acc |>
            IntMap.update v (Option.map (fun _ -> MemOffsetSet.union o_set (MemOffsetSet.of_list o_list)))
          end
        | _ -> acc
    ) IntMap.empty constriant_list

  let get_callee_context (constraint_list: (t * int) list) : ((SingleContext.t list) * int) list =
    List.filter_map (
      fun (x, pc) ->
        match x with
        | CalleeContext context -> Some (context, pc)
        | _ -> None
    ) constraint_list
    (* List.concat (
      List.filter_map (
        fun (x, _) ->
          match x with
          | CalleeContext context ->
            Some context
          | _ -> None
      ) constraint_list
    ) *)

  let has_callee_unknown_context (constraint_list: (t * int) list) : bool =
    List.find_opt (
      fun (x, _) -> x = CalleeUnknownContext
    ) constraint_list <> None

end

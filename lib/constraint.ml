open Mem_offset_new
open Single_context
open Taint_exp
open Sexplib.Std

module Constraint = struct
  exception ConstraintError of string
  let constraint_error msg = raise (ConstraintError ("[Constraint Error] " ^ msg))

  type t =
    | Unknown of MemOffset.t
    | Subset of MemOffset.t * MemRange.t * MemOffset.t
    | RangeEq of MemOffset.t * MemRange.t
    | RangeOverwritten of MemRange.t
    | TaintSub of TaintExp.t * TaintExp.t (* (x, y) where x => y *)
    | TaintOverwritten of TaintExp.t
    | TaintMustKnown of TaintExp.t
    | CalleeContext of (SingleContext.t list)
    | CalleeUnknownContext
  [@@deriving sexp]

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

  let gen_range_subset (sub_range: MemRange.t) (range: MemRange.t) (off: MemOffset.t) : t list =
    match sub_range with
    | RangeConst o -> List.map (fun x -> Subset (x, range, off)) o
    | _ -> constraint_error (Printf.sprintf "Cannot gen range constraint for %s" (MemRange.to_string sub_range))

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

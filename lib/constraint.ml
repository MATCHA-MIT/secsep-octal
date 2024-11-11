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
    | TaintSub of TaintExp.t * TaintExp.t (* (x, y) where x => y *)
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

  let gen_range_subset (sub_range: MemRange.t) (range: MemRange.t) (off: MemOffset.t) : t list =
    match sub_range with
    | RangeConst o -> List.map (fun x -> Subset (x, range, off)) o
    | _ -> constraint_error (Printf.sprintf "Cannot gen range constraint for %s" (MemRange.to_string sub_range))

  let get_callee_context (constraint_list: (t * int) list) : SingleContext.t list =
    List.concat (
      List.filter_map (
        fun (x, _) ->
          match x with
          | CalleeContext context ->
            Some context
          | _ -> None
      ) constraint_list
    )

  let has_callee_unknown_context (constraint_list: (t * int) list) : bool =
    List.find_opt (
      fun (x, _) -> x = CalleeUnknownContext
    ) constraint_list <> None

end

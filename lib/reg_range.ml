open Isa_basic
open Constraint
open Set_sexp
open Sexplib.Std
(* open Sexplib *)

module RegRange = struct
  exception RegRangeError of string
  let reg_range_error msg = raise (RegRangeError ("[Reg Range Error] " ^ msg))

  type var_id = int
  [@@deriving sexp]

  type range_t = int64 * int64
  [@@deriving sexp]

  type t =
    | RangeConst of range_t
    | RangeVar of var_id
    | RangeExp of var_id * range_t
  [@@deriving sexp]

  module VarSolMap = IntMapSexp (
    struct
      type t = range_t
      [@@deriving sexp]
    end
  )

  let get_empty_range () : t = RangeConst (0L, 0L)

  let is_empty_range (range: t) : bool =
    match range with
    | RangeConst (0L, 0L) -> true
    | _ -> false

  let is_full_range (range: t) (full_size: int64) : bool =
    match range with
    | RangeVar _ -> false
    | RangeConst (l, r) | RangeExp (_, (l, r)) ->
      l == 0L && r == full_size

  let get_input_range (num_args: int) (reg_idx: int) : t =
    let arg_idx =
      match reg_idx with
      | 7 -> 1
      | 6 -> 2
      | 2 -> 3
      | 1 -> 4
      | 8 -> 5
      | 9 -> 6
      | 3 | 4 | 5 | 12 | 13 | 14 | 15 -> 0 (* callee-saved registers *)
      | _ -> num_args + 1 (* non-arg, caller saved registers *)
    in
    if arg_idx <= num_args then 
      (* I hardcode 8 here since xmm are caller saved reg and are not args *)
      RangeConst (0L, 8L)
    else RangeConst (0L, 0L)

  let get_ret_range (reg_idx: int) : t =
    (* TODO: for now, I am marking rax as invalid, 
      but I should decide wehther it is valid or not 
      depending on the function's return type! *)
    if IsaBasic.is_reg_idx_callee_saved reg_idx then RangeConst (0L, 8L)
    else RangeConst (0L, 0L)

  let union_range (r: range_t) (off: range_t) : range_t =
    let r1, r2 = r in
    let o1, o2 = off in
    if r2 < o1 || o2 < r1 then reg_range_error "cannot merge non-connected range";
    Int64.min r1 o1, Int64.max r2 o2

  let write_update (r: t) (off: range_t) (is_full: bool) : t =
    if is_full then RangeConst off else
    match r with
    | RangeConst r -> RangeConst (union_range r off)
    | RangeVar v -> RangeExp (v, off)
    | RangeExp (v, r) -> RangeExp (v, union_range r off)

  let read_constraint (r: t) (off: range_t) : Constraint.t list =
    match r with
    | RangeConst _ -> []
    | RangeVar v -> [ RegRangeMustKnown v ]
    | RangeExp (v, r) ->
      let r1, r2 = r in
      let o1, o2 = off in
      if r1 <= o1 && o2 <= r2 then []
      else [ RegRangeMustKnown v ]

  let inter_range (a: range_t) (b: range_t) : range_t =
    let a1, a2 = a in
    let b1, b2 = b in
    if a2 <= b1 || b2 <= a1 then 0L, 0L
    else Int64.max a1 b1, Int64.min a2 b2

  let sub_sol (sol_map: VarSolMap.t) (r: t) : t =
    match r with
    | RangeConst _ -> r
    | RangeVar v -> 
      Option.value (
        Option.map (fun x -> RangeConst x) (VarSolMap.find_opt v sol_map)
      ) ~default:(r)
    | RangeExp (v, e) -> 
      Option.value (
        Option.map (fun x -> RangeConst (union_range e x)) (VarSolMap.find_opt v sol_map)
      ) ~default:(r)

end

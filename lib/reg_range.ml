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

  let get_empty_range () : t = RangeConst (0L, 0L)

  let write_update_range (r: range_t) (off: range_t) : range_t =
    let r1, r2 = r in
    let o1, o2 = off in
    let x1 = Int64.min r1 o1 in
    let x2 = Int64.min r2 o2 in
    if x1 >= x2 then 0L, 0L
    else x1, x2

  let write_update (r: t) (off: range_t) : t =
    match r with
    | RangeConst r -> RangeConst (write_update_range r off)
    | RangeVar v -> RangeExp (v, off)
    | RangeExp (v, r) -> RangeExp (v, write_update_range r off)

end

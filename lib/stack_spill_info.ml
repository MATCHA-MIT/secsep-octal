open Isa_basic
open Mem_offset_new
open Set_sexp
open Sexplib.Std

module StackSpillInfo = struct
  exception StackSpillInfoError of string

  let stack_spill_info_error msg = raise (StackSpillInfoError ("[Stack Spill Info error] " ^ msg))

  type entry_t = int * int
  [@@deriving sexp]

  type t = Int64PairSet.t
  [@@deriving sexp]

  let empty () : t = Int64PairSet.empty

  let init (stack_spill_layout: (MemOffset.t * MemRange.t * bool) list) : t =
    List.filter_map (
      fun (mem_slot: MemOffset.t * MemRange.t * bool) ->
        let (l, r), _, is_spill = mem_slot in
        if is_spill then
          match l, r with
          | SingleConst l, SingleConst r -> Some (l, r)
          | _ -> stack_spill_info_error (Printf.sprintf "stack slot off %s is not const" (MemOffset.to_string (l, r)))
        else None
    ) stack_spill_layout |> Int64PairSet.of_list

  let to_off_pair (ptr: IsaBasic.imm_var_id) (off: MemOffset.t) : int64 * int64 =
    let l, r = MemOffset.add_base (SingleBExp (SingleMul, SingleVar ptr, SingleConst (-1L))) off in
    match l, r with
    | SingleConst l, SingleConst r -> l, r
    | _ -> stack_spill_info_error (Printf.sprintf "stack slot off %s is not const" (MemOffset.to_string (l, r)))

  let is_spill (spill_info: t) (ptr: IsaBasic.imm_var_id) (off: MemOffset.t) : bool =
    if ptr = IsaBasic.rsp_idx then
      let off_pair = to_off_pair ptr off in
      Int64PairSet.mem off_pair spill_info
    else false

end
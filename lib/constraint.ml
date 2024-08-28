open Mem_offset

module Constraint = struct
  exception ConstraintError of string
  let constraint_error msg = raise (ConstraintError ("[Constraint Error] " ^ msg))

  type t =
    | Unknown of MemOffset.t
    | Subset of MemOffset.t * MemRange.t * MemOffset.t
    (* TODO: Add taint constraint later *)

  let get_unknown (constraint_list: (t * int) list) : (MemOffset.t * int) list =
    List.filter_map (
      fun (x, pc) ->
        match x with
        | Unknown off -> Some (off, pc)
        | _ -> None
    ) constraint_list

end

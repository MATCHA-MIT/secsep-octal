open Mem_offset

module Constraint = struct
  exception ConstraintError of string
  let constraint_error msg = raise (ConstraintError ("[Constraint Error] " ^ msg))

  type t =
    | Unknown of MemOffset.t
    | Subset of MemOffset.t * MemRange.t * MemOffset.t
    (* TODO: Add taint constraint later *)

end

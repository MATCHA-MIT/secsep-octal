open Isa
open Type_exp
open Mem_type
open State_type
open Cond_type
open Subtype

module ProgType = struct
  exception ProgTypeError of string
  let prog_type_error msg = raise (ProgTypeError ("[Prog Type Error] " ^ msg))

  type block_type = {
    lable: Isa.label;
    block_type: StateType.t;
  }

  type t = {
    prog: Isa.program;
    prog_type: block_type list;
    cond_type: CondType.t list;
    subtype_sol: SubType.t;
    ptr_set: MemType.MemKeySet.t;
    no_ptr_set: MemType.MemKeySet.t;
    next_single_var_idx: Isa.imm_var_id;
    next_type_var_idx: TypeExp.type_var_id;
  }

  (* Init / update prog type with proper type var or sym var *)

  (* Update structure and solution (if any) of subtype_sol *)

  (* Type propagation -> generate subtype relation and unknown mem access list *)

  (* Solve subtype relation to generate new relation *)

  (* Try to resolve known mem access with new solution *)

  (* Generate new memory layout (ptr-offset list) *)

end

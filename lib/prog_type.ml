open Isa
open State_type

module ProgType = struct
  exception ProgTypeError of string
  let prog_type_error msg = raise (ProgTypeError ("[Prog Type Error] " ^ msg))

  type block_type = {
    lable: Isa.label;
    block_type: StateType.t;
  }

  type t = block_type list


end

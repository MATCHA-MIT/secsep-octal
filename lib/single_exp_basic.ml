open Isa_basic

module SingleExpBasic = struct
  exception SingleExpError of string
  let single_exp_error msg = raise (SingleExpError ("[Single Exp Error] " ^ msg))

  module SingleVarSet = Set.Make(Int)

  type single_bop =
    | SingleAdd
    | SingleSub
    | SingleMul
    | SingleSal
    | SingleSar
    | SingleXor
    | SingleAnd
    | SingleOr

  type single_uop =
    | SingleNot

  type t =
    | SingleTop
    | SingleConst of int64
    | SingleVar of IsaBasic.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t
end

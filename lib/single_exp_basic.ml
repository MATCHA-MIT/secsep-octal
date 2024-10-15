open Isa_basic
open Set_sexp
open Sexplib.Std

module SingleExpBasic = struct
  exception SingleExpError of string
  let single_exp_error msg = raise (SingleExpError ("[Single Exp Error] " ^ msg))

  module SingleVarSet = IntSet

  type single_bop =
    | SingleAdd
    | SingleSub
    | SingleMul
    | SingleSal
    | SingleSar
    | SingleXor
    | SingleAnd
    | SingleOr
  [@@deriving sexp]

  type single_uop =
    | SingleNot
  [@@deriving sexp]

  type t =
    | SingleTop
    | SingleConst of int64
    | SingleVar of IsaBasic.imm_var_id
    | SingleBExp of single_bop * t * t
    | SingleUExp of single_uop * t
  [@@deriving sexp]

  let hh (x: t) = sexp_of_t x 
end

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
    | SingleMod
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

  type single_var_type_t =
    | VarPtr
    | VarLen
    | VarOther
  [@@deriving sexp]
  
  type var_type_map_t = (IsaBasic.imm_var_id * single_var_type_t) list
  [@@deriving sexp]

  let get_var_type
      (var_type_map: var_type_map_t)
      (var: IsaBasic.imm_var_id) : single_var_type_t =
    match List.find_opt (fun (v, _) -> v = var) var_type_map with
    | Some (_, t) -> t
    | _ -> VarOther

  let set_var_type 
      (var_type_map: var_type_map_t)
      (var: IsaBasic.imm_var_id)
      (var_type: single_var_type_t) :
      var_type_map_t =
    match get_var_type var_type_map var, var_type with
    | VarOther, _ -> (var, var_type) :: var_type_map
    | VarPtr, VarLen
    | VarLen, VarPtr -> single_exp_error "set_var_type conflict"
    | _ -> var_type_map  

end
